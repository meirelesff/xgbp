#' Extreme Gradient Boosting with Poststratification
#'
#' The package's main function, `xgbp` poststratify a survey response from a sample
#' using Extreme Gradient Boosting (XGB). Dependent variables can be both binomial
#' or multinomial and resulting estimates can be aggregated by the full sample or
#' by any group used in the estimation.
#'
#' @param survey A survey sample containing the variables to use in the poststratification.
#' Must be a `data.frame` or a `tibble`
#' @param census Census data to use in the poststratification. Must be a `data.frame` or
#' `tibble` containing the same variables, with the same categories, as the `survey` object
#' @param census_count `numeric` variable in the `census` object indicanting the raw number
#' or proportion of individuals in a given stratum
#' @param ... Individual and group level covariates used in the poststratification. All
#' variables must be included in the `survey` and in the `census` and passed unquoted to
#' the function call
#' @param dep_var Dependent variable. Must be `character` or `factor`
#' @param seed A seed for replication. Defaults to `NULL`
#' @param tune Should the XGBP tune the parameters with randomized grid search? Defaults to `TRUE`, in which
#' case `params` argument is ignored
#' @param params A list of parameters to be passed to xgboost function
#' @param nrounds Number of trees (rounds) used in to train the model. Defaults to `100`
#' @param n_iter When `tune = TRUE`, this indicates how many samples to draw
#' during gridsearch to use. Defaults to `30`.
#' @param nthread Number of htreads used in the computation. Defaults to `1`, but users are
#' encourage to increase this number to speed up computations (the limit is the actual number
#' of threads available at your computer)
#' @param verbose Should the function report messages along the estimation? Defaults to `TRUE`
#'
#' @return A list of class `xgbp` with the following items
#' * `estimates` -- A `tibble` containing raw estimates by strata
#' * `model` -- The trained `xgboost` model
#' * `data` -- GBP datamatrix used to train the model
#' * `nrounds` -- Number of rounds used to train the model
#' * `census` -- Census data used to poststratify results
#' * `census_count` -- Variable in the `census` object indicanting the raw number
#' or proportion of individuals in a given stratum
#' * `covars_matrix` -- GBP matrix with covars used to train the model
#' * `dep_var` -- Dependent variable (target)
#' * `seed` -- Seed used to reproduce results
#'
#' @examples
#' \dontrun{
#' # General use case
#' ps <- xgbp(survey, census, var1, var2, dep_var = Y)
#' }
#'
#' @export

xgbp <- function(survey, census, census_count, ..., dep_var = NULL,
                 seed = NULL, tune = TRUE, params = NULL, nrounds = 100,
                 n_iter = 25, nthread = 1, verbose = TRUE){


  # Check inputs
  if(!tibble::is_tibble(survey) & !is.data.frame(survey)){

    stop(cli::cli_alert_danger("'survey' object must be a data.frame or a tibble."))
  }

  if(!tibble::is_tibble(census) & !is.data.frame(census)){

    stop(cli::cli_alert_danger("'census' object must be a data.frame or a tibble."))
  }

  # Set seed for reproducibility
  if(verbose){

    d <- cli::cli_div(theme = list(rule = list("margin-bottom" = 1)))
    cli::cli_rule("Poststratification with {cli::col_cyan('XGBP')}")
    cli::cli_end(d)
  }
  set.seed(seed)

  # Process census data and convert covars to factor
  if(verbose) cli::cli_progress_step("Data processing")
  census <- census %>%
    tibble::as_tibble() %>%
    dplyr::ungroup() %>%
    stats::na.omit() %>% # Remove missings
    dplyr::mutate(dplyr::across(c(...), .fns = as.factor))

  # Create xgb matrix with covars
  fml <- stats::as.formula(~ .)
  est_mt <- stats::model.matrix(fml, data = dplyr::select(census, -{{ census_count }}))
  est_mt <- xgboost::xgb.DMatrix(data = est_mt)

  # Prepare data to train the model
  dep <- survey %>%
    dplyr::select({{ dep_var }}) %>%
    dplyr::rename(dep = {{ dep_var }})
  dep <- dep$dep

  # Create xgb matrix to train the model
  dados <- stats::model.matrix(fml, data = dplyr::select(survey, ...) %>%
                                 dplyr::mutate(dplyr::across(.fns = as.factor)))
  dados <- xgboost::xgb.DMatrix(data = dados,
                                   label = as.numeric(as.factor(dep)) - 1) # XGB's count

  # Select parameters (tune, default or provided by user)
  if(tune){

    if(verbose) cli::cli_progress_step("Tuning model parameters")
    res <- tune_xgbp(dados, ..., dep_var = dep, nrounds = nrounds,
                     nthread = nthread, n_iter = n_iter, seed = seed)

    params <- res$params
    nrounds <- res$nrounds

  } else if (is.null(params)) {

    params <- list(objective = "multi:softprob",
                   num_class = length(unique(dep)),
                   max.depth = 6, eta = 0.3,
                   subsample = 0.95,
                   min_child_weight = 1)
  }

  # Train the model
  if(verbose) cli::cli_progress_step("Model training")
  mod <- xgboost::xgboost(data = dados,
                          params = params,
                          nrounds = nrounds,
                          nthread = nthread,
                          early_stopping_rounds = 20,
                          verbose = 0,
                          eval_metric = "mlogloss")

  # Produce estimates by census strata
  if(verbose) cli::cli_progress_step("Generating estimates")
  res <- census %>%
    dplyr::bind_cols(

      stats::predict(mod, newdata = est_mt, reshape = T) %>%
        tibble::as_tibble(.name_repair = "minimal") %>%
        stats::setNames(levels(as.factor(dep)))
    ) %>%
    tidyr::pivot_longer(-c(..., {{ census_count }})) %>%
    dplyr::rename(cat = "name", est = "value", n_count = {{ census_count }})

  # Create the output
  res <- list(estimates = res, model = mod, data = dados, nrounds = nrounds,
              census = census, census_count = rlang::quo_name(rlang::enquo(census_count)),
              covars_matrix = est_mt, dep_var = dep, seed = seed)
  class(res) <- c("xgbp")

  # Return
  return(res)
}


#' Test if a object have XGBP class
#'
#' @param obj An object
#'
#' @export

is_xgbp <- function(obj) inherits(obj, "xgbp")




