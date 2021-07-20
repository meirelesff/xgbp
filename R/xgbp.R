#' Extreme Gradient Boosting with Poststratification
#'
#' The package's main function, `xgbp` poststratify a survey response from a sample
#' using Extreme Gradient Boosting (XGB). Dependent variables can be both binomial
#' or multinomial and resulting estimates can be aggregate by the whole sample or
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
#' @param seed A seed for replication. Defaults to `44`
#' @param max.depth XGB's max depth. Indicates the maximum depth of a tree. Defaults to `6`.
#' @param eta XGB's step size shrinkage used in update to prevents overfitting. Defaults to `0.3`
#' @param nrounds XGB's number of rounds for boosting. Defaults to `100`.
#' @param subsample XGB's subsample ratio of the training instances. Defaults to `0.95`
#' @param min_child_weight XGB's minimum sum of instance weight (hessian) needed in a child.
#' Defaults to `0.9`
#' @param early_stopping_rounds XGB's early stopping. Validation metric needs to improve at
#' least once in every early_stopping_rounds round(s) to continue training. Defaults to `20`
#' @param nthread Number of htreads used in the computation. Defaults to `1`, but users are
#' encourage to increase this number to speed up computations (the limit is the actual number
#' of threads available at your computer)
#'
#' @examples
#' \dontrun{
#' x <- xgbp(survey, census, var1, var2, dep_var = Y)
#' }
#'
#' @export

xgbp <- function(survey, census, census_count, ..., dep_var = NULL, seed = 44,
                 max.depth = 6, eta = 0.3, nrounds = 100,
                 subsample = 0.95, min_child_weight = 0.9,
                 early_stopping_rounds = 20, nthread = 1){


  # Check inputs
  if(!tibble::is_tibble(survey) & !is.data.frame(survey)){

    stop(cli::cli_alert_danger("'survey' object must be a data.frame or a tibble."))
  }

  if(!tibble::is_tibble(census) & !is.data.frame(census)){

    stop(cli::cli_alert_danger("'census' object must be a data.frame or a tibble."))
  }

  # Set seed for reproducibility
  set.seed(seed)

  # Process census data and convert covars to factor
  census <- census %>%
    tibble::as_tibble() %>%
    dplyr::ungroup() %>%
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


  # Train the model
  mod <- xgboost::xgboost(data = dados,
                          objective = "multi:softprob",
                          params = list(num_class = length(unique(dep))),
                          max.depth = max.depth,
                          eta = eta,
                          nrounds = nrounds,
                          subsample = subsample,
                          min_child_weight = min_child_weight,
                          early_stopping_rounds = early_stopping_rounds,
                          nthread = nthread,
                          verbose = 0,
                          eval_metric = "mlogloss")

  # Produce estimates by census strata
  res <- census %>%
    dplyr::bind_cols(

      stats::predict(mod, newdata = est_mt, reshape = T) %>%
        tibble::as_tibble() %>%
        stats::setNames(levels(as.factor(dep)))
    ) %>%
    tidyr::pivot_longer(-c(..., {{ census_count }})) %>%
    dplyr::rename(cat = "name", est = "value", n_count = {{ census_count }})

  # Return
  return(res)
}
