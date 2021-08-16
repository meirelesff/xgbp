#' Tune XGBoost model
#'
#' `tune_xgbp` tunes the parameters of XGBoost to improve first stage predicitons
#' and avid overfitting in the XGBP.
#'
#' @param survey A `tibble` created by the \code{\link{xgbp}} function (passed internally).
#' @param ... Individual and group level covariates used in the poststratification. All
#' variables must be included in the `survey` and in the `census` and passed unquoted to
#' the function call
#' @param dep_var Dependent variable. Must be `character` or `factor`
#' @param nrounds Number of trees (rounds) used in to train the model. Defaults to `100`
#' @param n_iter When `tune = TRUE`, this indicates how many samples to draw
#' during gridsearch to use. Defaults to `30`.
#' @param nthread Number of htreads used in the computation. Defaults to `1`, but users are
#' encourage to increase this number to speed up computations (the limit is the actual number
#' of threads available at your computer)
#' @param seed A seed for replication. Defaults to `NULL`
#'
#' @return A list with two elements:
#' * A list with the best parameters selected during gridsearch
#' * A vector with the optimal number of trees selected during gridsearch
#'
#' @importFrom rlang .data
#' @export

tune_xgbp <- function(survey, ..., dep_var = NULL, nrounds = 100, nthread = 1, n_iter = 25, seed = NULL){

  # Prepare a grid for random grid search
  set.seed(seed)
  mod_grid <- expand.grid(
    eta = seq(0.001, 0.3, by = 0.05),
    max_depth = 2:7,
    gamma = 0:5,
    num_class = length(unique(dep_var)),
    min_child_weight = seq(1, 9, by = 1),
    subsample = seq(0.8, 1, by = 0.05)
  ) %>%
    dplyr::slice_sample(n = n_iter)

  # Create parameters list and run 5-fold CV in each
  best_params <- mod_grid %>%
    dplyr::rowwise() %>%
    # Create list of parameters
    dplyr::mutate(params = list(list(

      objective = "multi:softprob",
      num_class = .data$num_class,
      eta = .data$eta,
      max_depth = .data$max_depth,
      gamma = .data$gamma,
      min_child_weight = .data$min_child_weight,
      subsample = .data$subsample
    ))) %>%
    dplyr::ungroup() %>%
    # Run cross-validation
    dplyr::mutate(model = purrr::map(.data$params,
                                     ~ xgboost::xgb.cv(params = .x,
                                                       data = survey,
                                                       early_stopping_rounds = 20,
                                                       nrounds = nrounds,
                                                       nthread = nthread,
                                                       nfold = 5,
                                                       verbose = 0,
                                                       eval_metric = "mlogloss"
                                     ))) %>%
    # Register best n of trees and best log loss
    dplyr::mutate(iteration = purrr::map_dbl(.data$model, ~ .x$best_iteration)) %>%
    dplyr::mutate(logloss = purrr::map_dbl(.data$model, ~ min(.x$evaluation_log$test_mlogloss_mean))) %>%
    # Select the best params
    dplyr::filter(.data$logloss == min(.data$logloss)) %>%
    # In case there are more than one model with the same logloss
    dplyr::slice(1)

  # Create a list with results
  params <- best_params %>%
    purrr::pluck("params") %>%
    purrr::pluck(1)
  nrounds <- best_params$iteration

  # Return
  return(list(params = params, nround = nrounds))
}
