#' Calculate bootstrap confidence intervals for XGBP
#'
#' `bootstrap` uses sample with replacement from provided survey data to calculate
#' non-parametric confidence intervals for the `xgbp` function
#'
#' @param xgbp_out A `xgbp` object returned by the \code{\link{xgbp}} function
#' @param boot_iter Number of bootstrap iterations used to estimate non-parametric
#' confidence intervals. Defaults to `100`
#' @param ci_level Level of confidence intervals. Defaults to `0.95` (95% CI)
#' @param verbose Should the function report messages along the estimation? Defaults to `TRUE`
#'
#' @details # Parallelization
#'
#' `get_estimates` uses `furrr`'s [furrr::future_map()] to perform bootstrap iterations.
#' By default, computation is done sequentially, but users can take advantage of parallelism
#' by declaring a plan using [future::plan()]. See below for an example of setting `multisession`
#' parallelism.
#'
#' @export

bootstrap <- function(xgbp_out, boot_iter = 100, ci_level = 0.95, verbose = TRUE){


  # Test inputs
  if(!is_xgbp(xgbp_out)){

    stop(cli::cli_alert("'xgbp_out' must be an object returned by the 'xgbp' function."))
  }

  if(!(is.null(boot_iter) | is.numeric(boot_iter))){

    stop(cli::cli_alert("'boot_iter' must be NULL or integer."))
  }

  if(!(is.numeric(ci_level) & ci_level > 0 & ci_level <= 1)){

    stop(cli::cli_alert("'ci_level' must be a numeric between 0 a 1."))
  }

  # Start boostraping
  if(verbose) cli::cli_progress_step("Bootstraping confidence intervals")

  # Run bootstrap models
  boots <- furrr::future_map(1:boot_iter, ~ iter_bootstrap(xgbp_out) %>%
                               dplyr::mutate(id = .x),
                             .options = furrr::furrr_options(seed = xgbp_out$seed)) %>%
    dplyr::bind_rows()

  # Change class and returns
  res <- list(estimates = xgbp_out$estimates, boots = boots, ci_level = ci_level)
  class(res) <- c("xgbp_boot")
  return(boots)
}


# Internal function to run bootstrap iterations
iter_bootstrap <- function(xgbp_out){


  # Create matrix
  n_rows <- xgboost::getinfo(xgbp_out$data, name = "nrow")
  data <- xgboost::slice(xgbp_out$data, sample(1:n_rows, n_rows, replace = TRUE))

  # Train model and return
  mod <- xgboost::xgboost(data = data,
                          params = xgbp_out$model$params,
                          nrounds = xgbp_out$nrounds,
                          early_stopping_rounds = 20,
                          verbose = 0)

  # Get estimates and return
  cats <- unique(xgbp_out$dep_var)

  xgbp_out$census %>%
    dplyr::bind_cols(

      stats::predict(mod, newdata = xgbp_out$covars, reshape = T) %>%
        tibble::as_tibble(.name_repair = "minimal") %>%
        stats::setNames(levels(as.factor(xgbp_out$dep_var)))
    ) %>%
    tidyr::pivot_longer(dplyr::all_of(cats)) %>%
    dplyr::rename(cat = "name", est = "value", n_count = xgbp_out$census_count)
}


#' Test if a object have XGBP boot class
#'
#' @param obj An object
#'
#' @export

is_xgbp_boot <- function(obj) inherits(obj, "xgbp_boot")
