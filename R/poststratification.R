#' Get estimates for XGBP
#'
#' `get_estimates` aggregates and returns estimates from a XGBP object.
#'
#' @param xgbp_out A `xgbp` object returned by the \code{\link{xgbp}} function
#' @param ... Group-level covars to aggregate results (leave blank for
#' sample-level estimates)
#' @param pivot Should the resuling table be pivoted to wider format? Defaults to
#' `FALSE`
#' @param boot_iter Number of bootstrap iterations used to estimate non-parametric
#' confidence intervals. Defaults to `NULL`, in which case bootstrap is not performed.
#' For most applications, 100 up to 200 iterations should be enough to produce appropriate
#' confidence intervals for XGBP
#' @param ci_level Level of confidence intervals. Defaults to `0.95` (95% CI) and
#' is used only if `boot_iter` is set
#'
#' @details # Parallelization
#'
#' `get_estimates` uses `furrr`'s [furrr::future_map()] to perform bootstrap iterations.
#' By default, computation is done sequentially, but users can take advantage of parallelism
#' by declaring a plan using [future::plan()]. See below for an example of setting `multisession`
#' parallelism.
#'
#' @examples
#' \dontrun{
#' get_estimates(xgbp_out)
#' }
#'
#' @importFrom rlang .data
#' @export

get_estimates <- function(xgbp_out, ...,
                          pivot = FALSE,
                          boot_iter = NULL,
                          ci_level = 0.95){

  # Test inputs
  if(!is_xgbp(xgbp_out)){

    stop(cli::cli_alert("'xgbp_out' must be an object returned by the 'xgbp' function."))
  }

  if(!(is.null(boot_iter) | is.numeric(boot_iter))){

    stop(cli::cli_alert("'boot_iter' must be NULL or integer."))
  }

  if((is.numeric(ci_level) & ci_level > 0 & ci_level <= 1)){

    stop(cli::cli_alert("'ci_level' must be a numeric between 0 a 1."))
  }



  # Aggregates and returns estimates
  res <- xgbp_out %>%
    purrr::pluck("estimates") %>%
    dplyr::group_by(.data$cat, ...) %>%
    dplyr::mutate(prop = .data$n_count / sum(.data$n_count)) %>%
    dplyr::summarise(estimate = sum(.data$prop * .data$est, na.rm = T), .groups = "drop")

  # Pivot table?
  if(pivot){

    res <- tidyr::pivot_wider(res, names_from = c(...),
                              values_from = .data$estimate)
  }

  # Return
  return(res)
}


