#' Get estimates for XGBP
#'
#' `get_estimates` aggregates and returns estimates from a XGBP object.
#'
#' @param xgbp_out A `xgbp` object returned by the \code{\link{xgbp}} function
#' @param ... Group-level covars to aggregate results (leave blank for
#' sample-level estimates)
#' @param pivot Should the resuling table be pivoted to wider format? Defaults to
#' `FALSE`
#'
#' @examples
#' \dontrun{
#' get_estimates(xgbp_out)
#' }
#'
#' @importFrom rlang .data
#' @export

get_estimates <- function(xgbp_out, ..., pivot = FALSE){

  # Test input
  if(!(is_xgbp(xgbp_out) | is_xgbp_boot(xgbp_out))){

    stop(cli::cli_alert("'xgbp_out' must be an object returned by the 'xgbp' or 'boostrap' functions."))
  }

  # Produce estimates
  res <- xgbp_out %>%
    purrr::pluck("estimates") %>%
    dplyr::group_by(.data$cat, ...) %>%
    dplyr::mutate(prop = .data$n_count / sum(.data$n_count)) %>%
    dplyr::summarise(estimate = sum(.data$prop * .data$est, na.rm = T), .groups = "drop")

  # In case of bootstrap intervals
  if(is_xgbp_boot(xgbp_out)){

    cis <- xgbp_out$boots %>%
      dplyr::group_by(.data$id, .data$cat, ...) %>%
      dplyr::mutate(prop = .data$n_count / sum(.data$n_count)) %>%
      dplyr::summarise(estimate = sum(.data$prop * .data$est, na.rm = T), .groups = "drop") %>%
      dplyr::group_by(.data$cat, ...) %>%
      dplyr::summarise(up = stats::quantile(estimate, probs = 1 - ci_level),
                       lo = stats::quantile(estimate, probs = ci_level)) %>%
      dplyr::select(.data$up, .data$lo)

    res <- dplyr::bind_cols(res, cis)
  }

  # Pivot table?
  if(pivot){

    res <- tidyr::pivot_wider(res, names_from = c(...),
                              values_from = .data$estimate)
  }

  # Return
  return(res)
}



