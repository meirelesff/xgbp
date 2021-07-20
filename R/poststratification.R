#' Get estimates for XGBP
#'
#' `get_estimates` aggregates and returns estimates from a XGBP object.
#'
#' @param xgbp_out A `xgbp` object returned by the \code{\link{xgbp}} function
#' @param ... Group-level covars to aggregate results (leave blank for
#' sample-level estimates)
#'
#' @examples
#' \dontrun{
#' get_estimates(xgbp_out)
#' }
#'
#' @export

get_estimates <- function(xgbp_out, ...){

  # Test input
  if(!is_xgbp(xgbp_out)){

    stop(cli::cli_alert("'xgbp_out' must be an object returned from the 'xgbp' function."))
  }

  # Aggregates and returns estimates
  xgbp_out %>%
    dplyr::group_by(rlang::.data$cat, ...) %>%
    dplyr::mutate(prop = {{ rlang::.data$n_count }} / sum({{ rlang::.data$n_count }})) %>%
    dplyr::summarise(estimativa = sum(rlang::.data$prop * rlang::.data$est, na.rm = T))
}


