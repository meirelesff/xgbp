% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poststratification.R
\name{get_estimates2}
\alias{get_estimates2}
\title{Get estimates for XGBP}
\usage{
get_estimates2(xgbp_out, ..., pivot = FALSE)
}
\arguments{
\item{xgbp_out}{A \code{xgbp} object returned by the \code{\link{xgbp}} function}

\item{...}{Group-level covars to aggregate results (leave blank for
sample-level estimates)}

\item{pivot}{Should the resuling table be pivoted to wider format? Defaults to
\code{FALSE}}
}
\description{
\code{get_estimates2} aggregates and returns estimates from a XGBP object. In
this version, marginals are based on ... variables.
}
\examples{
\dontrun{
get_estimates(xgbp_out)
}

}
