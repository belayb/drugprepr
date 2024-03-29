% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overlap.R
\name{shift_interval}
\alias{shift_interval}
\title{Shift time intervals until they no longer overlap}
\usage{
shift_interval(x)
}
\arguments{
\item{x}{a data frame containing variables \code{start_date},
\code{stop_date} and \code{patid}}
}
\value{
A data frame with time intervals moved such that they no longer overlap
}
\description{
This is a function used by \code{\link{decision_9}}.
}
