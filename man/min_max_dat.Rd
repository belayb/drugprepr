% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{min_max_dat}
\alias{min_max_dat}
\title{Example plausible ranges for prescription quantity and dosage}
\format{
A data frame with 2 rows and 5 variables:
\describe{
\item{prodcode}{CPRD unique code for the treatment selected by the GP}
\item{max_qty}{maximum possible quantity to be prescribed for the product}
\item{min_qty}{minimum possible quantity to be prescribed for the product}
\item{max_ndd}{maximum possible number of daily dose to be prescribed for the product}
\item{min_ndd}{minimum possible number of daily dose  to be prescribed for the product}
...
}
}
\usage{
min_max_dat
}
\description{
Minimum and maximum plausible values for total prescription quantity and
numerical daily dose of prescriptions given in the \code{\link{cprd}} dataset.
Both datasets are hypothetical.
}
\seealso{
\code{\link{cprd}}
}
\keyword{datasets}
