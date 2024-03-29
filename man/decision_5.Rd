% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algorithm.R
\name{decision_5}
\alias{decision_5}
\title{Decision 5: impute implausible prescription durations}
\usage{
decision_5(data, decision = "a")
}
\arguments{
\item{data}{a data frame}

\item{decision}{one of the following strings:
\describe{
\item{"a"}{leave duration as-is}
\item{"b_6"}{set to missing if > 6 months}
\item{"b_12"}{set to missing if > 12 months}
\item{"b_24"}{set to missing if > 24 months}
\item{"c_6"}{set to 6 months if > 6 months}
\item{"c_12"}{set to 12 months if > 12 months}
\item{"c_24"}{set to 24 months if > 24 months}
}}
}
\description{
A light wrapper around \code{\link{clean_duration}}.
}
\seealso{
Other decision functions: 
\code{\link{decision_10}()},
\code{\link{decision_1}()},
\code{\link{decision_2}()},
\code{\link{decision_3}()},
\code{\link{decision_4}()},
\code{\link{decision_6}()},
\code{\link{decision_7}()},
\code{\link{decision_8}()},
\code{\link{decision_9}()},
\code{\link{drug_prep}()}
}
\concept{decision functions}
