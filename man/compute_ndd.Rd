% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ndd.R
\name{compute_ndd}
\alias{compute_ndd}
\title{Compute numerical daily dose from free-text prescribing instructions}
\usage{
compute_ndd(data, dose_fn = mean, freq_fn = mean, interval_fn = mean)
}
\arguments{
\item{data}{a data frame containing free-text prescribing instructions in a
column called \code{text}.}

\item{dose_fn}{function to summarise range of numbers by a single value}

\item{freq_fn}{function to summarise range of frequencies by a single value}

\item{interval_fn}{function to summarise range of intervals by a single value}
}
\value{
A data frame mapping the raw \code{text} to structured dosage information.
}
\description{
The function calls the \R package \strong{doseminer} to extract dose
information from free-text prescribing instructions, then computes the
average numerical daily dose according to a given decision rule.
}
\details{
The general formula for computing numerical daily dose (ndd) is given by
\deqn{\mbox{ndd} = \mbox{DF} \times \mbox{DN} / \mbox{DI},}{ndd = DF * DN / DI,}
where
\describe{
\item{DF}{is dose frequency, the number of dose 'events' per day}
\item{DN}{is dose number, or number of units of drug taken during each dose 'event'}
\item{DI}{is dose interval, or the number of days between 'dose days', where an interval of 1 means every day}
}
Prescriptions can have a variable dose frequency or dose number, such as
'2-4 tablets up to 3 times per day'. In this case, the user can choose
to reduce these ranges to single values by taking the minimum, maximum or
average of these endpoints.
}
\examples{
compute_ndd(cprd, min, min, mean)

}
