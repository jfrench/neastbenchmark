% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/power.R
\name{cpower}
\alias{cpower}
\title{Compute power of test}
\usage{
cpower(tnull, tdata, hotspot, alpha = c(0.05, 0.01))
}
\arguments{
\item{tnull}{The set of null test statistics}

\item{tdata}{The list of maximum test statistics
(\code{tmax}) and most likely cluster (\code{mlc}) for
each simulated data set.}

\item{hotspot}{A vector containing the hotspot indices
for the current data set.}

\item{alpha}{The type I error rate.  Default is
\code{c(0.05, 0.01)}.}
}
\value{
A vector of power calculations.
}
\description{
Compute the empirical power of a method from the null
test statistics, the results from the observed data, and
the true hotspot locations.  The power is simply the
proportion of observed data sets for which a significant
result was detected.
}
\examples{
tnull = 1:99
tdata = list(list(tmax = 96, mlc = c(50, 51)),
             list(tmax = 101, mlc = c(48, 57)))
cpower(tnull, tdata, 50)
}
