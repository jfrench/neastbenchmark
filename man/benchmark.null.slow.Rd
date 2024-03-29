% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/benchmark.null.slow.R
\name{benchmark.null.slow}
\alias{benchmark.null.slow}
\title{Automatic, but slow benchmarking of a null data set}
\usage{
benchmark.null.slow(
  MAXFUN,
  test.name,
  null.name,
  idx = seq_len(10000),
  ...,
  units = "auto"
)
}
\arguments{
\item{MAXFUN}{A function that returns only the maximum test statistic across all zones.  The first argument MUST take a vector of cases.}

\item{test.name}{The name of the test being applied.  Must be a character vector.}

\item{null.name}{Which null data set to use.  Must be one of
\code{"null600"} or \code{"null6000"} or \code{"fakenull"}.}

\item{idx}{A vector with the row indices of the data set
to be benchmarked.}

\item{...}{Additional arguments passed on to the
\code{MAXFUN}.}

\item{units}{The units of time for printing the iterative
evaluation time.  The default is \code{"auto"}.  See
\code{\link[base]{difftime}} for additional options.}
}
\value{
NULL.  Results are saved in an rda file.
}
\description{
This function provides an automated mechanism for
identifying the largest test
statistic for a simulated null data set within the
\code{benchmark2003} or \code{benchmark2006} data sets.
This function uses a loop and \code{\link{message}} to
print progress instead of the
\code{\link[pbapply]{pbapply}} function.  The advantage
is that incremental progress is easily seen, allowing the
user to identify any problematic rows of the data set.
The results for each row of the data set are saved in a
file using the name \code{paste("t", null.name, "_",
test.name, "_", i, ".rds", sep = ""),}, where \code{i} is
the row of the data set.
}
\details{
For the specified data set, \code{MAXFUN} is applied to
each row of the specified data sets.
}
\examples{
# load required data
data(neastdata)
# construct zone information
coords = neastdata[, c("x", "y")]
ubpop = 0.5
pop = neastdata$population

# all distinct zones subject to population constraints
zones = smerc::scan.zones(coords, pop, ubpop)
# expected number of cases in each region
e = 600/sum(pop)*pop

# expected number of cases in each zone
ein = sapply(zones, function(x) sum(e[x]))
# expected number of cases outside of each zone
eout = 600 - ein

# takes a set of cases and determines the largest
# test statistic across all zones using required
# information
max.scan.test = function(cases, zones, ein, eout, ty) {
  # compute yin for each zone
  yin = sapply(zones, function(zone) sum(cases[zone]))
  # take max over statistics of all zones
  max(smerc::scan.stat(yin, ein, eout, ty))
}

\dontrun{
benchmark.null.slow(MAXFUN = max.scan.test,
                    test.name = "scan_test",
                    null.name = "fakenull",
                    idx = seq_len(5),
                    zones = zones,
                    ein = ein,
                    eout = eout,
                    ty = 600)
clean.benchmark(test.name = "scan_test",
                    data.name = "fakenull",
                    idx = seq_len(5),
                    unlist = TRUE)
}
}
