#' Benchmark data sets
#' 
#' This function provides an automated mechanism for 
#' producing the largest test statistic for each simulated
#' data set in one of the \code{\link{benchmark2003}} or
#' \code{\link{benchmark2006}} data sets.
#' 
#' For the specified data sets, \code{TESTFUN} is applied
#' to each row of the specified data sets.
#' 
#' If the results are saved, then the name of the data object
#' saved is \code{paste("t", data.name, sep = "")}.
#' 
#' @param TESTFUN A function that returns a list containing the maximum test statistic and the indices of the most likely cluster.  The first argument MUST take a vector of cases.
#' @param test.name The name of the test being applied.  Must be a character vector.
#' @param data.name A vector of names for the \code{benchmark2003} or \code{benchmark2006} data sets.
#' @param SAVE A logical value indicating whether the results
#' should be saved as an rda file.  If \code{TRUE}, then the
#' file is saved as 
#' \code{paste("t", null.name, "_", test.name, ".rda", sep = "")}
#' to the current working directly.  If FALSE, the vector is returned.
#' Default is \code{FALSE}.
#' @param ... Additional arguments passed on to the MAXFUN and \code{\link[pbapply]{pbapply}}.
#'
#' @return A vector of the largest test statistics for each simulated data set, or writing out to an rda file.
#' @export 
#'
#' @examples
#' # load required data
#' data(neastdata)
#' # construct zone information
#' coords = neastdata[,c("easting", "northing")]
#' ubpop = 0.5
#' pop = neastdata$population
#' 
#' # all distinct zones subject to population constraints
#' zones = smerc::scan.zones(coords, pop, ubpop)
#' # expected number of cases in each region
#' e = 600/sum(pop)*pop
#' 
#' # expected number of cases in each zone
#' ein = sapply(zones, function(x) sum(e[x]))
#' # expected number of cases outside of each zone
#' eout = 600 - ein
#' 
#' # takes a set of cases and determines the largest
#' # test statistic across all zones using required
#' # information
#' mlc.scan.test = function(cases, zones, ein, eout, ty) {
#'   # compute yin for each zone
#'   yin = sapply(zones, function(zone) sum(cases[zone]))
#'   # take max over statistics of all zones
#'   tobs = scan.stat(yin, ein, eout, ty)
#'   wmax = which.max(tobs)
#'   return(list(tmax = tobs[wmax],
#'               mlc = zones[[wmax]]))
#' }
#' 
#' out = benchmark.data(TESTFUN = mlc.scan.test,
#'                      test.name = "scan_test",
#'                      data.name = c("fakedata1", "fakedata2"),
#'                      SAVE = FALSE,
#'                      zones = zones,
#'                      ein = ein,
#'                      eout = eout,
#'                      ty = 600)
benchmark.data = function(TESTFUN, test.name, 
                          data.name, 
                          SAVE = FALSE, ...) {
  if (!is.character(test.name)) {
    stop("test.name must be a character vector")
  } 

  outlist = vector("list", length(data.name))
  for (i in seq_along(data.name)) {
    dname = data.name[i]
    do.call(data, list(dname))
    oname = paste("t", dname, sep = "")
    message(paste("Analyzing",dname))
    tdata = pbapply::pbapply(get(dname), 1, 
                             FUN = TESTFUN, 
                             ...)
    if (SAVE) {
      assign(oname, tdata)
      save(list = oname, file = save_nm, compress = "bzip2")
    } else {
      outlist[[i]] = tdata
    }
  }
  if (SAVE) {
    return(NULL)
  } else {
    return(outlist)
  }
}