#' Benchmark null data sets
#' 
#' This function provides an automated mechanism for 
#' producing the largest test statistic for each simulated
#' data set in one of \code{null600}, \code{null6000}, 
#' \code{fakenull}.
#' 
#' For the appropriate null data set, \code{MAXFUN} is applied
#' to each row of the specified null data set.
#' 
#' If the results are saved, then the name of the data object
#' saved is \code{paste("t", null.name, sep = "")}.
#' 
#' @param MAXFUN A function that returns only the maximum test statistic across all zones.  The first argument MUST take a vector of cases.
#' @param test.name The name of the test being applied.  Must be a character vector.
#' @param null.name Which null data set to use.  Must be one of
#' \code{"null600"} or \code{"null6000"} or \code{"fakenull"}.
#' @param SAVE A logical value indicating whether the results
#' should be saved as an rda file.  If \code{TRUE}, then the
#' file is saved as 
#' \code{paste("t", null.name, "_", test.name, ".rda", sep = "")}
#' to the current working directly.  If FALSE, the vector is returned.
#' Default is \code{FALSE}.
#' @inheritParams benchmark.data
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
#' max.scan.stat = function(cases, zones, ein, eout, ty) {
#'   # compute yin for each zone
#'   yin = sapply(zones, function(zone) sum(cases[zone]))
#'   # take max over statistics of all zones
#'   max(smerc::scan.stat(yin, ein, eout, ty))
#' }
#' 
#' benchmark.null(MAXFUN = max.scan.stat,
#'                test.name = "scan_test",
#'                null.name = "fakenull",
#'                SAVE = FALSE,
#'                zones = zones,
#'                ein = ein,
#'                eout = eout,
#'                ty = 600)
benchmark.null = function(MAXFUN, test.name, 
                          null.name = "null600", 
                          SAVE = FALSE, loop = FALSE,
                          pfreq = 1,
                          ...) {
  if (!is.character(test.name)) {
    stop("test.name must be a character vector")
  } 
  if (!is.element(null.name, 
                  c("null600", "null6000", "fakenull"))) {
    stop("null.name must be 'null600', 'null6000', or 'fakenull'")
  }

  save_nm = paste("t", null.name, "_", test.name, ".rda", sep = "")
  
  do.call(utils::data, list(null.name))
  oname = paste("t", null.name, sep = "")
  if (!loop) {
    tnull = pbapply::pbapply(get(null.name), 1, FUN = MAXFUN,
                             ...)
  } else {
    ndata = get(null.name)
    tnull = numeric(nrow(ndata))
    for (i in seq_along(tnull)) {
      tnull[i] = do.call(MAXFUN, list(ndata[i,], ...))
      if ((i %% pfreq) == 0 ) {
        message("Analysis of set ", i, " completed at ", Sys.time())
      }
    }
  }
  
  if (SAVE) {
    assign(oname, tnull)
    save(list = oname, file = save_nm, compress = "bzip2")
    return(NULL)
  } else {
    return(tnull)
  }
}