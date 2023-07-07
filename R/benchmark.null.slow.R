#' Automatic, but slow benchmarking of a null data set
#'
#' This function provides an automated mechanism for
#' identifying the largest test
#' statistic for a simulated null data set within the
#' \code{benchmark2003} or \code{benchmark2006} data sets.
#' This function uses a loop and \code{\link{message}} to
#' print progress instead of the
#' \code{\link[pbapply]{pbapply}} function.  The advantage
#' is that incremental progress is easily seen, allowing the
#' user to identify any problematic rows of the data set.
#' The results for each row of the data set are saved in a
#' file using the name \code{paste("t", null.name, "_",
#' test.name, "_", i, ".rds", sep = ""),}, where \code{i} is
#' the row of the data set.
#'
#' For the specified data set, \code{MAXFUN} is applied to
#' each row of the specified data sets.
#'
#' @inheritParams benchmark.null
#' @param idx A vector with the row indices of the data set
#' to be benchmarked.
#' @param ... Additional arguments passed on to the
#'   \code{MAXFUN}.
#' @param units The units of time for printing the iterative
#'   evaluation time.  The default is \code{"auto"}.  See
#'   \code{\link[base]{difftime}} for additional options.
#'
#' @return NULL.  Results are saved in an rda file.
#' @export
#'
#' @examples
#' # load required data
#' data(neastdata)
#' # construct zone information
#' coords = neastdata[, c("x", "y")]
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
#' max.scan.test = function(cases, zones, ein, eout, ty) {
#'   # compute yin for each zone
#'   yin = sapply(zones, function(zone) sum(cases[zone]))
#'   # take max over statistics of all zones
#'   max(smerc::scan.stat(yin, ein, eout, ty))
#' }
#'
#' \dontrun{
#' benchmark.null.slow(MAXFUN = max.scan.test,
#'                     test.name = "scan_test",
#'                     null.name = "fakenull",
#'                     idx = seq_len(5),
#'                     zones = zones,
#'                     ein = ein,
#'                     eout = eout,
#'                     ty = 600)
#' clean.benchmark(test.name = "scan_test",
#'                     data.name = "fakenull",
#'                     idx = seq_len(5),
#'                     unlist = TRUE)
#' }
benchmark.null.slow = function(MAXFUN, test.name, 
                               null.name, 
                               idx = seq_len(10000), ..., 
                               units = "auto") {
  if (!is.character(test.name)) {
    stop("test.name must be a character vector")
  } 
  if (length(null.name) != 1) {
    stop("Only a single data set is allowed")
  }
  if (!is.element(null.name,
                  c("null600", "null6000", "fakenull"))) {
    stop("null.name must be 'null600', 'null6000', or 'fakenull'")
  }
  
  # save_nm = paste("t", null.name, "_", test.name, ".rda", sep = "")
  
  do.call(utils::data, list(null.name))
  oname = paste("t", null.name, sep = "")
  message(paste("Analyzing",null.name))
  ndata = get(null.name)
  # tnull = numeric(nrow(ndata))
  # for (i in seq_along(tnull)) {
  #   tnull[i] = do.call(MAXFUN, list(ndata[i,], ...))
  #   if ((i %% pfreq) == 0 ) {
  #     message("Analysis of set ", i, " completed at ", Sys.time())
  #   }
  # }
  
  # make sure indices are allowable
  if (min(idx) < 1) {
    stop("The minimum allowable idx is 1")
  }
  if (max(idx) > nrow(ndata)) {
    stop("The maximum of idx is not a valid row of the data set")
  }
  
  for (i in idx) {
    message(paste("Analysis of row", i, "started", Sys.time()))
    stime = Sys.time()
    out = do.call(MAXFUN, list(ndata[i, ], ...))
    etime = Sys.time()
    dtime = difftime(etime, stime, units = units)
    message(paste("Evaluation took approximately", 
                  round(dtime, 4),
                  attr(dtime, "units")))
    save_nm = paste(oname, "_", test.name, "_", i, ".rds", sep = "")
    saveRDS(out, file = save_nm, compress = "bzip2")
  }
  return(NULL)
}
