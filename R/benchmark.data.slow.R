#' Automatic, but slow benchmarking of a data set
#'
#' This function provides an automated mechanism for
#' identifying the most likely cluster and largest test
#' statistic for a simulated data set within the
#' \code{benchmark2003} or \code{benchmark2006} data sets.
#' This function uses a a loop and \code{\link{message}} to
#' print progress instead of the
#' \code{\link[pbapply]{pbapply}} function.  The advantage
#' is that incremental progress is easily seen, allowing the
#' user to identify any problematic rows of the data set.
#' The results for each row of the data set are saved in a
#' file using the name \code{paste("t", data.name, "_",
#' test.name, "_", i, ".rds", sep = ""),}, where \code{i} is
#' the row of the data set.
#'
#' For the specified data set, \code{TESTFUN} is applied to
#' each row of the specified data sets.
#'
#' @param TESTFUN A function that returns a list containing
#'   the maximum test statistic and the indices of the most
#'   likely cluster.  The first argument MUST take a vector
#'   of cases.
#' @inheritParams benchmark.data
#' @param data.name The name for the \code{benchmark2003} or
#'   \code{benchmark2006} data set to benchmark.  This can
#'   only be a single data set.
#' @param idx A vector with the row indices of the data set
#' to be benchmarked.
#' @param ... Additional arguments passed on to the
#'   \code{TESTFUN}.
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
#'   tobs = smerc::scan.stat(yin, ein, eout, ty)
#'   wmax = which.max(tobs)
#'   return(list(tmax = tobs[wmax],
#'               mlc = zones[[wmax]]))
#' }
#'
#' \dontrun{
#' benchmark.data.slow(TESTFUN = mlc.scan.test,
#'                     test.name = "scan_test",
#'                     data.name = "fakedata1",
#'                     idx = seq_len(10),
#'                     zones = zones,
#'                     ein = ein,
#'                     eout = eout,
#'                     ty = 600)
#' clean.benchmark(test.name = "scan_test",
#'                     data.name = "fakedata1",
#'                     idx = seq_len(10))
#' }
benchmark.data.slow = function(TESTFUN, test.name, 
                          data.name, 
                          idx = seq_len(10000), ..., 
                          units = "auto") {
  if (!is.character(test.name)) {
    stop("test.name must be a character vector")
  } 
  if (length(data.name) != 1) {
    stop("Only a single data set is allowed")
  }

  if (data.name == "c") {
    utils::data("cc")
    message(paste("Analyzing c"))
    oname = "tc"
    tdata = get("cc")
  } else {
    utils::data(list = data.name)
    oname = paste("t", data.name, sep = "")
    tdata = get(data.name)
    message(paste("Analyzing",data.name))
  }
  
  # make sure indices are allowable
  if (min(idx) < 1) {
    stop("The minimum allowable idx is 1")
  }
  if (max(idx) > nrow(tdata)) {
    stop("The maximum of idx is not a valid row of the data set")
  }
  
  # l = list(tdata[i,], zones = zones, ein = ein,
  #          eout = eout, ty = 600)
  
  for (i in idx) {
    message(paste("Analysis of row", i, "started", Sys.time()))
    stime = Sys.time()
    out = do.call(TESTFUN, list(tdata[i, ], ...))
    # out = do.call(TESTFUN, l)
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

#' @rdname benchmark.data.slow
#' @export
clean.benchmark = function(test.name, data.name, 
                           idx = seq_len(10000), 
                           SAVE = FALSE) {
  if (data.name == "c") {
    oname = "tc"
  } else {
    oname = paste("t", data.name, sep = "")
  }
  
  tdata = vector("list", length(idx))
  for (i in idx) {
    save_nm = paste(oname, "_", test.name, "_", i, ".rds", sep = "")
    tdata[[i]] = readRDS(save_nm)
  }
  
  if (SAVE) {
    assign(oname, tdata)
    new_save_nm = paste(oname, "_", test.name, ".rda", sep = "")
    save(list = oname, file = new_save_nm, compress = "bzip2")
  } else {
    return(tdata)
  }
}
