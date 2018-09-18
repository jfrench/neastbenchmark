#' Benchmark power of test
#'
#' @param test.name The name of the test being applied.
#' @param data.name The name(s) of the data sets to be tested.
#' @inheritParams power 
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' benchmark.power("scan_stat", 
#'                 data.name = c("mixed01", "mixed01_6000"),
#'                 type = "basic")
#' }
benchmark.power = function(test.name,
                           data.name, 
                           type = "basic",
                           alpha = c(0.05, 0.01)) {
  as.data.frame(t(sapply(data.name, function(x) {
    message(paste("Analyzing", x))
    # determine null
    nc = nchar(x)
    if (substr(x, nc - 3, nc) == "6000") {
      load(paste("tnull6000_",test.name,".rda", sep = ""))
      assign("tnull", get("tnull6000"))
    } else {
      load(paste("tnull600_",test.name,".rda", sep = ""))
      assign("tnull", get("tnull600"))
    }
    # load results
    load(paste("t", x, "_", test.name, ".rda", sep = ""))
    assign("tdata", get(paste("t", x, sep = "")))
    # load hotspot information
    do.call(utils::data, list(paste(x, "_hotspot", sep = "")))
    assign("hotspot", get(paste(x, "_hotspot", sep = "")))
    # compute power
    power(tnull, tdata, hotspot, type = type, 
          alpha = alpha)
  })))
}
  
