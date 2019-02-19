#' Benchmark power of test
#'
#' @param test.name The name of the test being applied.
#' @param data.name The name(s) of the data sets to be tested.
#' @inheritParams sensitivity
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' benchmark.power("scan_stat", 
#'                 data.name = c("mixed01", "mixed01_6000"))
#' }
benchmark.power = function(test.name,
                           data.name, 
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
    cpower(tnull, tdata, hotspot, alpha = alpha)
  })))
}

#' @rdname benchmark.power
#' @export
benchmark.sensitivity = function(test.name,
                                 data.name, 
                                 pop,
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
    # compute sensitivity
    sensitivity(tnull, tdata, hotspot, pop, alpha = alpha)
  })))
}

#' @rdname benchmark.power
#' @export
benchmark.recall = benchmark.sensitivity

#' @rdname benchmark.power
#' @export
benchmark.tpr = benchmark.sensitivity

#' @rdname benchmark.power
#' @export
benchmark.specificity = function(test.name,
                                 data.name, 
                                 pop,
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
    # compute specificity
    specificity(tnull, tdata, hotspot, pop, alpha = alpha)
  })))
}

#' @rdname benchmark.power
#' @export
benchmark.selectivity = benchmark.specificity

#' @rdname benchmark.power
#' @export
benchmark.tnr = benchmark.specificity

#' @rdname benchmark.power
#' @export
benchmark.ppv = function(test.name,
                         data.name, 
                         pop,
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
    # compute ppv
    ppv(tnull, tdata, hotspot, pop, alpha = alpha)
  })))
}

#' @rdname benchmark.power
#' @export
benchmark.precision = benchmark.ppv

  
