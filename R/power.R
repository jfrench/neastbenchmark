#' Compute power of test
#'
#' Compute the empirical power of a method from the null
#' test statistics, the results from the observed data,
#' and the true hotspot locations.  The types of power
#' include \code{basic}, which is simply the proportion of
#' observed data sets that a significant result was detected,
#' \code{intersect}, which is the proportion of times a
#' significant most likely cluster intersect the true hotspot,
#' \code{overlap}, which is the average of the proportion of  
#' the hotspot detected in each simulated data set, and 
#' \code{contain}, which is the proportion of times a significant
#' most likely cluster entirely contained the true hotspot.
#'
#' @param tnull The set of null test statistics
#' @param tdata The list of maximum test statistics (\code{tmax}) and most likely cluster (\code{mlc}) for each simulated data set.
#' @param hotspot A vector containing the hotspot indices for the current data set.
#' @param type The type of power to compute.  Options are \code{"basic"}, 
#' \code{"intersect"}, \code{"overlap"}, \code{"contain"}.  The default is \code{"basic"}.
#' @param alpha The type I error rate.  Default is \code{c(0.05, 0.01)}.
#'
#' @return A vector of power calculations.
#' @export
#'
#' @examples
#' tnull = 1:99
#' tdata = list(list(tmax = 96, mlc = c(50, 51)),
#'              list(tmax = 101, mlc = c(48, 57)))
#' cpower(tnull, tdata, 50, type = "basic")
#' cpower(tnull, tdata, 50, type = "intersect")
#' cpower(tnull, tdata, 50, type = "overlap")
#' cpower(tnull, tdata, 50, type = "contain")
cpower = function(tnull, tdata, hotspot, type = "basic", alpha = c(0.05, 0.01)) {
  if (length(type) != 1) stop("type must have length 1")
  if (!is.element(type, c("basic", "intersect", "overlap", "contain"))) {
    stop("invalid choice for type")
  }
  # sort all max statistics
  quants = sort(tnull)
  # determine location of appropriate quantiles
  idx = (length(quants) + 1) * (1 - alpha)
  # initial quantiles
  oq = quants[idx]
  # tmax for each simulated data set
  tmax = sapply(tdata, getElement, name = "tmax")
  # determine of tmax is less than initial quantiles
  nq = sapply(oq, function(x) tmax < x)
  # if they are, those positions need a different quantile
  quants2 = matrix(oq, nrow = length(tdata), 
                   ncol = length(oq), byrow = TRUE)
  wnq = which(nq, arr.ind = TRUE)
  for (i in seq_len(nrow(wnq))) {
    quants2[wnq[i,1], wnq[i,2]] = 
      max(tmax[wnq[i,1]], quants[idx[wnq[i,2]] - 1])
  }
  
  out = apply(quants2, 2, function(x) {
    tmax >= x
  })
  colnames(out) = alpha
  
  if (type == "basic") {
    return(colMeans(out))
  } else if (type == "intersect") {
    p = sapply(seq_along(tdata), function(i) {
      out[i, ] * (length(intersect(tdata[[i]]$mlc, hotspot)) > 0)
    })
    return(rowMeans(p))
  } else if (type == "overlap") {
    p = sapply(seq_along(tdata), function(i) {
      out[i, ] * length(intersect(tdata[[i]]$mlc, hotspot))/length(hotspot)
    })
    return(rowMeans(p))
  } else if (type == "contain") {
    p = sapply(seq_along(tdata), function(i) {
      out[i, ] * (length(intersect(tdata[[i]]$mlc, hotspot)) == length(hotspot))
    })
    return(rowMeans(p))
  }
}
