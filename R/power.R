#' Compute power of test
#'
#' @param tnull The set of null test statistics
#' @param tdata The list of maximum test statistics (\code{tmax}) and most likely cluster (\code{mlc}) for each simulated data set.
#' @param hotspot A vector containing the hotspot indices for the current data set.
#' @param type The type of power to compute.  \code{"basic"} simply determines whether a cluster is detected at the provided \code{alpha} leves.  \code{type = "touch"} computes
#' the proportion of tests where the significant clusters intersect the true cluster. \code{"overlap"} computers the average proportion of the hotspot detected across all simulations.
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
#' cpower(tnull, tdata, 50, type = "touch")
#' cpower(tnull, tdata, 50, type = "overlap")
cpower = function(tnull, tdata, hotspot, type = "basic", alpha = c(0.05, 0.01)) {
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
  
  if (type == "basic") {
    out = apply(quants2, 2, function(x) {
      tmax >= x
    })
    colnames(out) = alpha
    return(colMeans(out))
  } else if (type == "touch") {
    out = apply(quants2, 2, function(x) {
      tmax >= x
    })
    p = sapply(seq_along(tdata), function(i) {
      out[i, ] * (length(intersect(tdata[[i]]$mlc, hotspot)) > 0)
    })
    return(rowMeans(p))
  } else if (type == "overlap") {
    out = apply(quants2, 2, function(x) {
      tmax >= x
    })
    p = sapply(seq_along(tdata), function(i) {
      out[i, ] * (length(intersect(tdata[[i]]$mlc, hotspot)) > 0)/length(hotspot)
    })
    return(rowMeans(p))
  }
}
