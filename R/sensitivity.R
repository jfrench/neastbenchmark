#' Compute sensitivity/recall/true positive rate
#'
#' Compute the empirical sensitivity/recall/true positive
#' rate of a method from a series of tests.  In this
#' context, the sensitivity is the average proportion of the
#' population of the true hotspot that lies within the most
#' likely cluster.  The function requires the null test
#' statistics, the results from the observed data sets
#' (i.e., the maximum test statistic and most likely cluster
#' from each data set), the true hotspot locations, and the
#' vector of population sizes for each region.  See Details.
#'
#' In this context, the sensitivity is the proportion of the
#' true hotspot population lying within the most likely
#' cluster, averaged over all tests.  If the \code{pop}
#' vector is a vector of 1s, then the sensitivity will be
#' the average of the proportion of the most likely cluster
#' regions intersecting the true hotspot, divided by the
#' number of regions in the true hotspot.
#'
#' @inheritParams cpower
#' @param pop A vector with the populations associated with each region.
#'
#' @return A vector of sensitivity values.
#' @export
#'
#' @examples
#' tnull = 1:99
#' tdata = list(list(tmax = 96, mlc = c(50, 51)),
#'              list(tmax = 101, mlc = c(48, 57)))
#' sensitivity(tnull, tdata, 50, pop = rep(1, 100))
sensitivity = function(tnull, tdata, hotspot, pop, 
                       alpha = c(0.05, 0.01)) {
  if (length(pop) <= 1) {
    stop("pop should be the vector of population sizes for each region. It should have length more than 1.")
  }
  # sort all max statistics
  quants = sort(tnull)
  # determine location of appropriate quantiles
  idx = (length(quants) + 1) * (1 - alpha)
  # initial quantiles
  oq = quants[idx]
  # tmax for each simulated data set
  tmax = sapply(tdata, getElement, name = "tmax")
  # determine if tmax is less than initial quantiles
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

  pop_hotspot = sum(pop[hotspot])
  
  p = sapply(seq_along(tdata), function(i) {
    inter = intersect(tdata[[i]]$mlc, hotspot)
    pop_inter = sum(pop[inter])
    out[i, ] * pop_inter/pop_hotspot
    # pop_inter/pop_hotspot
  })
  
  if (length(p) == length(tdata)) {
    return(mean(p))  
  } else {
    return(rowMeans(p))
  }
}

#' @name recall
#' @rdname sensitivity
#' @export
recall = sensitivity

#' @name tpr
#' @rdname sensitivity
#' @export
tpr = sensitivity