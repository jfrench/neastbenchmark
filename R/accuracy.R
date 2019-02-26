#' Compute accuracy
#'
#' Compute the empirical accuracy of a method from a series
#' of tests.  In this context, the accuracy is the average
#' proportion of the the total population that was correctly
#' placed within the cluster (for the outbreak regions) or
#' outside the cluster (for the null regions).  The function
#' requires the null test statistics, the results from the
#' observed data sets (i.e., the maximum test statistic and
#' most likely cluster from each data set), the true hotspot
#' locations, and the vector of population sizes for each
#' region.  See Details.
#'
#' @inheritParams sensitivity
#'
#' @return A vector of specificity values.
#' @export
#'
#' @examples
#' tnull = 1:99
#' tdata = list(list(tmax = 96, mlc = c(50, 51)),
#'              list(tmax = 101, mlc = c(48, 57)))
#' accuracy(tnull, tdata, 50, pop = rep(10, 100))
accuracy = function(tnull, tdata, hotspot, pop, alpha = c(0.05, 0.01)) {
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

  # total population
  tpop = sum(pop)
  
  # determine null region
  all_regions = seq_along(pop)
  null_region = setdiff(all_regions, hotspot)
  # sum of population in null region
  pop_null = sum(pop[null_region])
  
  p = sapply(seq_along(tdata), function(i) {
    # compute regions accurately predicted as hotspots
    inter = intersect(tdata[[i]]$mlc, hotspot)
    # is test significant
    is_sig = out[i, ]
    # if test is significant, computer sum of population
    # in inter, otherwise, the population correctly
    # identified as part of the cluster is 0.
    pop_inter = is_sig * sum(pop[inter])
    # compute regions accurately predicted as null regions
    outer = setdiff(all_regions, tdata[[i]]$mlc)
    outer = intersect(outer, null_region)
    pop_outer = sum(pop[outer])
    # if the test wasn't significant, the entire null
    # population was correctly identified
    pop_outer = (pop_outer * is_sig)
    pop_outer[is_sig == 0] = pop_null
    
    (pop_inter + pop_outer)/tpop
  })
  
  if (length(p) == length(tdata)) {
    return(mean(p))
  } else {
    return(rowMeans(p))
  }
}