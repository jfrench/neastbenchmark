#' Compute specificity/selectivity/true negative rate
#'
#' Compute the empirical specificity/selectivity/true negative
#' rate of a method from a series of tests.  In this
#' context, the specificity is the average proportion of the
#' population of the true null region that lies outside the most
#' likely cluster.  The function requires the null test
#' statistics, the results from the observed data sets
#' (i.e., the maximum test statistic and most likely cluster
#' from each data set), the true hotspot locations, and the
#' vector of population sizes for each region.  See Details.
#'
#' In this context, the specificity is the proportion of the
#' true null region population lying outside the most likely
#' cluster, averaged over all tests.
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
#' specificity(tnull, tdata, 50, pop = rep(10, 100))
specificity = function(tnull, tdata, hotspot, pop, alpha = c(0.05, 0.01)) {
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

  all_regions = seq_along(pop)
  null_region = setdiff(all_regions, hotspot)
  pop_hotspot = sum(pop[hotspot])
  pop_null = sum(pop) - pop_hotspot
  
  p = sapply(seq_along(tdata), function(i) {
    outer = setdiff(all_regions, tdata[[i]]$mlc)
    outer = intersect(outer, null_region)
    pop_outer = sum(pop[outer])
    # proportion of null population intersecting outer
    temp = out[i, ] * pop_outer/pop_null
    # if we didn't reject, we "detected" the entire null region
    temp[-which(out[i,])] = 1
    temp
  })
  
  if (length(p) == length(tdata)) {
    return(mean(p))
  } else {
    return(rowMeans(p))
  }
}

#' @name selectivity
#' @rdname specificity
#' @export
selectivity = specificity

#' @name tnr
#' @rdname specificity
#' @export
tnr = specificity