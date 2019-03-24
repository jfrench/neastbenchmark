#' Null data for benchmark2003 and benchmark2006
#' 
#' The null data sets that should be used for the
#' benchmark2003 and benchmark2006 data sets.  The two
#' null data sets are \code{null600} and \code{null6000}.
#' Each data set is a 99999 x 245 matrix.  Each row
#' of the matrix represents a null data set (generated
#' under the constant risk hypothesis) for the 245 regions.
#' The total number of cases in each row are 600 and 6000,
#' respectively.
#'
#' @name null600
#' @rdname benchmarknull
#' @docType data
#' @references 
#' Song, C., & Kulldorff, M. (2003). Power evaluation of disease clustering tests. International Journal of Health Geographics, 2, 9. http://doi.org/10.1186/1476-072X-2-9
#' 
#' Duczmal, L., Kulldorff, M., & Huang, L. (2006). Evaluation of Spatial Scan Statistics for Irregularly Shaped Clusters. Journal of Computational and Graphical Statistics, 15(2), 428-442. Retrieved from http://www.jstor.org/stable/27594187
#'
#' @format  A matrix
#' @keywords data
#' @seealso neastdata
NULL

#' @name null6000
#' @rdname benchmarknull
#' @format NULL
#' @docType data
NULL