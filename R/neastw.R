#' Binary adjacency matrix for \code{neastdata}
#'
#' A matrix indicating the binary adjacency matrix for the
#' \code{neastdata}.  Some of the islands (e.g., Nantucket)
#' are considered adjacent to the mainland because of
#' ferries traveling from certain mainland regions to these
#' islands. Manual connections were added for many of the
#' New York counties because they are on islands. 
#' \code{neastw_old} is an old version included for backwards
#' compatibility.  \code{neastw_old} and has some
#' imperfections related to symmetry and does not have
#' as many connections for the New York counties.
#'
#' @name neastw
#' @docType data
#' @references Martin Kulldorff, Eric J. Feuer, Barry A.
#'   Miller, Laurence S. Freedman; Breast Cancer Clusters in
#'   the Northeast United States: A Geographic Analysis,
#'   American Journal of Epidemiology, Volume 146, Issue 2,
#'   15 July 1997, Pages 161–170,
#'   \url{https://doi.org/10.1093/oxfordjournals.aje.a009247}
#'
#'
#' @keywords data
#' @seealso neastdata, neastpoly
#' @examples
#' if (require(spdep) & require(sp)) {
#' data(neastpoly)
#' plot(neastpoly)
#' data(neastw)
#' lw = mat2listw(neastw, style = "B")
#' plot(lw, coords = coordinates(neastpoly),
#'      col = "orange", add = TRUE)
#' }
NULL

#' @name neastw_old
#' @rdname neastw
#' @format NULL
#' @docType data
NULL