#' \code{SpatialPolygons} object for \code{neastdata}
#'
#' This loads a \code{SpatialPolygons} providing the borders
#' for \code{neastdata}.
#'
#' @name neastpoly
#' @docType data
#' @references Martin Kulldorff, Eric J. Feuer, Barry A.
#'   Miller, Laurence S. Freedman; Breast Cancer Clusters in
#'   the Northeast United States: A Geographic Analysis,
#'   American Journal of Epidemiology, Volume 146, Issue 2,
#'   15 July 1997, Pages 161–170,
#'   \url{https://doi.org/10.1093/oxfordjournals.aje.a009247}
#'
#' @keywords data
#' @seealso neastdata
#' @examples
#' library(sp)
#' plot(neastpoly)
#' neastpolydf = SpatialPolygonsDataFrame(neastpoly, 
#'                                        data = neastdata, 
#'                                        match = FALSE)
#' spplot(neastpolydf, "cases")
NULL