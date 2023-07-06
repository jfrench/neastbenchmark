#' Breast cancer mortality in the Northeastern United States
#'
#' A \code{\link[sp]{SpatialPolygonsDataFrame-class}} object
#' containing data related to breast cancer mortality in the
#' Northeastern United States.  The data include information
#' from 245 counties (or similar) and polygon information
#' converted to coordinate system EPSG 2263. The following
#' variables are included in the object:
#' \itemize{
#' \item \code{id}: The name of each county.
#' \item \code{population}: The number of residents in the county
#' based on 1990 U.S. census results.
#' \item \code{easting}: The easting coordinate of a centroid associated with each county.
#' \item \code{northing}: The northing coordinate of a centroid associated with each county.
#' \item \code{cases}: The number of breast cancer
#' mortality cases between 1988-1992.
#' }
#'
#' @name neast_sp
#' @docType data
#' @references Martin Kulldorff, Eric J. Feuer, Barry A.
#'   Miller, Laurence S. Freedman; Breast Cancer Clusters in
#'   the Northeast United States: A Geographic Analysis,
#'   American Journal of Epidemiology, Volume 146, Issue 2,
#'   15 July 1997, Pages 161–170,
#'   \url{https://doi.org/10.1093/oxfordjournals.aje.a009247}
#'
#' @keywords data
#' @examples
#' if (require(sp)) {
#' data(neast_sp)
#' plot(neast_sp)
#' spplot(neast_sp, "cases")
#' }
#' 
NULL