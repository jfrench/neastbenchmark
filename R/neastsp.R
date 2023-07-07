#' Breast cancer mortality in the Northeastern United States
#'
#' A \code{\link[sp]{SpatialPolygonsDataFrame-class}} object
#' containing data related to breast cancer mortality in the
#' Northeastern United States.  The data include several variables observed for
#' 245 counties (or similar) as well polygon information defined using
#' longitude/latitude coordinates in the WGS84 coordinate
#' system. The following variables are included in the
#' object:
#' \itemize{
#' \item \code{id}: A name-based id for each county.
#' \item \code{cases}: The number of breast cancer
#' mortality cases between 1988-1992.
#' \item \code{population}: The number of residents in the county
#' based on 1990 U.S. census results.
#' \item \code{x}: An x coordinate of a centroid associated
#' with each county provided by Kulldorff et al. (2003). See
#' Details.
#' \item \code{y}: A y coordinate of a centroid associated
#' with each county provided by Kulldorff et al. (2003). See
#' Details.
#' }
#'
#' The \code{x} and \code{y} coordinates define centroids
#' associated with each county. The coordinates were
#' provided by Kulldorf et al. (2003). They are appropriate
#' for computing standard Euclidean intercentroid distance
#' between counties but are not consistent with the polygon
#' geometry of the data set.  The coordinate system
#' of these coordinates is unknown.
#'
#' Alternative centroids for the geometry can be obtained
#' through the \code{labpt} of each polygon using the code
#' below.
#'
#' \code{pts_list <- lapply(X = neastsp@polygons,
#'                          FUN = slot,
#'                          name = "labpt")} \cr
#' \code{pts <- do.call(rbind, pts_list)}
#'
#' @name neastsp
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
#' data(neastsp)
#' plot(neastsp)
#' spplot(neastsp, "cases")
#' }
#' 
NULL