#' Finds location of farthest point from outlet
#'
#' @param DEMfile Required. ArcGIS \code{.asc} file of a basin DEM. Must have non-basin points indicated as missing.
#' @export
#'
#' @return Returns a list containing the XY coordinates and the distance of the point farthest from the outlet.
#' @seealso \code{\link{findOutlet}}
#'
#' @examples \dontrun{
#' farthest <- findFarthest("basin5.asc")
#' }
findFarthest <- function(DEMfile) {
  dem  <-  SDMTools::read.asc(DEMfile)
  dem <- raster::raster(dem)

  # first find outlet
  outlet <- findOutlet(DEMfile)
  #output <- list(lowestEl=lowestEl, lowestXY=lowestXY, pourEl=pourEl, pourXY=pourXY)

  # find basin divide
  divide <- raster::boundaries(dem, type = "inner", classes = FALSE,
                               directions = 8, asNA = TRUE)

  # get distance of each point on the divide from the outlet
  distanceFromOutlet <- raster::distanceFromPoints(divide, outlet$pourXY)

  # clip distance with divide
  distanceFromOutlet <- distanceFromOutlet * divide

  maxDivideDist <- raster::maxValue(distanceFromOutlet)
  maxDistLoc <- raster::which.max(distanceFromOutlet)
  maxDistXY <- raster::xyFromCell(divide, maxDistLoc, spatial = FALSE)
  max_dist_dem_loc <- cellFromXY(dem, maxDistXY)
  max_dist_el <- raster::extract(dem, max_dist_dem_loc)

  # get elevation of farthest point (useful for slope)
  output <- list(maxDistXY = maxDistXY, maxDivideDist = maxDivideDist, maxDistEl = max_dist_el)
  return(output)
}
