#' Finds location of farthest point from outlet
#'
#' @param DEMfile Required. ArcGIS \code{.asc} file of a basin DEM. Must have non-basin points indicated as missing.
#' @export
#'
#' @return Returns a list containing the XY coordinates and the distance of the point farthest from the outlet.
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

  maxDivideDist <- raster::maxValue(distanceFromOutlet)
  maxDistLoc <- raster::which.max(distanceFromOutlet)
  maxDistXY <- raster::xyFromCell(dem, maxDistLoc, spatial = FALSE)
  output <- list(maxDistXY = maxDistXY, maxDivideDist = maxDivideDist)
  return(output)
}
