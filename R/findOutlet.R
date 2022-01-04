#' Finds lowest elevation in a DEM file, and the basin pour point
#'
#' @param DEMfile Required. ArcGIS \code{.asc} file of a basin DEM. Must have non-basin points indicated as missing.
#'
#' @return Returns a list containing the mininum elevation within
#'
#' @seealso \code{\link{findFarthest}} \code{\link{findClosestPointToOutlet}}
#' the basin, its location, the pour point elevation and its location.
#' @export
#'
#' @examples \dontrun{
#' outlet <- findOutlet('basin5.asc')
#' }
findOutlet <- function(DEMfile){
  dem  <-  SDMTools::read.asc(DEMfile)
  cellsize <- attr(dem, "cellsize")
  dem <- raster::raster(dem)

  # find lowest point
  lowestEl <- raster::minValue(dem)
  lowestLoc <- raster::which.min(dem)
  lowestXY <- raster::xyFromCell(dem, lowestLoc, spatial = FALSE)

  # find basin boundary
  divide <- raster::boundaries(dem, type='inner', classes = FALSE,
                               directions = 8, asNA = TRUE)

  # create mask
  emptyRaster <- divide
  emptyRaster[emptyRaster != 1] <- 0

  m <- raster::mask(dem, emptyRaster)

  # now get lowest boundary point
  pourEl <- raster::minValue(m)
  pourLoc <- raster::which.min(m)
  pourXY <- raster::xyFromCell(m, pourLoc, spatial = FALSE)
  # assemble output
  output <- list(lowestEl = lowestEl, lowestXY = lowestXY, pourEl = pourEl, pourXY = pourXY)
  return(output)
}
