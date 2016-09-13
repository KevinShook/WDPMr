getPondBasinAreas <- function(patches, dem, missingvalue){
  # create fake inputs
  wetlandID <- c(0)
  wetlandAreas <- c(0)
  basinAreas <- c(0)

  size <- attr(dem, "cellsize")
  dem <- raster::raster(dem)
  patches <- raster::raster(patches)

  dem <- raster::as.matrix(dem)
  patches <- raster::as.matrix(patches)

  dem[is.na(dem)] <- missingvalue
  patches[is.na(patches)] <- missingvalue

  numCols <- ncol(dem)
  numRows <- nrow(dem)

  # call Fortran code
  retdata <- .Fortran("RunoffAreas", as.integer(numCols), as.integer(numRows), missingvalue,
                      size, dem, as.integer(patches), wetlandID, wetlandAreas, basinAreas,
                      PACKAGE = 'WDPMr')
}
