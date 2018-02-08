#' Finds fraction of a basin below the pour point
#'
#' @param DEMfile Required. ArcGIS \code{.asc} file of a basin DEM. Must have non-basin points indicated as missing.
#' @param fractionOnly Optional. If \code{TRUE} (the default) then only the fraction below the pour point is returned. If \code{FALSE}, then other statistics, and the raster below the pour point are returned
#'
#' @return Returns either the fraction of the basin below the pour point or a list containing the fraction of the basin area below the pour point and the rasters of the below-outlet dem and of the divide.
#' @export
#'
#' @examples \dontrun{
#' fracBelowPour('basin5.asc')
#' }
fracBelowPour <- function(DEMfile, fractionOnly=TRUE){
  dem  <-  SDMTools::read.asc(DEMfile)
  cellsize <- attr(dem, "cellsize")
  dem <- raster::raster(dem)
  cellArea <- cellsize ^ 2

  # find basin boundary
  divide <- raster::boundaries(dem, type='inner', classes=FALSE, directions=8, asNA=TRUE)

  # now get lowest boundary point
  divideDEM <- dem[divide == 1]
  pourEl <- min(divideDEM)

  # find outlet location
  minLoc <- raster::which.min(divideDEM)

  # if ties, get 1st one
  if (length(minLoc) > 1)
    minLoc <- minLoc[1]
  outletLoc <- raster::xyFromCell(divide, minLoc)


  # get basin size and # of points below pour
  demGood <- dem[!is.na(dem)]
  basinPoints <- length(demGood)
  belowPour <- demGood[demGood < pourEl]
  belowPourPoints <- length(belowPour)

  belowfrac <- belowPourPoints / basinPoints
  basinArea <- raster::area(dem)

  belowRegion <- dem < pourEl

  if(fractionOnly)
    allBasindata <- belowfrac
  else{
    allBasindata <- list(belowfrac = belowfrac, pourEl=pourEl, basinPoints = basinPoints,
                         outletLoc = outletLoc, cellArea= cellArea, belowRegion=belowRegion, divide=divide)
  }

  return(allBasindata)
}
