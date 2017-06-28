#' Finds fraction of a basin below the pour point
#'
#' @param DEMfile Required. ArcGIS \code{.asc} file of a basin DEM. Must have non-basin points indicated as missing.
#'
#' @return Returns the fraction of the basin area below the pour point.
#' @export
#'
#' @examples \dontrun{
#' fracBelowPour('basin5.asc')
#' }
fracBelowPour <- function(DEMfile){
  dem  <-  SDMTools::read.asc(DEMfile)
  cellsize <- attr(dem, "cellsize")
  dem <- raster::raster(dem)

  # find basin boundary
  divide <- raster::boundaries(dem, type='inner', classes=FALSE, directions=8, asNA=TRUE)

  # now get lowest boundary point
  divideDEM <- dem[divide == 1]
  pourEl <- min(divideDEM)

  # get basin size and # of points below pour
  demGood <- dem[!is.na(dem)]
  basinPoints <- length(demGood)
  belowPour <- demGood[demGood < pourEl]
  belowPourPoints <- length(belowPour)

  belowfrac <- belowPourPoints / basinPoints
  return(belowfrac)
}
