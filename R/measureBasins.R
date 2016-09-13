#' Measures areas of basins, as found by Fortran program
#'
#' @param infile Required. ArcGIS .asc file containing numbered slough basins.
#'
#' @return Returns a data frame containing the slough basin ID, and the areas (m2)
#' @export
#'
#' @examples \dontrun{
#' areas <- measureBasins('300mm_basins.asc')}
measureBasins <- function(infile){
  basins  <-  SDMTools::read.asc(infile)
  cellsize <- attr(basins, "cellsize")

  #get number of patches
  basins_raster <- raster::raster(basins)
  patch_count <- length(raster::unique(basins_raster))

  # loop through all basins
  for (i in 1:(patch_count-1)){
    # make binary
    basins.binary <- basins
    basins.binary[basins.binary != i] <- 0
    basins.binary[basins.binary == i] <- 1


    # get patch connectivity
    ccl <- SDMTools::ConnCompLabel(basins.binary)

    # get patch stats - omit unfilled region
    patches <- SDMTools::PatchStat(ccl, cellsize=cellsize)
    patches <- patches[patches$patchID > 0,]
    area <- sum(patches$area)
    ID <- i
    patches <- data.frame(ID, area)
    if (i == 1)
      all <- patches
    else{
      all <- rbind(all, patches)
    }
  }
  return(all)
}
