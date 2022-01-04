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
  basin_nums <- raster::unique(basins_raster)
  patch_count <- length(basin_nums)

  # loop through all basins
  for (i in 2:(patch_count)){
    # make binary
    basin <- basin_nums[i]
    basins.binary <- basins
    basins.binary[basins.binary != basin] <- 0
    basins.binary[basins.binary == basin] <- 1


    # get patch connectivity
    ccl <- SDMTools::ConnCompLabel(basins.binary)

    # get patch stats - omit unfilled region
    patches <- SDMTools::PatchStat(ccl, cellsize=cellsize)
    patches <- patches[patches$patchID > 0,]
    area <- sum(patches$area)
    ID <- basin
    patches <- data.frame(ID, area)
    if (i == 2)
      all <- patches
    else{
      all <- rbind(all, patches)
    }
  }
  return(all)
}
