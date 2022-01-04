#' Finds volumes of largest water patches
#'
#' @param waterFile Required. An ArcGIS .asc file containing water depths, as produced by \code{WDPM}.
#' @param threshold Optional. Threshold for delineating water. Default is 0.001 m.
#' @return Returns a vector consisting of the \code{area} (m\eqn{^2}{^2}), and the \code{volume} (m\eqn{^3}{^3}) for the largest patch.
#' @export
#'
#' @examples \dontrun{
#' vols <- waterPatchVolume("5m_300_0_0_0_d.asc", threshold = 0.001)
#' }
largestWaterPatchVolume <- function(waterFile, threshold = 0.001){
  patches <- getWaterPatches(waterFile, threshold = threshold)
  patch_stats <- measureWaterPatches(waterFile, threshold = 0.001)
  largest_patch_num <- which.max(patch_stats$area)
  water  <-  SDMTools::read.asc(waterFile)

  cellsize <- attr(water, "cellsize")
  water <- raster::raster(water)

  patchBinary <- patches
  patchBinary[patches != largest_patch_num] <- NA_real_
  patchBinary[patches == largest_patch_num] <- 1

  #get number of water patches
  patches_raster <- raster::raster(patches)
  patch_count <- length(raster::unique(patches_raster))

  water_mask <- raster::raster(patchBinary)
  ccl <- SDMTools::ConnCompLabel(patchBinary)
  patchStats <- SDMTools::PatchStat(ccl, cellsize = cellsize)
  patchStats <- patchStats[patchStats$patchID > 0,]

  # apply mask
  single_patch <- raster::mask(water, water_mask, maskvalue = NA)

  # get mean depth
  patch_depth <- raster::cellStats(single_patch, stat = 'mean', na.rm = TRUE)

  # get area
  patch_area <- patchStats$area[1]

  volume <- patch_depth * patch_area

  area <- patch_area

  # create data frame
  patchAreaVolume <- c(area, volume)
  return(patchAreaVolume)
}
