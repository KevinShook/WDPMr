#' Finds volumes of water patches
#'
#' @param patches Required. \pkg{SDMTools} object containing numbered water patches, as returned by \code{getWaterPatches}.
#' @param waterFile Required. An ArcGIS .asc file containing water depths, as produced by \code{WDPM}.
#' @param quiet Optional. If \code{TRUE}, the default, there is no extra output. If \code{FALSE}, the patches are listed as they are measured.
#'
#' @return Returns a data frame consisting of the \code{ID}, \code{area} (m\eqn{^2}{^2}), and the \code{volume} (m\eqn{^3}{^3}) for each patch.
#' @export
#'
#' @examples \dontrun{
#' vols <- waterPatchVolume(patches, '5m_300_0_0_0_d.asc')
#' }
waterPatchVolume <- function(patches, waterFile, quiet = TRUE){
  volume <- c(0)
  area <- c(0)
  ID <- c(0)
  water  <-  SDMTools::read.asc(waterFile)
  cellsize <- attr(water, "cellsize")
  water <- raster::raster(water)

  #get number of water patches
  patches_raster <- raster::raster(patches)
  patch_count <- length(raster::unique(patches_raster))
  if (!quiet) {
    cat(patch_count, " patches found\n", sep = "")
  }
  for (i in 1:(patch_count - 1)) {
    # make binary, and set values outside the patch to be NA
    if (!quiet) {
      cat("patch: ", i, "\n", sep = "")
    }
    water_binary <- patches
    water_binary[water_binary != i] <- NA_real_
    water_binary[water_binary == i] <- 1
    ccl <- SDMTools::ConnCompLabel(water_binary)
    patchStats <- SDMTools::PatchStat(ccl, cellsize = cellsize)
    patchStats <- patchStats[patchStats$patchID > 0,]
    # now create mask
    water_mask <- raster::raster(water_binary)

    # apply mask
    single_patch <- raster::mask(water, water_mask, maskvalue = NA)

    # get mean depth
    patch_depth <- raster::cellStats(single_patch, stat = 'mean', na.rm = TRUE)

    # get area
    patch_area <- patchStats$area[1]

    volume[i] <- patch_depth * patch_area
    ID[i] <- i
    area[i] <- patch_area
  }

  # create data frame
  patchAreaVolume <- data.frame(ID, area, volume)
  return(patchAreaVolume)
}
