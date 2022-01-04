#' Calculates mean elevations of patches of water
#'
#' @param waterFile Required. ArcGIS .asc file of water from WDPM.
#' @param threshold Optional. Threshold (in m) to discriminate water. Defaul is 0.001 m.
#' @param DEMfile Required. ArcGIS .asc file of DEM elevations.
#' @param quiet Optional. If \code{TRUE}, messages are suppressed.
#' @param return Optional. If \option{raster}, then returns a raster of the patch elevations.
#' If \option{patch_vals}, returns a data frame of patch properies. Otherwise, returns both.
#'
#'
#' @return Returns patch mean elevations.
#' @export
#'
#' @examples \dontrun{waterFile = "300_0_0_0_d.asc", DEMfile="basin5.asc")
#' }
waterPatchElevation <- function(waterFile, threshold = 0.001, DEMfile,
                                quiet = TRUE,
                                return = 'raster') {

  water  <- SDMTools::read.asc(waterFile)
  dem <- SDMTools::read.asc(DEMfile)
  patch_el_mean <- c(0)
  patch_id <- c(0)
  patch_area <- c(0)
  patch_perimeter <- c(0)

  cellsize <- attr(water, "cellsize")
  water <- raster::raster(water)
  dem <- raster::raster(dem)
  elevation <- water + dem

  # get patches
  patches <- getWaterPatches(waterFile, threshold)

  # get number of water patches
  patches_raster <- raster::raster(patches)
  patch_count <- length(raster::unique(patches_raster))
  if (!quiet) {
    cat(patch_count, " patches found\n", sep = "")
  }

  # set not-water areas to be zero

  elevation[water <= 0] <- 0

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
    single_patch <- raster::mask(elevation, water_mask, maskvalue = NA)

    # get mean elevation
    patch_el <- raster::cellStats(single_patch, stat = 'mean', na.rm = TRUE)

    # put back in raster
    single_patch <- raster::mask(elevation, water_mask, maskvalue = patch_el)
    elevation[water_mask] <- patch_el
    patch_el_mean[i] <- patch_el
    patch_id[i] <- i
    patch_area[i] <- patchStats$area[1]
    patch_perimeter[i] <- patchStats$perimeter[1]
  }

  patch_vals <- data.frame(patch_id, patch_el_mean, patch_area, patch_perimeter)
  if (return == "raster")
    return(elevation)
  else if (return == "data")
    return(patch_vals)
  else {
    all <- list(elevation = elevation, patch_vals = patch_vals,
                patches_raster = patches_raster)
    return(all)
  }

}
