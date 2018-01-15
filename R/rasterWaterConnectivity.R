#' Find connectivity of ponds in raster
#'
#' @param elevations List of pond data as returned by \code{waterPatchElevation}, using \option{all} for \code{return}.
#' @param quiet Optional. If \code{TRUE}, the default, there is no extra output. If \code{FALSE}, the patches are listed as they are located.
#'
#' @return Returns a data frame of patch parameters (ID, area, perimeter, and destination). The destination of the first pond is always \code{0} (zero), as used by the model PCM and the function \code{graphPCMPonds}.
#' @export
#'
#' @examples \dontrun{conn <- rasterWaterConnectivity(el, FALSE)
#' }
rasterWaterConnectivity <- function(elevations, quiet = TRUE) {
  patches <- elevations$patch_vals
  patches_raster <- elevations$patches_raster
  numPatches <- nrow(patches)
  connectivity <- c(0)

  # sort patches
  patches_sorted <- patches[order(patches$patch_el_mean, decreasing = TRUE),]

  for (i in 1:(numPatches - 1)) {
    current_patch <- patches_sorted$patch_id[i]
    rest <- patches_sorted$patch_id[-(1:i)]  # downstream patches
    # find distances to current patch
    dists <- raster::gridDistance(patches_raster, origin = current_patch)
    downstream <- patches_raster %in% rest
    downstream_dists <- dists[downstream]
    patches_ds <- patches_raster[downstream]

    # find location of minDist
    minLoc <- which.min(downstream_dists)
    patchNum <- patches_ds[minLoc]
    connectivity[i] <- patchNum
    if (!quiet) {
      cat("patch:", i, ", destination:", patchNum, "\n" )
    }

  }

  destination_patch <- c(connectivity, 0)
  output_df <- cbind(patches_sorted, destination_patch)

  # recode patch numbers to PCM standard
  # first destination must be the outlet
  newID <- seq(1, numPatches)
  newDests <- rep.int(0, numPatches)

  # sort by destination
  output_sorted <- output_df[order(output_df$destination_patch),]
  oldDests <- output_sorted$destination_patch
  oldID <- output_sorted$patch_id

  newDests[2:numPatches] <- newID[match(oldDests, oldID)][2:numPatches]

  output_df <- data.frame(newID, output_sorted$patch_el_mean,
                          output_sorted$patch_area,
                          output_sorted$patch_perimeter,
                          newDests)
  names(output_df) <- c("ID", "patch_el_mean", "patch_area",
                        "patch_perimeter", "destination")
  return(output_df)
}
