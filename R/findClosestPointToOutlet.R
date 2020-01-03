#' Finds minumum distance to outet from pond
#'
#' @param water_file Required. WDPM output.
#' @param outletXY Required. Vector containing outlet X and Y coordinates. Can be
#' the output of the command \code{findOutlet}.
#'
#' @return Returns the minimum distance from the the pond divide to the outlet.
#' @author Kevin Shook
#' @seealso \code{\link{findOutlet}}
#' @export
#'
#' @examples \dontrun{
#' outlet <- findOutlet("basin5.asc")
#' min_distance <- findClosestPointToOutlet("100_0_0_0_d.asc", outlet$pourXY)
#' }
findClosestPointToOutlet <- function(water_file, outletXY){
  # get water patches
  patches <- getWaterPatches(water_file, threshold = 0.001)
  patch_stats <- measureWaterPatches(water_file, threshold = 0.001)
  largest_patch_num <- which.max(patch_stats$area)

  patchBinary <- patches
  patchBinary[patches != largest_patch_num] <- NA_real_
  largest <- raster(patchBinary)
  divide <- raster::boundaries(largest, type = "inner", classes = FALSE,
                               directions = 8, asNA = TRUE)

  distanceFromOutlet <- raster::distanceFromPoints(divide, outletXY)
  distanceFromOutlet <- distanceFromOutlet * divide
  min_dist <- raster::minValue(distanceFromOutlet)

  return(min_dist)
}
