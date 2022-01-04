#' Finds centroid of largest water area
#'
#' @param water_file Required. WDPM output.
#' @param threshold Optional. Threshold for delineating water. Default is 0.001 m.
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' centroid <- findLargestPondCentroid("100_0_0_0_d.asc")
#' }
findLargestPondCentroid <- function(water_file, threshold = 0.001) {

  # get water patches
  patches <- getWaterPatches(water_file, threshold = 0.001)
  patch_stats <- measureWaterPatches(water_file, threshold = 0.001)
  largest_patch_num <- which.max(patch_stats$area)

  patchBinary <- patches
  patchBinary[patches != largest_patch_num] <- NA_real_

  centroid <- SDMTools::COGravity(patchBinary)
  return(centroid)
}
