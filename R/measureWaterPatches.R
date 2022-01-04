#' Measures water patches in ArcGIS .asc files
#'
#' @description finds the statitics describing water patches in an ArcGIS .asc file.
#' @param infile Required. ArcGIS .asc file of water depths, as output by WDPM.
#' @param threshold Optional. Threshold (in m) to determine the existence of water. The default value is 0.001 m, i.e. 1 mm.
#'
#' @return Returns a data frame containing the statistics for each numbered patch
#' @export
#'
#' @examples \dontrun{
#' patches <- measureWaterPatches('10_0_0_0_d.asc')}
measureWaterPatches <- function(infile, threshold=0.001){
  asc  <-  SDMTools::read.asc(infile)
  cellsize <- attr(asc, "cellsize")

  # make binary
  asc.binary <- asc
  asc.binary[asc.binary <= threshold] <- NA_real_
  asc.binary[asc.binary > threshold] <- 1

  # get patch connectivity
  ccl <- SDMTools::ConnCompLabel(asc.binary)

  # get patch stats - omit unfilled region
  patches <- SDMTools::PatchStat(ccl, cellsize = cellsize)
  patches <- patches[patches$patchID > 0,]

  # return patch statistics
  return(patches)
}
