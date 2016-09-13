#' Gets water patches from an ArcGIS .asc file
#'
#' @description The purpose is to find water patches in an .asc file. The patch numbers can be used to determine the drainage basin areas.
#' @param infile Required. ArcGIS .asc file of water depths, as output by WDPM.
#' @param threshold Optional. Threshold (in m) to determine the existence of water. The default value is 0.001 m, i.e. 1 mm.
#'
#' @return Returns an SDMTools object containing the numbered water patches.
#' @export
#'
#' @examples \dontrun{
#' patches <- getWaterPatches('10_0_0_0_d.asc')}
getWaterPatches <- function(infile, threshold=0.001){
  asc  <-  SDMTools::read.asc(infile)
  cellsize <- attr(asc, "cellsize")
  a  <-  strsplit(infile,".asc")
  waterfile  <-  a[[1]][1]

  # make binary
  asc.binary <- asc
  asc.binary[asc.binary>0]<-1

  # get patch connectivity
  ccl <- SDMTools::ConnCompLabel(asc.binary)

  # return patches
  return(ccl)
}
