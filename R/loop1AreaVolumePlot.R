#' Plots loop1 water area-volume hysteresis
#'
#' @param orderedVA Required. Data frame containing WDPM run total water volumes and areas, as returned by \code{volumeAreaSequence}, for the initial loop (complete filling, followed by complete emptying).
#' @param outfile Optional. File to hold hysteresis calculated using \code{hysteresis}. If file name is not given, then the hysteresis is output to the screen.
#'
#' @return If successful, returns ggplot2 object of area-volume hysteresis. If unsuccessful, returns \code{FALSE}.
#' @export
#'
#' @examples \dontrun{
#' p <- loop1AreaVolumePlot}
loop1AreaVolumePlot <- function(orderedVA, outfile=''){
  # declare plot variables
  add1 <- NULL
  subtract1 <- NULL
  add2 <- NULL
  subtract2 <- NULL
  water.volume <- NULL
  water.area <- NULL

  if(is.null(orderedVA)){
    cat('Error: missing values\n')
    return(FALSE)
  }

  curve1 <- subset(orderedVA, (add1 > 0) & (subtract1 == 0) & (add2 == 0) &
                     (subtract2 == 0), select=c(water.volume, water.area))
  curve2 <- subset(orderedVA, (add1 > 0) & (subtract1 > 0) & (add2 == 0) &
                     (subtract2 == 0), select=c(water.volume, water.area))

  p <- areaVolumeHysteresisPlot(curve1, curve2, TRUE, TRUE, outfile, 1)

  AV.hysteresis <- hysteresis(curve1, curve2, TRUE)

  if(outfile != '')
    cat('Area volume hysteresis = ', AV.hysteresis,'\n', sep='', file=outfile)
  else
    cat('Area volume hysteresis = ', AV.hysteresis,'\n', sep='')
  return(p)

}
