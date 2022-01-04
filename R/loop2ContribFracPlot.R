#' Plots loop2 contributing fraction hysteresis
#'
#' @param orderedAreaVol Required. A set of ordered areas and volumes as produced by \code{volumeAreaSequence}
#' @param extend Optional. Should curves extend to origin? Default is \code{FALSE}.
#' @param normalize Optional. Should areas and volumes be divided by their maximum values? Default is \code{TRUE}.
#' @param outfile Optional. File name for output of line segments. If not specified (the default), the data are not ouput.
#' @param N Optional. Plot arrows every Nth point. Default is 1.
#'
#' @return Returns a ggplot2 object of the area vs volume. The line segments are written to a file (if specified).
#' @export
#'
#' @examples \dontrun{
#' p <- loop2contribFracPlot(areavol)}
loop2ContribFracPlot <- function(orderedAreaVol, extend=FALSE, normalize=TRUE,  outfile='', N=1){
  # reads WDPM areas and volumes and plots contributing fraction loop
  # declare variables
  add1 <- NULL
  subtract1 <- NULL
  add2 <- NULL
  subtract2 <- NULL
  add3 <- NULL
  subtract3 <- NULL

  curve1.volfrac <- c(0)
  curve2.volfrac <- c(0)
  curve1.contribfrac <- c(0)
  curve2.contribfrac <- c(0)

  # get contributing fractions
  vol.max <- max(orderedAreaVol$water.volume)

  # now get changes in volume
  # select additions of 1mm to existing water
  added <- orderedAreaVol[ (orderedAreaVol$add3 > 0), ]
  rest <- orderedAreaVol[ (orderedAreaVol$add3 == 0), ]
  max.volume <- max(orderedAreaVol$water.volume)
  added.count <- nrow(added)

  j <- 0
  k <- 0
  for (i in 1:added.count){
    final <- added[i,]
    final.volume <- final$water.volume
    volfrac <- final.volume / max.volume
    others <- subset(rest, (add1 == final$add1) & (subtract1 == final$subtract1) &
                       (subtract2 == final$subtract2) & (add2 == final$add2))
    initial <- others[which.min(others$water.volume),]
    initial.volume <- initial$water.volume
    volume.added <- (final$add3 * final$basin.area) / 1000
    delta.volume  <- final.volume - initial.volume
    rof <- 1 - (delta.volume / volume.added)

    if (final$subtract2 > 0 ){
      j <- j + 1
      curve2.volfrac[j]<- volfrac
      curve2.contribfrac[j] <- rof
    }
    else{
      k <- k + 1
      curve1.volfrac[k] <- volfrac
      curve1.contribfrac[k] <- rof
    }
  }


  curve1 <- data.frame(curve1.volfrac, curve1.contribfrac)
  curve2 <- data.frame(curve2.volfrac, curve2.contribfrac)
  p <- contributingAreaHysteresisPlot(curve1, curve2, extend=extend, normalize=TRUE, outfile='', N=1)

  AV.hysteresis <- hysteresis(curve1, curve2, TRUE)
  cat('Area volume hysteresis = ', AV.hysteresis,'\n', sep='')

  p <- contributingAreaHysteresisPlot(curve1, curve2, extend=FALSE,
                                      normalize=normalize, outfile=outfile, N=N)

  return(p)
}
