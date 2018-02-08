#' Outputs file of rising limb runoff fraction and volume.
#'
#' @param sortedAreaVols Required. Set of areas and volumes sorted from smallest to largest.
#' @param extend Optional. Logical. Should plots be extended to (0,0) and (1,1)? Default is \code{FALSE}.
#' @param outFile Required. CSV file for output.
#'
#' @return Returns rising limb contributing fraction plot.
#' @export
#'
#' @examples \dontrun{p <- plotRisingLimbROF(df, "RisingLimb.csv")
#' }
plotRisingLimbROF <- function(sortedAreaVols='', extend=FALSE, outFile=""){
  tempOutfile <- "./tempFile"
  # get contrib area fractions
  p <- loop1ContribFracPlot(orderedAreaVol = sortedAreaVols, extend = extend, outfile = tempOutfile)

  # read in file and extract rising limb values
  segments <- utils::read.csv(tempOutfile, stringsAsFactors = FALSE, header = TRUE)
  file.remove(tempOutfile)
  # select rising limb
  segments <- segments[segments$Operation == 'filling',]

  # get segments
  rising <- segments[,c('xstart', 'ystart')]

  # get final destintation
  numRows <- nrow(rising)
  lastRow <- segments[numRows, c('xend', 'yend')]
  names(lastRow) <- c('xstart', 'ystart')
  if (!is.na(lastRow$xstart[1]) & !is.na(lastRow$ystart[1]) )
    rising <- rbind(rising, lastRow)

  # write file
  if (outFile != "")
    utils::write.csv(rising, file = outFile, row.names = FALSE)
  return(p)
}
