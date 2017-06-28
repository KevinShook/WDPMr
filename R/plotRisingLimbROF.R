plotRisingLimbROF <- function(sortedAreaVols='', extend=FALSE){

  # get contrib area fractions



  p <- loop1ContribFracPlot(orderedAreaVol = sortedAreaVol, extend = extend, outfile = tempOutfile)

  # read in file and extract rising limb values
  segments <- read.csv(tempOutfile, stringsAsFactors = FALSE, header = TRUE)
  file.remove(tempOutfile)
  # select rising limb
  segments <- segments[segments$Operation == 'filling',]

  # get segments
  rising <- segments[,c('xstart', 'ystart')]

  # get final destintation
  numRows <- nrow(rising)
  lastRow <- segments[numRows, c('xend', 'yend')]
  names(lastRow) <- c('xstart', 'ystart')
  if(!is.na(lastRow$xstart[1]) & !is.na(lastRow$ystart[1]) )
    rising <- rbind(rising, lastRow)

  # write file
  write.csv(rising, file=outFile, row.names = FALSE)
}
