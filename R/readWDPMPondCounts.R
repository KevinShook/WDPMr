readWDPMPondCounts <- function(infile){

  WDPMSummary <- utils::read.csv(infile, header=FALSE, stringsAsFactors = FALSE, skip=1)
  names(WDPMSummary) <- c('Filename','patch.area', 'water.area','water.volume', 'water.patches')

  # parse file names
  f <- strsplit(WDPMSummary$Filename, '_', fixed=TRUE)
  WDPMSummary$add1 <- as.numeric(unlist((lapply(f, "[[", 1))))
  WDPMSummary$subtract1 <- as.numeric(unlist((lapply(f, "[[", 2))))
  WDPMSummary$add2 <- as.numeric(unlist((lapply(f, "[[", 3))))
  WDPMSummary$subtract2 <- as.numeric(unlist((lapply(f, "[[", 4))))
  WDPMSummary$type <- unlist((lapply(f, "[[", 5)))
  # order by addition and subtraction
  WDPMSummary <- WDPMSummary[order(WDPMSummary$add1, WDPMSummary$subtract1, WDPMSummary$add2, WDPMSummary$subtract2),]
  return(WDPMSummary)
}
