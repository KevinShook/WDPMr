loop2ReadWDPMVolumesAreas <- function(infile){
  # reads summary created by WDPMAreaVolume
  # for loop2 processing
  # declare plot vars
  add1 <- NULL
  subtract1 <- NULL
  add2 <- NULL
  subtract2 <- NULL
  add3 <- NULL


  WDPMSummary <- utils::read.csv(infile, header=TRUE, stringsAsFactors = FALSE)
  # parse file names
  f <- strsplit(WDPMSummary$file, '_', fixed=TRUE)
  WDPMSummary$add1 <- as.numeric(unlist((lapply(f, "[[", 1))))
  WDPMSummary$subtract1 <- as.numeric(unlist((lapply(f, "[[", 2))))
  WDPMSummary$add2 <- as.numeric(unlist((lapply(f, "[[", 3))))
  WDPMSummary$subtract2 <- as.numeric(unlist((lapply(f, "[[", 4))))
  WDPMSummary$type <- unlist((lapply(f, "[[", 5)))

  # find file names for loop 2
  WDPMSummary$add3 <- NA_real_

  if (length(f[[1]]) >= 5 ){
    WDPMSummary$add3 <- as.numeric(unlist((lapply(f, "[[", 5))))
  }

  # omit all with missing values
  WDPMSummary <- stats::na.omit(WDPMSummary)

  # order by addition and subtraction
  WDPMSummary <- WDPMSummary[order(WDPMSummary$add1, WDPMSummary$subtract1, WDPMSummary$add2, WDPMSummary$subtract2),]
  return(WDPMSummary)
}
