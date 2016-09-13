loop1PondCountPlot <- function(infile){
  # plots number of wetlands
  add1 <- NULL
  subtract1 <- NULL
  add2 <- NULL
  subtract2 <- NULL

  WDPM <- readWDPMPondCounts(infile)
  f <- strsplit(infile, '.', fixed=TRUE)
  graphfile <- paste(unlist(f[[1]][1]),'_Loop1PondCount.pdf', sep='')
  curve1 <- subset(WDPM, (add1 > 0) & (subtract1 == 0) & (add2 == 0) & (subtract2 == 0))
  curve2 <- subset(WDPM, (add1 > 0) & (subtract1 > 0) & (add2 == 0) & (subtract2 == 0))

  # check for small additions and subtractions and remove them
  curve1.diff <- c(10,diff(curve1$add1))
  curve2.diff <- c(10,diff(curve2$subtract1))

  curve1 <-curve1[curve1.diff>5,]
  curve2 <-curve2[curve2.diff>5,]

  curve1 <- curve1[,c('water.volume','water.patches')]
  curve2 <- curve2[,c('water.volume','water.patches')]

  pondCountHysteresisPlot(curve1, curve2, TRUE, TRUE, graphfile, 1)

}
