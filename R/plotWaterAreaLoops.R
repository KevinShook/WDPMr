plotWaterAreaLoops <- function(infiles, names, graphfile){
  # reads Korcak plot 2nd order polynomial fits
  # and plots loops of water areas
  # declare variables
  volmaxfrac <- NULL
  patchfrac <- NULL
  site <- NULL

  NumFiles <- length(infiles)
  if (outpath == '')
    outpath <- getwd()

  for (i in 1:NumFiles){
    infile <- infiles[i]
    name <- names[i]

    korcak <- utils::read.csv(infile, header=TRUE, fill=TRUE)
    names(korcak) <- c(names(korcak)[-1],'NULL')
    max.vol <- pmax(korcak$vol)
    max.patchcount <- pmax(korcak$patchcount)
    korcak$volmaxfrac <- korcak$vol / max.vol
    korcak$patchfrac <- korcak$patchcount / max.patchcount

    if (i == 1)
      all <- korcak
    else
      all <- rbind(all, korcak)
  }

  melted <- melt(all, id.vars=c('volmaxfrac', 'site'),
                 measure.vars=c('patchfrac'))
  names(melted) <- c('volmaxfrac', 'site', 'parameter', 'value')

  p1 <- ggplot2::ggplot(melted, ggplot2::aes(volmaxfrac, patchfrac, colour=site)) +
    ggplot2::geom_path() + ggplot2::xlim(0,1) + ggplot2::ylim(0,1) +
    ggplot2::xlab('Fraction of max water volume') +
    ggplot2::ylab('Fraction of max number of water areas') +
    ggplot2::theme_gray(14)

  return(p1)

}
