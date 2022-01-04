plotKorkcakLoops <- function(infiles, names, outpath=''){
  # reads Korcak plot 2nd order polynomial fits
  # and plots loops
  # declare variab;es
  volmaxfrac <- NULL
  value <- NULL
  site <- NULL
  areamaxfrac <- NULL
  basinareafrac <- NULL
  const <- NULL
  linear <- NULL
  linear <- NULL
  quadratic <- NULL

  NumFiles <- length(infiles)
  if (outpath == '')
    outpath <- getwd()

  for (i in 1:NumFiles){
    infile <- infiles[i]
    name <- names[i]

    korcak <- utils::read.csv(infile, header=TRUE, fill=TRUE)
    names(korcak) <- c(names(korcak)[-1],'NULL')
    max.vol <- pmax(korcak$vol)
    max.area <- pmax(korcak$area)
    korcak$volmaxfrac <- korcak$vol / max.vol
    korcak$areamaxfrac <- korcak$area / max.area
    korcak$basinareafrac <- korcak$area / korcak$maxarea
    korcak$site <- name

    if (i == 1)
      all <- korcak
    else
      all <- rbind(all, korcak)
  }

  # melt
  melted <- melt(all, id.vars=c('volmaxfrac', 'areamaxfrac', 'basinareafrac', 'site'),
                 measure.vars=c('const', 'linear', 'quadratic'))
  names(melted) <- c('volmaxfrac', 'areamaxfrac', 'basinareafrac', 'site', 'parameter', 'value')

  p1 <- ggplot2::ggplot(melted, ggplot2::aes(volmaxfrac, value, colour=site)) +
    ggplot2::geom_path() + ggplot2::xlim(0,1) + ggplot2::xlab('Fraction of max. volume') +
    ggplot2::ylab('Parameter value') +
    ggplot2::facet_grid(. ~ parameter, scales = "free", space="free") +
    ggplot2::theme_gray(14)
  ggsave(paste(outpath,'/MaxVolFrac.pdf',sep=''), width=8, height=4)
  ggsave(paste(outpath,'/MaxVolFrac.png', sep=''), width=8, height=4)

  p2 <-ggplot2::ggplot(melted, ggplot2::aes(areamaxfrac, value, colour=site)) +
    ggplot2::geom_path() + ggplot2::xlim(0,1) + ggplot2::xlab('Fraction of max. area') +
    ggplot2::ylab('Parameter value') +
    ggplot2::facet_grid(. ~ parameter, scales = "free", space="free") +
    ggplot2::theme_gray(14)
  ggsave(paste(outpath,'/MaxAreaFrac.pdf',sep=''), width=8, height=4)
  ggsave(paste(outpath,'/MaxAreaFrac.png', sep=''), width=8, height=4)

  p3 <- ggplot2::ggplot(melted, ggplot2::aes(basinareafrac, value, colour=site)) +
    ggplot2::geom_path() + ggplot2::xlab('Fraction of basin area') +
    ggplot2::ylab('Parameter value') +
    ggplot2::facet_grid(. ~ parameter, scales = "free", space="free") +
    ggplot2::theme_gray(14)
  ggsave(paste(outpath,'/BasinAreaFrac.pdf',sep=''), width=8, height=4)
  ggsave(paste(outpath,'/BasinAreaFrac.png', sep=''), width=8, height=4)

  # do individual plots
  p4 <- ggplot2::ggplot(all, ggplot2::aes(volmaxfrac, const, colour=site)) +
    ggplot2::geom_path() + ggplot2::xlim(0,1) + ggplot2::xlab('Fraction of max. volume') +
    ggplot2::ylab('Constant parameter') +
    ggplot2::theme_gray(14)
  ggsave(paste(outpath,'/MaxVolFracConst.pdf',sep=''), width=8, height=4)
  ggsave(paste(outpath,'/MaxVolFracConst.png',sep=''), width=8, height=4)

  p5 <- ggplot2::ggplot(all, ggplot2::aes(volmaxfrac, linear, colour=site)) +
    ggplot2::geom_path() + ggplot2::xlim(0,1) + ggplot2::xlab('Fraction of max. volume') +
    ggplot2::ylab('Linear parameter') +
    ggplot2::theme_gray(14)

  ggsave(paste(outpath,'/MaxVolFracLinear.pdf',sep=''), width=8, height=4)
  ggsave(paste(outpath,'/MaxVolFracLinear.png',sep=''), width=8, height=4)

  p6 <- ggplot2::ggplot(all, ggplot2::aes(volmaxfrac, quadratic, colour=site)) +
    ggplot2::geom_path() + ggplot2::xlim(0,1) + ggplot2::xlab('Fraction of max. volume') +
    ggplot2::ylab('Quadratic parameter') +
    ggplot2::theme_gray(14)
  ggsave(paste(outpath,'/MaxVolFracQuadratic.pdf',sep=''), width=8, height=4)
  ggsave(paste(outpath,'/MaxVolFracQuadraic.png',sep=''), width=8, height=4)

}

