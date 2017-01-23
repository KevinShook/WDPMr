combinedContribFracLoop <- function(infile, outfile, ylabel='Connected fraction'){
  # plots all contrib fraction loops on a single graph
  # declare plot variables
  xstart <- NULL
  xend <- NULL
  ystart <- NULL
  yend <- NULL
  Operation <- NULL

  all.segs <- utils::read.csv(file=infile, stringsAsFactors=FALSE, header=TRUE)

  # order plots by resolution
  all.segs$Resolution <- ordered(all.segs$Resolution ,
                                 levels = c('10m', '50m', '100m'))

  p <- ggplot2::ggplot() + ggplot2::scale_colour_manual(values = c("red","blue")) +
    ggplot2::xlab('Fractional water volume') +
    ggplot2::ylab(ylabel) +
    ggplot2::coord_equal(ratio = 1) +
    ggplot2::geom_segment(data=all.segs,
                          ggplot2::aes(xstart,ystart,xend=xend,yend=yend,
                                       colour=Operation),
                 arrow=ggplot2::arrow(length=unit(3,"mm"))) +
    ggplot2::facet_grid(. ~ Resolution) +
    ggplot2::theme_gray(18) +
    ggplot2::theme(panel.margin.x = unit(1, "lines"))

  ggplot2::ggsave(filename=outfile, width=12, height=5)

}
