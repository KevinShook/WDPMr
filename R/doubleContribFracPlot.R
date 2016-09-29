#' Plots 2 sets of contributing fraction hysteresis curves
#'
#' @param curveFile1 Required. File containing first set of curves.
#' @param name1 Required. Name of first set of curves.
#' @param curveFile2 Required. File containing second set of curves.
#' @param name2 Required. Name of second set of curves.
#'
#' @return Returns \pkg{ggplot2} object of both sets of curves
#' @export
#'
#' @examples \dontrun{
#' p <- doubleContribFracPlot('Run1_loop1.csv', 'PCM', 'WDPM_loop1.csv', 'WDPM')}
doubleContribFracPlot <- function(curveFile1, name1, curveFile2, name2){
  xstart <- NULL
  xend <- NULL
  ystart <- NULL
  yend <- NULL
  Operation <- NULL
  name <- NULL

  # read curves
  curves1 <- utils::read.csv(curveFile1, header=TRUE, stringsAsFactors = FALSE)
  curves1 <- curves1[,c('xstart','xend','ystart','yend','Operation')]
  curves1$name <- name1
  

  curves2 <- utils::read.csv(curveFile2, header=TRUE, stringsAsFactors = FALSE)
  curves2 <- curves2[,c('xstart','xend','ystart','yend','Operation')]
  curves2$name <- name2

  # combine
  curves <- rbind(curves1, curves2)

  # now plot

   p <- ggplot2::ggplot() + ggplot2::scale_colour_manual(values = c("red","blue")) +
    ggplot2::xlab('Fractional water volume') +
    ggplot2::ylab('Fractional contributing area') +
    ggplot2::coord_equal(ratio = 1) +
    ggplot2::geom_segment(data=curves,
                          ggplot2::aes(xstart,ystart,xend=xend,yend=yend,
                                       colour=Operation, linetype=name),
                          arrow=ggplot2::arrow(length=ggplot2::unit(3,"mm"))) +
    ggplot2::theme_gray(18) +
    ggplot2::theme(legend.title=ggplot2::element_blank())

   return(p)


}
