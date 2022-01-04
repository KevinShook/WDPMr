#' Plots Loop1 contributing fraction for PCM output
#'
#' @param PCMROPfile Required. File containing PCM output.
#' @param extend Optional. Should curves extend to origin? Default is \code{TRUE}.
#' @param normalize Optional. Should areas and volumes be divided by their maximum values? Default is \code{TRUE}.
#' @param outfile Optional. File name for output of line segments. If not specified (the default), the data are not ouput.
#' @param N Optional. Plot arrows every Nth point. Default is 1.
#'
#' @return Returns a ggplot2 object of the area vs volume. The line segments are written to a file (if specified).
#' @export
#'
#' @examples \dontrun{
#' p <- plotPCMcontribLoop1('AreaVolumeROP_Loop1.txt')}
plotPCMcontribLoop1<- function(PCMROPfile, extend=TRUE, normalize=TRUE, outfile='', N=1){
  # get PCM output
  ROP <- utils::read.table(PCMROPfile, stringsAsFactors = FALSE, header=TRUE)

  # separate into filling and emptying
  delta <- diff(ROP$final_vol)
  delta <- c(0, delta)

  filling <- ROP[delta >= 0,]
  emptying <- ROP[delta < 0,]

  curve1 <- filling[,c('volfrac', 'rof')]
  curve2 <- emptying[,c('volfrac', 'rof')]

  # plot curves
  p <- contributingAreaHysteresisPlot(curve1, curve2, extend=extend,
                                      normalize=normalize, outfile=outfile, N=N)
  return(p)

}
