#' Compares and plots 2 data sets of water patches
#' @description  Only plots patches bigger than 100m x 100m, i.e. 10,000 m2.
#'
#' @param data1 Required. Vector of first set of water areas.
#' @param name1 Required. String of name of first data set.
#' @param data2 Required. Vector of second set of water areas.
#' @param name2 Required. String of name of second data set.
#'
#' @return Returns \pkg{ggplot2} object containing plot.
#' @export
#'
#' @examples \dontrun{
#' p <- KorcakPlot2_100m(data1, 'set1', data2, 'set2')}
KorcakPlot2_100m <- function(data1, name1, data2, name2){
  # declare ggplot2 variables
  value <- NULL
  p <- NULL
  Scale <- NULL

  d1.korcak <- korcak(data1[data1>=10000])
  d2.korcak <- korcak(data2[data2>=10000])


  d1.korcak <- subset(d1.korcak, p>0)
  d2.korcak <- subset(d2.korcak, p>0)

  d1.korcak=cbind(d1.korcak,rep.int(name1,nrow(d1.korcak)))
  d2.korcak=cbind(d2.korcak,rep.int(name2,nrow(d2.korcak)))

  names(d1.korcak)=c("value","p","Scale")
  names(d2.korcak)=c("value","p","Scale")

  all=rbind(d1.korcak, d2.korcak)
  ybreaks=c(0.005,0.001,0.01,0.1,0.2,0.4,0.6,0.8,1)
  xbreaks=c(1000,10000,100000)


  p <- ggplot2::ggplot(all,aes(value,p,color=Scale,shape=Scale)) +
    ggplot2::scale_colour_manual(values = c("blue","red")) +
    ggplot2::geom_point() +
    ggplot2::xlab(expression(paste('Water area (m',''^{2}, ')', sep = ""))) +
    ggplot2::ylab("Exceedance probability") +
    ggplot2::scale_y_log10(breaks=ybreaks,labels=ybreaks) +
    ggplot2::scale_x_log10(breaks=xbreaks,labels=xbreaks) +
    ggplot2::stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 0.5, se = TRUE)

  return(p)

}
