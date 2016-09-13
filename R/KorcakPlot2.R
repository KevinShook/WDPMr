#' Korkcak plot of 2 data sets
#'
#' @param data1 Required. Vector holding first data set.
#' @param name1 Required. Name of first data set.
#' @param data2 Required. Vector holding second data set.
#' @param name2 Required. Name of second data set.
#' @param minsize Optional. Minimum value to be plotted.
#' @param xbreaks Optional. Location of breaks in x-axis. Default is \code{c(10,1000,100000)}
#' @param ybreaks Optional. Location of breaks in y-axis. Default is \code{c(0.005,0.001,0.01,0.1,0.2,0.4,0.6,0.8,1)}.
#' @param lines Optional. Should fitted lines be plotted? Default is \code{TRUE}.
#'
#' @return Returns a \pkg{ggplot2} object of korcak plot.
#' @export
#'
#' @examples \dontrun{
#' p <- KorcakPlot2(data1, 'water', data2, 'bog')}
KorcakPlot2 <- function(data1, name1, data2, name2, minsize=0,
                        xbreaks=c(10,1000,100000),
                        ybreaks=c(0.005,0.001,0.01,0.1,0.2,0.4,0.6,0.8,1),
                        lines=TRUE){
  # compares and plots 2 data sets
  # declare variables
  p <- NULL
  name <- NULL
  value <- NULL

  d1.korcak=korcak(data1[data1>=minsize])
  d2.korcak=korcak(data2[data2>=minsize])


  d1.korcak <- subset(d1.korcak, p>0)
  d2.korcak <- subset(d2.korcak, p>0)

  d1.korcak=cbind(d1.korcak,rep.int(name1,nrow(d1.korcak)))
  d2.korcak=cbind(d2.korcak,rep.int(name2,nrow(d2.korcak)))

  names(d1.korcak)=c("value","p","name")
  names(d2.korcak)=c("value","p","name")

  all=rbind(d1.korcak, d2.korcak)

  p <- ggplot2::ggplot(all,ggplot2::aes(value,p,color=name,shape=name)) +
    ggplot2::scale_colour_manual(values = c("blue","red")) +
    ggplot2::geom_point() +
    ggplot2::xlab(expression(paste('Water area (m',''^{2}, ')', sep = ""))) +
    ggplot2::ylab("Exceedance probability") +
    ggplot2::scale_y_log10(breaks=ybreaks,labels=as.character(ybreaks)) +
    ggplot2::scale_x_log10(breaks=xbreaks,labels=as.character(xbreaks))

    if (lines)
      p <- p + ggplot2::stat_smooth(method = "lm",
                                    formula = y ~ x + I(x^2), size = 0.5, se = TRUE)

  return(p)

}
