#' Korcak plot of 1 data set of water areas
#'
#' @param areas Required. Vector of water areas.
#' @param minval Optional. Minimum water area to be plotted. Default is 100 m2.
#'
#' @return Returns \pkg{ggplot2} object of Korcak plot.
#' @export
#'
#' @examples \dontrun{
#' p <- KorcakPlot1(alldata$areas)}
KorcakPlot1 <- function(areas, minval=100){
  # declare ggplot vars
  p <- NULL
  value <- NULL

  # plots 1 data set

  d1.korcak <- korcak(areas[areas >= minval])
  d1.korcak <- d1.korcak[d1.korcak$p > 0,]

  names(d1.korcak) <- c("value","p")
  all <- d1.korcak
  ybreaks <- c(0.005,0.001,0.01,0.1,0.2,0.4,0.6,0.8,1)
  xbreaks <- c(100,1000,10000)

  p <- ggplot2::ggplot(all, ggplot2::aes(value, p)) +
    ggplot2::geom_point() +
    ggplot2::xlab(expression(paste('Water area (m',''^{2}, ')', sep = ""))) +
    ggplot2::ylab("Exceedance probability") +
    ggplot2::scale_y_log10(breaks=ybreaks,labels=ybreaks) +
    ggplot2::scale_x_log10(breaks=xbreaks,labels=xbreaks) +
    ggplot2::stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 0.5, se = TRUE)

  return(p)

}
