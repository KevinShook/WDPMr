#' Korkcak plot of 2 data sets
#'
#' @param data1 Required. Vector holding first data set.
#' @param name1 Required. Name of first data set.
#' @param data2 Required. Vector holding second data set.
#' @param name2 Required. Name of second data set.
#' @param minsize Optional. Minimum value to be plotted.
#' @param xbreaks Optional. Location of breaks in x-axis. Default is \code{c(10,1000,100000)}
#' @param ybreaks Optional. Location of breaks in y-axis. Default is \code{c(0.005,0.001,0.01,0.1,0.2,0.4,0.6,0.8,1)}.
#' @param addLine Optional. Default is \option{none}. Other options are \option{linear}, \option{quadratic}, and \option{gpd}.
#' @param useLargest Optional. If \code{TRUE} then the largest value will be used, i.e. its non-exceedence probability willl be > 0. Default is \code{FALSE}.
#' @return Returns a \pkg{ggplot2} object of korcak plot.
#' @export
#'
#' @examples \dontrun{
#' p <- KorcakPlot2(data1, 'water', data2, 'bog')}
KorcakPlot2 <- function(data1, name1, data2, name2, minsize=0,
                        xbreaks=c(10,1000,100000),
                        ybreaks=c(0.005,0.001,0.01,0.1,0.2,0.4,0.6,0.8,1),
                        addLine="linear", useLargest=FALSE){
  # compares and plots 2 data sets
  # declare variables
  p <- NULL
  name <- NULL
  value <- NULL

  d1.korcak = korcak(data1[data1 >= minsize], useLargest = useLargest)
  d2.korcak = korcak(data2[data2 >= minsize], useLargest = useLargest)


  d1.korcak <- subset(d1.korcak, p > 0)
  d2.korcak <- subset(d2.korcak, p > 0)

  d1.korcak = cbind(d1.korcak,rep.int(name1,nrow(d1.korcak)))
  d2.korcak = cbind(d2.korcak,rep.int(name2,nrow(d2.korcak)))

  names(d1.korcak) = c("value","p","name")
  names(d2.korcak) = c("value","p","name")

  all = rbind(d1.korcak, d2.korcak)

  p <- ggplot2::ggplot(all, ggplot2::aes(value, p ,color = name,shape = name)) +
    ggplot2::scale_colour_manual(values = c("blue","red")) +
    ggplot2::geom_point() +
    ggplot2::xlab(expression(paste('Water area (m',''^{2}, ')', sep = ""))) +
    ggplot2::ylab("Exceedance probability") +
    ggplot2::scale_y_log10(breaks = ybreaks,labels = as.character(ybreaks)) +
    ggplot2::scale_x_log10(breaks = xbreaks,labels = as.character(xbreaks))


  if (addLine == "quadratic")
    p <- p + ggplot2::stat_smooth(method = "lm", formula = y ~ x + I(x^2),
                                  size = 0.5, se = TRUE)

  if (addLine == "linear")
    p <- p + ggplot2::stat_smooth(method = "lm", formula = y ~ x,
                                  size = 0.5, se = TRUE)

  if (addLine == "gpd") {
    # fit gpd to distribution
    # fit GPD
    gpdfit1 <- ismev::gpd.fit(data1, minsize, show = FALSE)
    beta1 <- gpdfit1$mle[1]
    xi1 <- gpdfit1$mle[2]

    gpdfit2 <- ismev::gpd.fit(data2, minsize, show = FALSE)
    beta2 <- gpdfit2$mle[1]
    xi2 <- gpdfit2$mle[2]

    # create plotting points
    # generate random values
    r1 <- fExtremes::rgpd(1e5, xi = xi1, mu = minsize, beta = beta1)
    # trim to min and max from data1
    r1 <- r1[(r1 >= min(data1)) & (r1 <= max(data1))]

    # get exceedence probability
    exProb1 <- 1 - fExtremes::pgpd(r1, xi = xi1, mu = minsize, beta = beta1)
    gpd1 <- data.frame(as.numeric(r1), as.numeric(exProb1), name1)
    names(gpd1) <- c("value","p","name")

    r2 <- fExtremes::rgpd(1e5, xi = xi2, mu = minsize, beta = beta2)
    # trim to min and max from data1
    r2 <- r2[(r2 >= min(data2)) & (r2 <= max(data2))]

    # get exceedence probability
    exProb2 <- 1 - fExtremes::pgpd(r2, xi = xi2, mu = minsize, beta = beta2)
    gpd2 <- data.frame(as.numeric(r2), as.numeric(exProb2), name2)

    names(gpd2) <- c("value","p","name")
    allGPD <- rbind(gpd1, gpd2)
    p <- p + ggplot2::geom_line(data = allGPD,
                                ggplot2::aes(value, p, colour = name))


  }

  return(p)

}
