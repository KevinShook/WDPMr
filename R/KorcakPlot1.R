#' Korcak plot of 1 data set of water areas
#'
#' @param areas Required. Vector of water areas.
#' @param minval Optional. Minimum water area to be plotted. Default is 100 m2.
#' @param addLine Optional. Default is \option{none}. Other options are \option{linear}, \option{quadratic}, \option{gpd} and
#' \option{pareto2}.
#' @param lineColour Optional. Colour to be used for the fitted line. Default is \option{red}.
#' @param useLargest Optional. If \code{TRUE} then the largest value will be used, i.e. its non-exceedence probability willl be > 0. Default is \code{FALSE}.
#' @return Returns \pkg{ggplot2} object of Korcak plot.
#' @export
#'
#' @examples \dontrun{
#' p <- KorcakPlot1(alldata$areas)}
KorcakPlot1 <- function(areas, minval=100, addLine="none", lineColour="red", useLargest=FALSE){
  addLine <- stringr::str_to_lower(addLine)
  # declare ggplot vars
  p <- NULL
  value <- NULL

  # plots 1 data set

  d1.korcak <- korcak(areas[areas >= minval], useLargest = useLargest)
  d1.korcak <- d1.korcak[d1.korcak$p > 0,]

  names(d1.korcak) <- c("value","p")
  all <- d1.korcak
  ybreaks <- c(0.005,0.001,0.01,0.1,0.2,0.4,0.6,0.8,1)
  xbreaks <- c(100,1000,10000)

  p <- ggplot2::ggplot(all, ggplot2::aes(value, p)) +
    ggplot2::geom_point() +
    ggplot2::xlab(expression(paste('Water area (m',''^{2}, ')', sep = ""))) +
    ggplot2::ylab("Exceedance probability") +
    ggplot2::scale_y_log10(breaks = ybreaks,labels = ybreaks) +
    ggplot2::scale_x_log10(breaks = xbreaks,labels = xbreaks)

  if (addLine == "quadratic")
    p <- p + ggplot2::stat_smooth(method = "lm", formula = y ~ x + I(x^2),
                                  size = 0.5, se = TRUE, colour = lineColour)

  if (addLine == "linear")
    p <- p + ggplot2::stat_smooth(method = "lm", formula = y ~ x,
                                  size = 0.5, se = TRUE, colour = lineColour)

  if (addLine == "gpd") {
    # fit gpd to distribution
    # fit GPD
    gpdfit <- ismev::gpd.fit(areas, minval, show = FALSE)
    beta <- gpdfit$mle[1]
    xi <- gpdfit$mle[2]

    # create plotting points
    # generate random values
    r <- fExtremes::rgpd(1e5, xi = xi, mu = minval, beta = beta)
    # get exceedence probability
    exProb <- 1 - fExtremes::pgpd(r, xi = xi, mu = minval, beta = beta)
    gpd <- data.frame(r, exProb)
    p <- p + ggplot2::geom_line(data = gpd, ggplot2::aes(r, exProb),
                                colour = lineColour)

  }

  if (addLine == "pareto2") {
    # fit gpd to distribution
    # fit GPD
    paretofit <- fitdistrplus::fitdist(SDB_ponds$area, "pareto2", method = "mge",
                      start = list(shape = 1, scale = 300), gof = "ADR")
    shape <- paretofit$estimate[1]
    scale <- paretofit$estimate[2]


    # create plotting points
    # generate random values
    r <- actuar::rpareto(50000, shape = shape, scale = scale)
    # get exceedence probability
    exProb <- 1 - actuar::ppareto(r, shape = shape, scale = scale)
    pareto2 <- data.frame(r, exProb)
    p <- p + ggplot2::geom_line(data = pareto2, ggplot2::aes(r, exProb),
                                colour = lineColour)

  }

  return(p)
}
