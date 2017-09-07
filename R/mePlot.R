#' Creates mean excess function plot
#'
#' @param data Required. Vector to plot.
#' @param cut Optional. The number of maxima to exclude. The default value is 5
#'
#' @return  Returns \pkg{ggplot2} object.
#' @export
#'
#' @examples \dontrun{
#' p <- mePlot(x)}
#'
#' @references From Cirillo, P. (2013), Are your data really Pareto distributed?, Phys. A Stat. Mech. its Appl., 392(23), 5947-5962, doi:10.1016/j.physa.2013.07.061.
#'
mePlot <- function(data, cut = 5) {
  # In cut you can specify the number of maxima you want to exclude.
  # The standard value is 5
  data <- sort(as.numeric(data))
  n <- length(data)
  mex <- c()
  for (i in 1:n) {
    mex[i] <- mean(data[data > data[i]]) - data[i]
  }
  data_out <- data[1:(n - cut)]
  mex_out <- mex[1:(n - cut)]

  # assemble dataframe
  all <- data.frame(data_out, mex_out)
  p <- ggplot2::ggplot(all, ggplot2::aes(data_out,mex_out)) +
    ggplot2::geom_point() +
    ggplot2::xlab('Threshold u') +
    ggplot2::ylab('Mean Excess e(u)')

  return(p)

}
