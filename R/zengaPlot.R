#' Create Zenga plot used to examine if data truly fit a Pareto distribution.
#'
#' @param data Required. Vector of values to be plotted
#'
#' @return Returns \pkg{ggplot2} object.
#' @export
#'
#' @examples \dontrun{
#' p <- zengaPlot(x)}
#' @references From Cirillo, P. (2013), Are your data really Pareto distributed?, Phys. A Stat. Mech. its Appl., 392(23), 5947-5962, doi:10.1016/j.physa.2013.07.061.
zengaPlot <- function(data){
  # Since the code relies on the Lorenz curve
  #as computed by the "ineq" library,
  # we upload it library(ineq) #
  # Empirical Lorenz
  est <- ineq::Lc(data)
  # Zenga curve
  Zu <- (est$p - est$L)/(est$p*(1 - est$L))
  # We rescale the first and the last point for
  # graphical reasons
  Zu[1] <- Zu[2]
  Zu[length(Zu)] <- Zu[(length(Zu) - 1)]

  # Here’s the plot
  all <- data.frame(est$p, Zu)
  names(all) <- c('u', 'Z')
  p <- ggplot2::ggplot(all, ggplot2::aes(u, Z)) +
    ggplot2::geom_line() +
    ggplot2::xlab('u') +
    ggplot2::ylab('Z(u)') +
    ggplot2::ylim(0,1)

  return(p)
}
