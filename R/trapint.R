#' Integrates function by trapeziodal rule
#'
#' @param curve Required. A data frame of x and y values. Points must be sorted by x value.
#'
#' @return Returns the area under the curve.
#' @author Kevin Shook
#' @export
#'
#' @examples \dontrun{
#' area <- trapint(curve)}

trapint <- function(curve){
  x.delta <- diff(curve[,1])
  y <- curve[,2]
  y2 <- y[-1]
  y1 <- y[1:length(y)-1]
  y.mean <- (y1+y2)/2

  trapint <- sum(x.delta * y.mean)

}
