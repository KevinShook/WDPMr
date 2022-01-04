#' Calculates degree of hysteresis between 2 curves
#' @description Calculates fractional hysteresis as difference in areas under curves. Uses trapeziodal integration. Can force curves through origin, and adds max value to lower curve.
#' @param curve1 Required. Data frame of (x,y) values for first curve.
#' @param curve2 Required. Data frame of (x,y) values for second curve.
#' @param extend Optional. Should curves be forced through the origin? Also extends the lower curve to include the maximum point. Default is \code{TRUE}.
#'
#' @return Returns the fractional hysteresis between the curves, i.e. the area between the curves as a fraction of the area below the upper curve.
#' @author Kevin Shook
#' @seealso \code{\link{trapint}}
#' @export
#'
#' @examples \dontrun{
#' hysteresis(curve1, curve2)
#' }
hysteresis <- function(curve1, curve2, extend=TRUE){
  names(curve1) <- c('x','y')
  names(curve2) <- c('x','y')

  if (extend == TRUE){
    # check both columns for zero values and add zeroes if missing
    if (!(0 %in% curve1[,1]) && !(0 %in% curve1[,2]))
      curve1[nrow(curve1)+1,] <- c(0,0)

    if (!(0 %in% curve2[,1]) && !(0 %in% curve2[,2]))
      curve2[nrow(curve2)+1,] <- c(0,0)

    # get largest values
    curve1.x.max <- max(curve1$x)
    curve1.y.max <- max(curve1$y)

    curve2.x.max <- max(curve2$x)
    curve2.y.max <- max(curve2$y)

    # add max x and y to smaller curve
    if ((curve1.x.max > curve2.x.max) &&(curve1.y.max > curve2.y.max))
      curve2[nrow(curve2)+1,]  <- c(curve1.x.max, curve1.y.max)

    if ((curve2.x.max > curve1.x.max) &&(curve2.y.max > curve1.y.max))
      curve1[nrow(curve1)+1,]  <- c(curve2.x.max, curve2.y.max)

  }

  # sort curves by x value
  curve1 <- curve1[order(curve1$x), ]
  curve2 <- curve2[order(curve2$x), ]

  # integrate curves
  auc1 <- trapint(curve1)
  auc2 <- trapint(curve2)

  hysteresis <- 1.0 - min(auc1, auc2)/ max(auc1, auc2)

}
