#' Calculates non-exceedance probabilities
#'
#' @param sizes Required. A vector of values.
#'
#' @return Returns the size values and their non-exceedance probabilities
#' @export
#'
#' @examples
#' a <- korcak(runif(100))
korcak <- function(sizes){

 sizes <- stats::na.omit(sizes)
 ecdf.fun <- stats::ecdf(sizes)
 P <- 1- ecdf.fun(sizes)
 out <- data.frame(sizes, P)
 names(out) <- c("value", "p")
 return(out)
}
