#' Calculates non-exceedance probabilities
#'
#' @param sizes Required. A vector of values.
#' @param useLargest Optional. If \code{TRUE} then the largest value will be used, i.e. its non-exceedence probability willl be > 0. Default is \code{FALSE}.
#' @param minval Optional. Minimum value to use. Default is \code{0}.
#'
#' @return Returns the size values and their non-exceedance probabilities
#' @export
#'
#' @examples
#' a <- korcak(runif(100))
korcak <- function(sizes, useLargest=FALSE, minval=0){

 sizes <- sizes[sizes > minval]

 if (!useLargest){
   ecdf.fun <- stats::ecdf(sizes)
   P <- 1- ecdf.fun(sizes)
   out <- data.frame(sizes, P)
 }
 else{
   # rank values
   ranked <- sizes[order(sizes, decreasing=TRUE)]
   ranks <- seq(from=1, to=length(ranked), by=1)
   maxRank <- max(ranks)
   P <- ranks / (maxRank + 1)
   out <- data.frame(ranked, P)
 }

 names(out) <- c("value", "p")
 return(out)
}
