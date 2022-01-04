#' Interleaves 2 vectors.
#' From bogdan romocea http://r.789695.n4.nabble.com/Interleaving-elements-of-two-vectors-td795123.html
#' @param v1 Required. Vector 1.
#' @param v2 Required. Vector 2.
#' @return  Returns a vector with elements of vectors 1 and 2 interleaved.
#' @export
#' @examples{
#' a <- c(1,2,3)
#' b <- c(4,5,6)
#' interleave(a,b)}
interleave <- function(v1,v2)
{
  ord1 <- 2*(1:length(v1))-1
  ord2 <- 2*(1:length(v2))
  c(v1,v2)[order(c(ord1,ord2))]
}
