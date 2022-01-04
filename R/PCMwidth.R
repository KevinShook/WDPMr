#' Calculates width function for graphs of PCM ponds
#'
#' @param graph Required. Graph (from \pkg{igraph}) of PCM ponds. Can be created using \code{graphPCMPonds}.
#'
#' @return Returns a \code{hist} object of pond counts vs. distance from outlet.
#' @export
#'
#' @examples\dontrun{
#' h <- PCMwidth(PCMgraph)
#' plot(h)}
PCMwidth <- function(graph){

  # see if there is node zero or not
  if ('0' %in% igraph::V(graph)$name){
    dist.from.outlet <-  igraph::distances(graph, v= igraph::V(graph)[ igraph::V(graph)$name=="0"],
                                           to= igraph::V(graph), weights=NA)
    dist.from.outlet <- t(dist.from.outlet)
  }
  else{
    dist.from.outlet <-  igraph::distances(graph, v= igraph::V(graph)[ igraph::V(graph)$name=="1"],
                                           to= igraph::V(graph), weights=NA)
    dist.from.outlet <- t(dist.from.outlet)
  }

  # get histogram
  h <- graphics::hist(dist.from.outlet, breaks=max(dist.from.outlet), plot=FALSE)
  return(h)

}
