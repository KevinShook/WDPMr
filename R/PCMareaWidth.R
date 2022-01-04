#' Calculates width function for graphs of PCM ponds weighted by area
#'
#' @param graph Required. Graph (from \pkg{igraph}) of PCM ponds. Can be created using \code{graphPCMPonds}.
#' @param areas Required. Vector containing pond areas.
#'
#' @return Returns a \code{hist} object of weighted pond counts vs. distance from outlet.
#' @export
#'
#' @examples\dontrun{
#' h <- PCMareaWidth(PCMgraph, areas)
#' plot(h)}
PCMareaWidth <- function(graph, areas){
  # see if there is node zero or not
  if ('0' %in% igraph::V(graph)$name) {
    dist.from.outlet <-  igraph::distances(graph,
                                           v = igraph::V(graph)[ igraph::V(graph)$name == "0"],
                                           to = igraph::V(graph), weights = areas)
    dist.from.outlet <- t(dist.from.outlet)
  }
  else{
    dist.from.outlet <-  igraph::distances(graph,
                                           v =  igraph::V(graph)[ igraph::V(graph)$name == "1"],
                                           to = igraph::V(graph), weights = areas)
    dist.from.outlet <- t(dist.from.outlet)
  }

  # get histogram
  h <- graphics::hist(dist.from.outlet, breaks = max(dist.from.outlet),
                      plot = FALSE)
  return(h)

}
