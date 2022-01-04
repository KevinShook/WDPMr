#' Creates graph of PCM ponds
#'
#' @param dest Required. Vector of drainage destinations.
#' @param numPonds Optional. Number of ponds to be plotted. If omitted, all ponds will be used.
#' @param disConnected Optional. Vector of link numbers to be deleted.
#' @param size Optional. Either scalar of common pond size or vector of all pond areas.
#' @param maxSize Optional. Used to scale vector of pond areas.
#' @param removeNodeZero Optional. Should pond zero be plotted? Default is \code{FALSE}.
#' @param labels Optional. Should ponds labels be plotted? Default is \code{FALSE}.
#' @param arrowSize Optional. Size of arrow heads. Default is \code{0} (no arrow).
#' @param simplify Optional. Should plot be simplified? Default is \code{TRUE}.
#' @param vertexColours Optional. Can be a single named colour for all vertices or a vector of names colours for individual ponds.
#' @param edgeColours Optional. Can be a single named colour for all edges or a vector of names colours for individual edges.
#' @param outletRed Optional. If \code{TRUE} (the default) then the outlet pond is coloured red.
#' @param lineWidth Optional. Sets line width for edges. Default is 1.
#' @return Returns an \pkg{igraph} object of the PCM model.
#' @export
#' @examples \dontrun{
#' g <- graphPCMPonds(pcmModel)
#' plot(g)}

graphPCMPonds <- function(dest, numPonds=0, disConnected="", size=0, maxSize=20, removeNodeZero=TRUE,
                         labels=FALSE, arrowSize=0, simplify=TRUE, vertexColours='', edgeColours='',
                         outletRed=TRUE, lineWidth=1){

  dest <- as.character(dest)
  if (removeNodeZero){
    dest[dest=="0"] <- "1"
  }
  if (numPonds <= 0)
    numPonds <- length(dest)
  else
    dest <- dest[1:numPonds]


  # add pond ID
  ID <- seq(1, numPonds)
  ID <- as.character(ID)

  # convert to vector
  ponds <- interleave(ID, dest)

  # create graph
  g <- igraph::graph(ponds)

  if (simplify)
    g <- igraph::simplify(g)

  # now set graph parameters
  # set size first
  if (length(size) == 1){
    if (size > 0){

    }
    else{
      size <- maxSize
    }
  }
  else{
    # apply size vector, after scaling
    # trim size vector
    if (removeNodeZero){
      if (length(size) < (numPonds)){
        cat("Error: insufficient pond areas\n")
        return(FALSE)
      }
      else
        size <- size[1:(numPonds)]
    }
    else{
      if (length(size) < (numPonds+1)){
        cat("Error: insufficient pond areas\n")
        return(FALSE)
      }
      else
        size <- size[1:(numPonds+1)]
    }

    biggest <- max(size)
    size <- (size / biggest) * maxSize
  }

  igraph::V(g)$size <- size

  # set arrows
  if (arrowSize == 0)
    igraph::E(g)$arrow.mode <- 0
  else
    igraph::E(g)$arrow.size <- arrowSize

  # set line width
   igraph::E(g)$width <- lineWidth

  # set labels
  if (!labels)
    igraph::V(g)$label <- ""

  # disconnect links
  if (length(disConnected) > 1)
    g <- igraph::delete_edges(g, disConnected)
  else if (disConnected !="")
    g <- igraph::delete_edges(g, disConnected)

  # set colours
  if (length(vertexColours) > 1)
    igraph::V(g)$color <- vertexColours
  else if (vertexColours != "")
    igraph::V(g)$color <- vertexColours

  if (length(edgeColours) > 1)
    igraph::E(g)$color <- edgeColours
  else if (edgeColours != "")
    igraph::E(g)$color <- edgeColours

  if(outletRed)
    igraph::V(g)$color[1] <- "red"

  return(g)
}
