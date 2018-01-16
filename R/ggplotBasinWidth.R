#' Create ggplot2 of basin width function
#'
#' @param destinations Required. Vector containing pond drainage destinations as used by \code{PCM}.
#' @param counts Optional. If \code{TRUE}, the default, plots number of ponds as y-axis. Otherwise plots the density.
#' @return Returns ggplot2 object of basin width function.
#' @export
#'
#' @examples \dontrun{p <- ggplotBasinWidth(dests)}
ggplotBasinWidth <- function(destinations, counts=TRUE) {
  p <- NULL
  distance <- NULL
  width <- NULL
  density <- NULL

  # get graph
  g <- graphPCMPonds(destinations, removeNodeZero = FALSE)

  # get width function
  width <- PCMwidth(g)

  # assemble output
  df <- data.frame(width$breaks[-1], width$counts, width$density)
  names(df) <- c('distance', 'width', 'density')
  if (counts) {
    p <- ggplot2::ggplot(df, ggplot2::aes(distance, width)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2) +
      ggplot2::xlab('Network distance') +
      ggplot2::ylab('Number of ponds')
  } else {
    p <- ggplot2::ggplot(all, ggplot2::aes(distance, density)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2) +
      ggplot2::xlab('Network distance') +
      ggplot2::ylab('Width density')
  }
 return(p)
}
