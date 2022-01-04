hysteresisPlot2 <- function(curve1, curve2, extend, normalize, filename){
  # plots filling/emptying curves
  # extend (T/F) forces curves through origin, and adds max value to recession limb
  x <- NULL
  y <- NULL
  xstart <- NULL
  xend <- NULL
  ystart <- NULL
  yend <- NULL
  Operation <- NULL
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

    # set axes labels
    xlabel <- "Fractional water volume"
    ylabel <- "Fractional water area"
  }
  else{
    xlabel <- expression(paste('Water volume (m',''^{3}, ')', sep = ""))
    ylabel <- expression(paste('Water area (m',''^{2}, ')', sep = ""))
  }

  # sort curves by x value
  curve1 <- curve1[order(curve1$x), ]
  curve2 <- curve2[order(curve2$x), ]

  # add bottoms to curves
  # get largest x values
  curve1.x.max <- max(curve1$x)
  curve2.x.max <- max(curve2$x)

  # get smallest x values
  curve1.x.min <- min(curve1$x)
  curve2.x.min <- min(curve2$x)

  curve1[nrow(curve1)+1,] <- c(curve1.x.max, 0)
  # curve1[nrow(curve1)+1,] <- c(curve1.x.min, 0)

  curve2[nrow(curve2)+1,] <- c(curve2.x.max, 0)
  # curve2[nrow(curve2)+1,] <- c(curve2.x.min, 0)


  if (normalize == TRUE){
    x.max <- max(curve1$x, curve2$x)
    y.max <- max(curve1$y, curve2$y)

    curve1[,1] <- curve1[,1] / x.max
    curve2[,1] <- curve2[,1] / x.max

    curve1[,2] <- curve1[,2] / y.max
    curve2[,2] <- curve2[,2] / y.max
  }

  # assemble datasets

  curve1$Operation <- rep.int("filling", nrow(curve1))
  curve2$Operation <- rep.int("emptying", nrow(curve2))
  all.points <- rbind(curve1, curve2)

  p <- ggplot2::ggplot(all.points, ggplot2::aes(x, y, fill=Operation)) +
    ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel) +
    ggplot2::geom_polygon(alpha=0.6)

  ggsave(filename)

}
