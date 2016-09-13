pondCountHysteresisPlot <- function(curve1, curve2, extend, normalize, filename, N){
  # plots filling/emptying curves
  # extend (T/F) forces curves through origin, and adds max value to recession limb

  # declare variables
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
    # check filling curve columns for zero values and add zeroes if missing
    if (!(0 %in% curve1[,1]) && !(0 %in% curve1[,2]))
      curve1[nrow(curve1)+1,] <- c(0,0)
  }
  # get largest values
  curve1.x.max <- max(curve1$x)
  curve1.y.max <- max(curve1$y)
  curve2.x.max <- max(curve2$x)
  curve2.y.max <- max(curve2$y)

  # set axes labels
  xlabel <- expression(paste('Water volume (m',''^{3}, ')', sep = ""))
  ylabel <- "Number of ponds"

  # sort curves by x value
  curve1 <- curve1[order(curve1$x), ]
  curve2 <- curve2[order(curve2$x), ]

  if (normalize == TRUE){
    xlabel <- "Fractional water volume"
    x.max <- max(curve1.x.max, curve2.x.max)

    curve1[,1] <- curve1[,1] / x.max
    curve2[,1] <- curve2[,1] / x.max
  }

  # assemble datasets
  if ((curve1.x.max > curve2.x.max) &&(curve1.y.max > curve2.y.max)){
    curve1$Operation <- rep.int("filling", nrow(curve1))
    curve2$Operation <- rep.int("emptying", nrow(curve2))
    curve1 <- curve1[order(curve1$x, decreasing = FALSE), ]
    curve2 <- curve2[order(curve2$x, decreasing = TRUE), ]
    all.points <- rbind(curve1, curve2)

    ind <- seq(1,nrow(curve1),by=N)

    curve1.segs <- data.frame(xstart = curve1$x[ind],
                              xend = curve1$x[ind+N],
                              ystart = curve1$y[ind],
                              yend = curve1$y[ind+N])

    curve1.segs$Operation <- rep.int("filling", length(ind))

    ind <- seq(1,nrow(curve2),by=N)


    curve2.segs <- data.frame(xstart = curve2$x[ind],
                              xend = curve2$x[ind+N],
                              ystart = curve2$y[ind],
                              yend = curve2$y[ind+N])

    curve2.segs$Operation <- rep.int("emptying", length(ind))
    all.segs <- rbind(curve1.segs, curve2.segs)
  }
  else{
    curve1$Operation <- rep.int("filling", nrow(curve1))
    curve2$Operation <- rep.int("emptying", nrow(curve2))
    curve1 <- curve1[order(curve1$x, decreasing = FALSE), ]
    curve2 <- curve2[order(curve2$x, decreasing = TRUE), ]


    all.points <- rbind(curve1, curve2)

    ind <- seq(1,nrow(curve1),by=N)

    curve1.segs <- data.frame(xstart = curve1$x[ind],
                              xend = curve1$x[ind+N],
                              ystart = curve1$y[ind],
                              yend = curve1$y[ind+N])

    curve1.segs$Operation <- rep.int("filling", length(ind))

    ind <- seq(1,nrow(curve2),by=N)
    # ind <- ind[-c(1,length(ind))]

    curve2.segs <- data.frame(xstart = curve2$x[ind],
                              xend = curve2$x[ind+N],
                              ystart = curve2$y[ind],
                              yend = curve2$y[ind+N])

    curve2.segs$Operation <- rep.int("emptying", length(ind))
    all.segs <- rbind(curve1.segs, curve2.segs)

  }
  #  all.segs$Operation <- factor(all.segs$Operation, levels = rev(levels(all.segs$Operation)))
  #  all.points$Operation <- factor(all.points$Operation, levels = rev(levels(all.points$Operation)))

  p <- ggplot2::ggplot(all.points, ggplot2::aes(x, y, colour=Operation)) +
    ggplot2::scale_colour_manual(values = c("red","blue")) +
    ggplot2::xlab(xlabel) + ylab(ylabel) +
    ggplot2::geom_segment(data=all.segs,
                          ggplot2::aes(xstart,ystart,xend=xend,yend=yend,
                                       colour=Operation),
                          arrow=ggplot2::arrow(length=unit(4,"mm"))) +
    ggplot2::theme_gray(18)
  ggsave(filename)

}
