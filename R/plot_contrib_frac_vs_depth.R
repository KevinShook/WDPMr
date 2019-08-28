#' Plots rising limb contributing fraction vs storage
#'
#' @param orderedAreaVol Required. A set of ordered areas and volumes as produced by \code{volumeAreaSequence}
#'
#' @return Returns a list contaning ggplot of the contributing fraction vs depth fo water and the data frame.
#' @export
#'
#' @examples \dontrun{
#' p <- loop1contribFracPlot(areavol)}
plot_contrib_frac_vs_depth <- function(orderedAreaVol) {
  # reads WDPM areas and volumes and plots contributing fraction loop
  # declare variables
  add1 <- NULL
  subtract1 <- NULL
  add2 <- NULL
  subtract2 <- NULL

  curve1.contribfrac <- c(0)
  curve2.contribfrac <- c(0)
  curve1.depth <- c(0)
  curve2.depth <- c(0)

  basin_area <- orderedAreaVol$basin_area[1]

  # now get changes in volume
  # select additions of 1mm to existing water
  added <- subset(orderedAreaVol, add2 > 0)
  rest <- subset(orderedAreaVol, add2 == 0)
  max.volume <- max(orderedAreaVol$water.volume)
  added.count <- nrow(added)

  j <- 0
  k <- 0
  for (i in 1:added.count) {
    final <- added[i,]
    final.volume <- final$water.volume
    vol_depth <- (final.volume / basin_area) * 1000              # m -> mm
    others <- subset(rest, (add1 == final$add1) & (subtract1 == final$subtract1) &
                       (subtract2 == final$subtract2) )
    initial <- others[which.min(others$water.volume),]
    initial.volume <- initial$water.volume
    volume.added <- (final$add2 * final$basin.area) / 1000
    delta.volume  <- final.volume - initial.volume
    rof <- 1 - (delta.volume / volume.added)

    if (final$subtract1 > 0 ) {
      j <- j + 1
      curve2.depth[j] <- vol_depth
      curve2.contribfrac[j] <- rof
    }
    else{
      k <- k + 1
      curve1.depth[k] <- vol_depth
      curve1.contribfrac[k] <- rof
    }
  }


  curve1 <- data.frame(curve1.depth, curve1.contribfrac)
  p <- ggplot(curve1, aes(curve1.depth, curve1.contribfrac)) +
    geom_line() +
    xlab("Basin mean depth (mm)") +
    ylab("Fractional contibuting area")

  return_val <- list(p, curve1)
  return(return_val)

}
