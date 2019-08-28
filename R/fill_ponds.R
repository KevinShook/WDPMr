fill_ponds <- function(slough_area, slough_volume = NULL, drainage_area = NULL, p1 = 1.72, p2 = 3.33) {
  # calculate pond volumes and drainage areas
  num_sloughs <- length(slough_area)
  if (is.null(drainage_area[1])) {
    drainage_area <- exp(3.44329) * (slough_area ^ 0.738377)
  }

  p1 <- 1.72
  pval <- rep.int(p1, num_sloughs)

  h = 1
  S = slough_area / (h ** (2.0 / pval))
  maxVol <- S / (1 + (2/pval))
  maxArea <- drainage_area
  area <- maxArea
  #area <- exp(3.44329) * (maxArea ^ 0.738377)
  fakeArea <- maxVol
  vol <- maxVol

  # get actual sum of areas
  df <- data.frame(vol, area, fakeArea)

  maxVol <- max(df$vol)
  df.sorted <- df[order(df$vol, decreasing = FALSE),]
  totalArea <- sum(df.sorted$area)
  totalVol <- sum(df.sorted$vol)
  numinc <- num_sloughs
  for (i in 1:numinc){
    volfraction <- i / numinc
    volVal <- volfraction * maxVol
    areaSum <- sum(df.sorted$area[df.sorted$vol <= volVal])
    fakeAreaSum <- sum(df.sorted$fakeArea[df.sorted$vol <= volVal])
    volSum <- sum(pmin(df.sorted$vol, volVal))
    areaFrac[i] <- areaSum / totalArea
    fakeAreaFrac[i] <- fakeAreaSum / totalVol
    volFrac[i] <- volSum / totalVol
  }
  df1 <- data.frame(volFrac, areaFrac)
  df2 <- data.frame(volFrac, fakeAreaFrac)

  names(df1) <- c('volfrac', 'areafrac')
  names(df2) <- c('volfrac', 'areafrac')

  df1$type <- 'Filling'
  df2$type <- 'Filling'

  df1$value <- 'Area connected fraction'
  df2$value <- 'Volume connected fraction'
}
