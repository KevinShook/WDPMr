#' Finds total water volumes and areas in WDPM output files
#' @description The total area and volume are used to find hysteresis and to determine the contributing (conencted) fraction.
#' @param location Optional. Directory containing the WDPM output files. Default is the current working directory.
#' @param filespec Optional. File specification of WDPM output in current working directory. Default is \option{*.asc}.
#' @param outfile Optional. Name of file to export values. Values are comma-delimited. If not specified, values are not written.
#'
#' @return Returns basin area, water volume, water area for each .asc file. Optionally writes values to \option{outfile}.
#' @author Kevin Shook
#' @export
#'
#' @examples \dontrun{
#' setwd('./WDPM/output')
#' areasVolumes <- totalAreaVolume(filespec='*_d.asc', 'drainedAreasVolumes.csv')}
totalAreaVolume <- function(location='.', filespec='*.asc', outfile=''){
  # declare variables
  waterfilename <- c(0)
  total.area <- c(0)
  water.volume <- c(0)
  water.area <- c(0)

  # set location if req'd
  if (location !='.'){
    currentDir <- getwd()
    setwd(location)
  }

  # get files to read
  FilePattern <- utils::glob2rx(filespec)
  FileList <- list.files(pattern=FilePattern)
  NumFiles <- length(FileList)

  for (i in 1:NumFiles){
    infile <- FileList[i]

    asc  <-  SDMTools::read.asc(infile)
    cellsize <- attr(asc, "cellsize")
    waterfilename[i]  <-  infile
    good <- asc[!is.na(asc)]

    # get total area of water
    total.area[i] <- as.double(length(good)) * cellsize^2
    water.area[i] <- as.double(length(good[good > 0])) * cellsize^2
    water.volume[i] <- sum(as.double(good[good > 0]) * cellsize^2)
  }

  # create output dataframe
  output <- data.frame(waterfilename, total.area, water.area, water.volume)
  names(output) <- c('file', 'basin.area', 'water.area', 'water.volume')

  if(outfile !='')
    utils::write.csv(output, file=outfile, row.names = FALSE)


  # reset location
  if (location !='.'){
    setwd(currentDir)
  }

  return(output)

}
