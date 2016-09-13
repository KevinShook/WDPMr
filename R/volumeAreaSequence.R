#' Orders values created by totalAreaVolume
#' @description The areas and volumes are sorted in order of addition and removal of water. These value can then be used to calculate and plot the hysteresis between water vaolume and area, and between water volume and contributing (connected) area.
#' @param areasVolumes Required. A data frame containing WDPM total water areas and volumes.
#'
#' @return If successful, returns a data frame of the water areas and volumes, sorted in order of Add1, Subtract1, Add2, Subtract2. If unsuccssful, returns \code{FALSE}.
#' @author Kevin Shook
#' @seealso code{\link{totalAreaVolume}} code{\link{loop1ContribFracPlot}}
#' @export
#'
#' @examples \dontrun{
#' areavol <- volumeAreaSequence(areasVolumes}
volumeAreaSequence <- function(areasVolumes){

  if(is.null(areasVolumes)){
    cat('Error: missing values\n')
    return(FALSE)
  }

  # parse file names

  # remove file extensions
  f <- stringr::str_split(areasVolumes$file, pattern=stringr::fixed('.'),n=2)
  output <- data.frame(areasVolumes)
  output$file <- unlist((lapply(f, "[[", 1)))

  # count number of _ characters
  underscores <- stringr::str_count(output$file[1], pattern=stringr::fixed('_'))

  # spit up file names
  f <- stringr::str_split(output$file, pattern=stringr::fixed('_'), n=underscores+1)

  # check if first and last columns are needed
  options(warn=-1)
  if (!is.numeric(f[[1]][[1]]))
    doFirst <- FALSE
  else
    doFirst <- TRUE

  if (!is.numeric(f[[1]][[underscores]]))
    doLast <- FALSE
  else
    doLast <- TRUE
  options(warn=0)

  pieces <- seq(1:(underscores+1))

  if(!doFirst){
    pieces <- pieces[-1]
    output$name <- unlist((lapply(f, "[[", 1)))
  }


  if(!doLast){
    pieces <- pieces[1:(length(pieces)-1)]
    output$type <- unlist((lapply(f, "[[", (underscores+1))))
  }

  num_pieces <- length(pieces)
  if (num_pieces >= 2){
    add1 <- as.numeric(unlist((lapply(f, "[[", pieces[1]))))
    subtract1 <- as.numeric(unlist((lapply(f, "[[", pieces[2]))))
    output <- data.frame(output, add1, subtract1)
    ordered_areasVolumes <- output[order(output$add1, output$subtract1),]
  }
  if (num_pieces >= 3){
    add2 <- as.numeric(unlist((lapply(f, "[[", pieces[3]))))
    output <- data.frame(output, add2)
    ordered_areasVolumes <- output[order(output$add1, output$subtract1, add2),]
  }
  if (num_pieces >= 4){
    subtract2 <- as.numeric(unlist((lapply(f, "[[", pieces[4]))))
    output <- data.frame(output, subtract2)
    # order by addition and subtraction
    ordered_areasVolumes <- output[order(output$add1, output$subtract1,
                                         output$add2, output$subtract2),]
  }

  if (num_pieces >= 5){
    add3 <- as.numeric(unlist((lapply(f, "[[", pieces[5]))))
    output <- data.frame(output, add3)
    ordered_areasVolumes <- output[order(output$add1, output$subtract1,
                                         output$add2, output$subtract2,
                                         output$add3),]
  }

  if (num_pieces >= 6){
    subtract3 <- as.numeric(unlist((lapply(f, "[[", pieces[6]))))
    output <- data.frame(output, subtract3)
    ordered_areasVolumes <- output[order(output$add1, output$subtract1,
                                         output$add2, output$subtract2,
                                         output$add3, output$subtract3),]
  }


  return(ordered_areasVolumes)
}
