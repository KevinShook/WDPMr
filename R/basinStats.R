#' Gets statistics of .asc file of a basin
#'
#' @description Returns the statistics describing a basin contained in an ArcGIS \code{.asc} file,
#' either a DEM or a water layer. The points outside the basin must be coded as missing.
#' @param asc_file Required. A text \code{.asc} file describing a basin, as used by \code{WDPM}.
#'
#' @return If successful, returns a vector containing the following values
#' \describe{
#'  \item{num_rows}{Number of rows in the file}
#'  \item{num_cols}{Number of cols in the file}
#'  \item{cell_size}{Width of each cell. Typically in metres, could be in degrees}
#'  \item{outside_cells}{Number of cells outside the basin}
#'  \item{inside_cells}{Number of cells inside the basin}
#'  \item{inside_area}{Basin area in units of \code{cell_size} squared}
#' }
#' @export
#'
#' @examples \dontrun{
#' stats <- basinStats("StDenis.asc")
#' }
basinStats <- function(asc_file) {
  if(is.null(asc_file) | asc_file == "") {
    cat("Error: asc_file is missing\n")
    return(FALSE)
  }
  basin  <-  SDMTools::read.asc(asc_file)
  cellsize <- attr(basin, "cellsize")
  dimensions <- attr(basin, "dim")
  num_rows <- dimensions[1]
  num_cols <- dimensions[2]

  total_cells <- length(basin)
  outside_cells <- sum(is.na(basin))
  inside_cells <- total_cells - outside_cells
  cell_area <- cellsize * cellsize
  inside_area <- inside_cells * cell_area

  # assemble output
  return_values <- c(num_rows, num_cols, cellsize, outside_cells, inside_cells, inside_area)
  names(return_values) <- c("num_rows", "num_cols", "cell_size", "outside_cells", "inside_cells", "inside_area")

  return(return_values)

}
