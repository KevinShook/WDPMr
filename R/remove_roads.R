#' Removes roads from a DEM
#'
#' @param DEM_file Required. ArcGIS .asc file.
#' @param road_shapefile Required. Shapefile of roads buffered to road widths.
#' @param basin_shapefile Required. Shpefile of basin boundary.
#'
#' @return If successful, returns \code{TRUE}. Writes several files, including 1)
#' a .asc file of the DEM with missing values in place of the roads, 2) SAGA files (.sdat, .mgrd)
#' contining the filled DEM, and 3) a .asc file of the filled DEM clipped to the basin
#' boundary.
#'
#' @note This function uses the \pkg{raster} function \code{mask} to delete the
#' DEM elements covered by the buffered roads, and \pkg{RSAGA} function \code{rsaga.close.gaps} to fill in
#' the deleted DEM cells.
#' @export
#' @import raster RSAGA rgdal
#'
#' @examples \dontrun{remove_roads("basin5.asc", "basin5_roads.shp", "basin5_boundary.shp")}
remove_roads <- function(DEM_file, road_shapefile, basin_shapefile) {

  # read in DEM raster
  DEM <- raster(DEM_file)

  # read roads
  roads <- readOGR(road_shapefile)

  # clip DEM raster with road shapefile
  clipped <- mask(DEM, roads, inverse = TRUE)

  # save as SAGA DEM
  DEM_location <- dirname(DEM_file)
  DEM_file_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(DEM_file))
  no_road_file <- paste(DEM_location, "/", DEM_file_name,"_no_roads.asc", sep = "")
  writeRaster(clipped, no_road_file, format = "ascii")

  # write SAGA file
  no_road_file_SAGA <- paste(DEM_location, "/", DEM_file_name,"_no_roads.sgrd", sep = "")
  writeRaster(clipped, no_road_file, format = "SAGA")

  # fill in DEM
  filled_dem_file <- paste(DEM_location, "/", DEM_file_name,"_filled_roads.asc", sep = "")

  # close gaps
  filled_dem_file_sdat <- paste(DEM_location, "/", DEM_file_name,"_filled_roads.sdat", sep = "")
  filled_dem_file_sgrd <- paste(DEM_location, "/", DEM_file_name,"_filled_roads.sgrd", sep = "")
  try(rsaga.esri.wrapper(rsaga.close.gaps(in.dem = no_road_file, out.dem = filled_dem_file_sgrd)))


  # clip closed file with basin shapefile
  closed <- raster(filled_dem_file_sdat)
  basin <- readOGR(basin_shapefile)
  clipped_closed <- mask(closed, basin)

  # write to .asc file
  closed_asc_file <- paste(DEM_location, "/", DEM_file_name,"_filled_roads_clipped.asc", sep = "")
  writeRaster(clipped_closed, filename = closed_asc_file, format = "ascii")
  return(TRUE)
}
