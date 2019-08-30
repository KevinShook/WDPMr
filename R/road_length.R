#' Length of roads in a basin
#'
#' @param road_shapefile Required. Shapefile containing roads.
#' @param basin_shapefile Required. Shapefile of basin boundary.
#'
#' @return Returns a vector of the total length of roads (km), basin area (km2) and
#' density of roads (km/km2).
#' @import raster rgeos rgdal
#' @export
#'
#' @examples \dontrun{road_length("basin5roads.shp", "basin5.shp")}
road_length <- function(road_shapefile, basin_shapefile) {

  # read in shapefiles
  roads <- readOGR(road_shapefile)
  basin <- readOGR(basin_shapefile)


  # get intersection
  basin_roads <- intersect(roads, basin)

  # get total length of roads
  total_road_length <- gLength(basin_roads) / 1e3
  basin_area <- gArea(basin) / 1e6
  road_density <- total_road_length / basin_area

  result <- c(total_road_length, basin_area, road_density)
  names(result) <- c("total_road_length", "basin_area", "road_density")
}
