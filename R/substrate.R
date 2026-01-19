#' @title Local Substrate Information
#'
#' @description Returns local sea-floor substrate information.
#'
#' @param longitude,latitude Numerical vector of longitudes in decimal degree format.
#'
#' @return Returns character vector of substrate composition (e.g. sand, gravel, etc.)
#' the same length as the coordinate vectors.
#'
#' @references Loring and Nota, 1973, https://gcgeo.gc.ca/geonetwork/metadata/eng/8c269a91-d3a2-4f49-943d-6b2401c42cba, accessed April 10, 2025
#'
#' @examples
#'    # Find the water depth at a single point:
#'    substrate(63.8, 47.05)
#'
#'    # Find water depth for multiple points:
#'    lat <- c(48, 47, 46.5)
#'    long <- c(-64, -61.5, -62)
#'    substrate(long, lat)
#'
#' @export substrate
#'

substrate <- function(longitude, latitude){
   # Define substrate file path:
   path <- locate.gulf.spatial(c("Seafloor", "shp"))
   path <- paste0(strsplit(file, "shapefiles")[[1]][1], "shapefiles")

   # Read seafloor substrate file:
   substrate <- sf::read_sf(dsn = path, layer = "Seafloor_SubstratBenthique")

   # Create and re-project coordinate data:
   p <- sf::st_as_sf(data.frame(longitude, latitude), crs = 4326, coords = c("longitude", "latitude"))
   p <- sf::st_transform(p, crs = sf::st_crs(substrate))

   # Join points and substrate data:
   p <- sf::st_join(p, subtrate, left = TRUE, sf::st_nearest_feature)[, c("DEPOT_EN")]
   names(p) <- gsub("DEPOT_EN", "substrate", names(p))

   # Clean-up:
   p$substrate <- gsub("gravier", "gravel", tolower(p$substrate))

   return(p$substrate)
}
