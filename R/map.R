#' Map Gulf of St. Lawrence Features
#'
#' @description Draws southern Gulf of St. Lawrence geographical features.
#'
#' @param layer Layer specification
#'
#' @seealso \code{\link{read.gulf.spatial}}
#'
#' @examples
#' map()
#' map(layer = "strata", survey = "rv", region = "gulf")
#'
#' @export
#'

#' @export map
map <- function(x, ...) UseMethod("map")

#' @export map.new
map.new <- function(xlim, ylim, region = "gulf", ...){
   # Parse 'region':
   region <- match.arg(tolower(region), c("gsl", "gulf", "4rst", "sgsl", "southern gulf",
                                          "4t",  "ngsl", "northern gulf", "4s",
                                          "cb", "cape breton", "wcb", "western cape breton",
                                          "ns", "northumberland strait"))

   if (region %in% c("4t", "sgsl", "gulf"))       region <- "southern.gulf"
   if (region %in% c("4rst"))                     region <- "gsl"
   if (region %in% c("4s", "ngsl"))               region <- "northern.gulf"
   if (region %in% c("cb", "cape breton", "wcb")) region <- "western.cape.breton"
   if (region %in% c("ns"))                       region <- "northumberland.strait"

   # Define axis limits:
   if (missing(xlim)){
      if (region == "gsl")                   xlim <- c(-70, -56)
      if (region == "southern.gulf")         xlim <- c(-66.5, -60)
      if (region == "northern.gulf")         xlim <- c(-70, -59)
      if (region == "western.cape.breton")   xlim <- c(-62.5, -60)
      if (region == "northumberland.strait") xlim <- c(-65.5, -61.0)
   }
   if (missing(ylim)){
      if (region == "gsl")                   ylim <- c(45, 52)
      if (region == "southern.gulf")         ylim <- c(45.5, 49.3)
      if (region == "northern.gulf")         ylim <- c(47.5, 51)
      if (region == "western.cape.breton")   ylim <- c(45.5, 47.5)
      if (region == "northumberland.strait") ylim <- c(45.5, 47.75)
   }

   # Blank axes:
   plot(xlim, ylim, type = "n", xlab = NA, ylab = NA, xaxs = "i", yaxs = "i", axes = FALSE, ...)
}

#' @describeIn map Plot Gulf of Saint Lawrence base map.
#' @export
map.default <- function(layer, add = TRUE, ...){
   if (!add) map.new()

   # Load gulf coast:
   if (!missing(layer)){
      if (length(grep("coast", layer)) > 0)      {coast(...); return()}
      if (length(grep("bathymetry", layer)) > 0) {bathymetry(...); return()}

      # Load spatial data:
      v <- read.gulf.spatial(file = locate.gulf.spatial(layer = layer[i], ...), region = region, ...)

      if ("SpatialPolygonsDataFrame" %in% class(v)) plot(v, ...)
      if (is.data.frame(v)){
         if (length(grep("polygon", layer)) > 0) graphics::polygon(v$longitude, v$latitude, ...)
         if (length(grep("vertice", layer)) > 0) graphics::lines(v$longitude, v$latitude, ...)
         if (length(grep("station", layer)) > 0) graphics::points(v$longitude, v$latitude, ...)
      }
   }
}

#' @describeIn map Plot \strong{eSonar} coordinate track data.
#' @export
map.esonar <- function(x, variable, ...){
   # Create map axes:
   rx <- range(lon(x))
   ry <- range(lat(x))
   dx <- diff(rx)
   dy <- diff(ry)
   map(xlim = c(rx[1] - dx*0.1, rx[2] + dx*0.1), ylim = c(ry[1] - dy*0.1, ry[2] + dy*0.1))

   # Draw points:
   points(lon(x), lat(x), pch = 21, bg = "blue", cex = 0.8, ...)
}
