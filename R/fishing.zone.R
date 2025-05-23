#' @title Determine Fishing Zone
#'
#' @description Returns a fishing zone given a set of of coordinates.
#'
#' @param longitude Numerical vector of longitudes in decimal degree format.
#' @param latitude Numerical vector of latitudes in decimal degree format.
#' @param region Character value specifying the geographic region. This argument is passed onto the \code{\link[gulf]{fishing.zone.info}} function.
#' @param species Numerical scalar containing a species code.
#'
#' @examples
#' # Find the fishing.zone at a single point:
#' fishing.zone(-63.8, 47.05, species = 2526)
#'
#' # Find strata values for multiple points:
#' lat <- c(48, 47, 46.5)
#' long <- c(-64, -61.5, -62)
#' fishing.zone(long, lat, species = 2526)
#'
#' x <- read.scsset(2020)
#' fishing.zone(lon(x), lat(x), species = 2526) # Read snow crab set card for 2013 and determine fishing zone:
#' fishing.zone(lon(x), lat(x), species = 2550) # Lobster fishing areas of snow crab survey tows.

#' @describeIn fishing.zone  Returns a fishing zone for a set of coordinate points.
#' @rawNamespace S3method(fishing.zone,default)
fishing.zone.default <- function(longitude, latitude, species, ...){
   # Check 'species' argument:
   if (missing(species)) stop("'species' must be defined.")
   if (length(longitude) != length(latitude)) stop("'longitude' and 'latitude' must be the same length.")

   # Read polygons:
   p <- read.gulf.spatial("fishing zone polygon", file = "shp", species = species, ...)

   # Convert coordinates to 'sf' object:
   x <- sf::st_as_sf(data.frame(longitude = longitude, latitude = latitude), coords = c("longitude", "latitude"), crs = sf::st_crs(p))

   w <- options("warn")$warn
   options(warn = -1)

   # Get fishing zones:
   r <- sf::st_intersects(x,p)
   ix <- unlist(lapply(r, length))
   v <- rep(NA, length(longitude))
   v[ix == 1] <- p$label[unlist(r[ix == 1])]
   v[ix > 1] <- p$label[unlist(lapply(r[ix > 1], function(x) x[1]))]

   options(warn = w)

   return(v)
}
