#' Determine Fishing Zone
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

   # Read polygons:
   p <- read.gulf.spatial("fishing zone polygon", file = "shp", species = species, ...)

   # Convert coordinates to 'sp' object:
   x <- data.frame(longitude = longitude, latitude = latitude)
   sp::coordinates(x) <- ~ longitude + latitude
   w <- options("warn")$warn
   options(warn = -1)
   sp::proj4string(x) <- sp::proj4string(p)
   options(warn = w)

   # Determine zone:
   r <- sp::over(x, p)

   return(r$label)
}
