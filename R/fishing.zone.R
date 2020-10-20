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
#'    # Find the fishing.zone at a single point:
#'    fishing.zone(-63.8, 47.05, species = 2526)
#'
#'    # Find strata values for multiple points:
#'    lat <- c(48, 47, 46.5)
#'    long <- c(-64, -61.5, -62)
#'    fishing.zone(long, lat, species = 2526)
#'
#'    # Read snow crab set card for 2013 and determine fishing zone:
#'    x <- read.gulf(year = 2013, survey = "sc")
#'    zone <- fishing.zone(longitude(x), latitude(x), species = 2526)
#'
#'    # Read rv set card for 2013 and determine fishing zone:
#'    x <- read.gulf(year = 2013)
#'    zone <- fishing.zone(longitude(x), latitude(x), species = 60)

#' @export fishing.zone
fishing.zone <- function(x, ...) UseMethod("fishing.zone")

#' @describeIn fishing.zone  Returns a fishing zone for a set of coordinate points.
#' @rawNamespace S3method(fishing.zone,default)
fishing.zone.default <- function(longitude, latitude, species = NULL, region = "gulf", ...){
   # Check 'species' argument:
   if (is.null(species)) stop("'species' must be defined.")

   # Get fishing zone polygons definitions:
   utils::data(fishing.zone.polygons)
   p <- fishing.zone.polygons
   p <- subset(p, species = species, region = region)

   # Create index matching points to polygon:
   index <- which.polygon(p, x = -abs(longitude), y = latitude, as.list = FALSE)

   # Extract fishing zone labels:
   labels <- NA
   labels[!is.na(index)] <- unlist(lapply(p[index[!is.na(index)]], function(x) x$label))

   return(labels)
}
