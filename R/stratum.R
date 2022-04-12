#' @title Survey Spatial Strata
#'
#' @description Determine the spatial survey stata corresponding to a set of coordinates.
#'
#' @param longitude,latitude Numerical longitude and latitude value(s) in decimal degree format.
#'
#' @param region Character string specifying the geographic region. This argument is passed onto
#'               the \code{\link{stratum.info}} function.
#'
#' @param survey Character string specifying the research survey. This argument
#'               is passed onto the \code{\link{stratum.info}} function.
#'
#' @param x Object.
#'
#' @param ... Further arguments (not used).
#'
#' @return Returns a numerical vector the same size as \code{latitude} and \code{longitude}
#'         containing the survey strata numbers.
#'
#' @examples
#' # Get stratum information:
#' stratum(region = "gulf", survey = "rv")
#'
#' # Find the stratum at a single point:
#' stratum(-63.8, 47.05) # returns 429
#'
#' # Find strata values for multiple points:
#' lat <- c(48, 47, 46.5)
#' long <- c(-64, -61.5, -62)
#' stratum(long, lat) # returns: c(422, 434, 401)
#'
#' # Read a set card:
#' x <- read.scsset(year = 2010)
#' stratum(longitude(x), latitude(x))

#' @export
stratum <- function(x, ...) UseMethod("stratum")

#' @export
stratum.default <- function(longitude, latitude, region, survey = "rv", stratum, ...){
   if (missing(longitude) | missing(latitude)){
      # Get stratum information from polygons:
      stratum.info <- as.data.frame(read.gulf.spatial("stratum"))
      stratum.info$stratum <- as.numeric(stratum.info$stratum)

      # Subset variables:
      if (!missing(stratum)) stratum.info <- stratum.info[which(stratum.info$stratum %in% stratum), ]
      if (!missing(survey))  stratum.info <- stratum.info[which(stratum.info$survey %in% survey), ]
      if (!missing(region))  stratum.info <- stratum.info[which(stratum.info$region %in% region), ]

      return(stratum.info)
   }

   # Load stratum polygons:
   s <- read.gulf.spatial(layer = "strata", region = region, ...)
   s <- s[which(s@data$survey %in% survey), ]

   # Identify strata for each coordinate point:
   p <- sp::SpatialPoints(cbind(longitude, latitude))
   v <- as.numeric(as.character(over(p, s, fn = NULL)$stratum))

   # Return result:
   return(v)
}

