#' Retrieve Coordinates
#'
#' @description Functions to retrieve coordinates from an object.
#'
#' @param x Object containing coordinates. Coordinates can be extracted from list and
#'          data frame objects if they either have defined \code{longitude} and \code{latitude}
#'          methods already defined, or looking for \sQuote{longitude} or \sQuote{latitude}
#'          in the field variable names. In the latter case, partial matching is used.
#'
#' @export coordinates
#' @export coordinates.default
#' @export longitude
#' @export latitude
#' @export longitude.data.frame
#' @export latitude.data.frame
#' @export long
#' @export lon
#' @export lat
#'
#' @section Functions:
#' \describe{
#'   \item{\code{longitude}, \code{latitude}}{Extract longitude or latitude coordinates from a data frame or list.}
#'   \item{\code{lon}, \code{long}}{Alias functions for \code{longitude}.}
#'   \item{\code{lat}}{Alias function for \code{latitude}.}
#' }
#'
coordinates <- function(x, ...) UseMethod("coordinates")

coordinates.default <- function(x, ...) if (is.installed("sp")) return(sp::coordinates(x, ...))

#' @rdname coordinates
longitude <- function(x, ...) UseMethod("longitude")

#' @rdname coordinates
latitude <- function(x, ...) UseMethod("latitude")

#' @rdname coordinates
longitude.default <- function(x, ...) return(NULL)

#' @rdname coordinates
latitude.default <- function(x, ...) return(NULL)

#' @rdname coordinates
#' @method longitude list
longitude.data.frame <- function(x, ...){
   n <- tolower(names(x))
   if (is.null(n)) return(NULL)
   index <- grep("longitude", n)[1]
   if (length(index) == 0) index <- grep("long", n)[1]
   if (length(index) == 0) index <- grep("lon", n)[1]
   if (length(index) == 0) return(NULL)
   return(x[[index]])
}

#' @rdname coordinates
#' @method latitude list
latitude.data.frame <- function(x, ...){
   n <- tolower(names(x))
   if (is.null(n)) return(NULL)
   index <- grep("latitude", n)[1]
   if (length(index) == 0) index <- grep("lat", n)[1]
   if (length(index) == 0) return(NULL)
   return(x[[index]])
}

#' @rdname coordinates
lon <- longitude

#' @rdname coordinates
lat <- latitude

#' @rdname coordinates
long <- longitude

