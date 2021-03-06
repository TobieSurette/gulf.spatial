#' Retrieve Spatial Coordinates
#'
#' @description Functions to retrieve coordinates from an object.
#'
#' @param x Object containing coordinates. Coordinates can be extracted from list and
#'          data frame objects if they either have defined \code{longitude} and \code{latitude}
#'          methods already defined, or looking for \sQuote{longitude} or \sQuote{latitude}
#'          in the field variable names. In the latter case, partial matching is used.
#'
#' @export
#'
#' @section Functions:
#' \describe{
#'   \item{\code{longitude}, \code{latitude}}{Extract longitude or latitude coordinates from a data frame or list.}
#'   \item{\code{lon}, \code{long}}{Alias functions for \code{longitude}.}
#'   \item{\code{lat}}{Alias function for \code{latitude}.}
#' }
#'

#' @rdname coordinates
#' @export
longitude <- function(x, ...) UseMethod("longitude")

#' @rdname coordinates
#' @export
latitude <- function(x, ...) UseMethod("latitude")

#' @rdname coordinates
#' @export
longitude.default <- function(x, ...) return(NULL)

#' @rdname coordinates
#' @export
latitude.default <- function(x, ...) return(NULL)

#' @rdname coordinates
#' @export
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
#' @export
latitude.data.frame <- function(x, ...){
   n <- tolower(names(x))
   if (is.null(n)) return(NULL)
   index <- grep("latitude", n)[1]
   if (length(index) == 0) index <- grep("lat", n)[1]
   if (length(index) == 0) return(NULL)
   return(x[[index]])
}

#' @rdname coordinates
#' @export
lon <- longitude

#' @rdname coordinates
#' @export
lat <- latitude

#' @rdname coordinates
#' @export
long <- longitude

