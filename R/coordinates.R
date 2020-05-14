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
#' @export longitude
#' @export latitude
#' @export longitude.list
#' @export latitude.list
#' @export lat
#' @export lon
#' @export long
#'
coordinates <- function(x, ...) UseMethod("coordinates")

#' @describeIn coordinates Generic \code{longitude} function.
longitude <- function(x, ...) UseMethod("longitude")

#' @describeIn coordinates Generic \code{latitude} function.
latitude <- function(x, ...) UseMethod("latitude")

#' @describeIn coordinates Extract longitude coordinates from a data frame or list.
longitude.list <- function(x, ...){
   n <- tolower(names(x))
   if (is.null(n)) return(NULL)
   index <- grep("longitude", n)[1]
   if (length(index) == 0) index <- grep("long", n)[1]
   if (length(index) == 0) index <- grep("lon", n)[1]
   if (length(index) == 0) return(NULL)
   return(x[[index]])
}

#' @describeIn coordinates Extract latitude coordinates from a data frame or list.
latitude.list <- function(x, ...){
   n <- tolower(names(x))
   if (is.null(n)) return(NULL)
   index <- grep("latitude", n)[1]
   if (length(index) == 0) index <- grep("lat", n)[1]
   if (length(index) == 0) return(NULL)
   return(x[[index]])
}

# Alias functions:
#' @describeIn coordinates Alias function for \code{longitude}.
lon <- longitude

#' @describeIn coordinates Alias function for \code{latitude}.
lat <- latitude

#' @describeIn coordinates Alias function for \code{longitude}.
long <- longitude
