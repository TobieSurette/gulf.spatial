#' Retrieve Coordinates
#'
#' @description Functions to retrieve latitude-longitude coordinates from an object.
#'
#' @param x An object.
#'
#' @export longitude
#' @export latitude
longitude <- function(x, ...) UseMethod("longitude")

#' @describeIn longitude Generic \code{latitude} function.
latitude <- function(x, ...) UseMethod("latitude")
