#' Retrieve Coordinates
#'
#' @description Functions to retrieve coordinates from an object.
#'
#' @param x An object.
#'
#' @export coordinates
#' @export longitude
#' @export latitude
coordinates <- function(x, ...) UseMethod("coordinates")

#' @describeIn coordinates Generic \code{longitude} function.
longitude <- function(x, ...) UseMethod("longitude")

#' @describeIn coordinates Generic \code{latitude} function.
latitude <- function(x, ...) UseMethod("latitude")
