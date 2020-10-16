#' Calculate Bounding Box
#'
#' @description Functions to calculate bounding box of an object.
#'
#' @examples
#' x <- c(0, 0, 0.5, 1, 1, 0)
#' y <- c(0, 1, 0.5, 1, 0, 0)
#' bbox(x, y) # Calculate area of polygon.
#'

#' @export
bbox <- function(x, ...) UseMethod("bbox")

#' @describeIn bbox Calculate the bounding box coordinates for a set of coordinate points.
#' @export
bbox.default <- function(x, y, ...) return(data.frame(x = range(x, na.rm = TRUE), y = range(y, na.rm = TRUE)))

#' @describeIn bbox Calculate surface area of a polygon using its vertex coordinates.
#' @export
bbox.polygon <- function(p, ...){
   x <- unlist(lapply(p, function(x) x$x))
   y <- unlist(lapply(p, function(x) x$y))
   return(bbox(x,y))
}
