#' Map Gulf of St. Lawrence Features
#'
#' @description Draws southern Gulf of St. Lawrence geographical features.
#'
#' @param layer Layer specification
#'
#' @seealso \code{\link{read.gulf.spatial}}
#'
#'
map <- function(x, ...) UseMethod("map")

#' @describeIn map Default map method.
map.default <- function(x, ...){
   v <- read.spatial(x, ...)

   plot(v, ...)
}
