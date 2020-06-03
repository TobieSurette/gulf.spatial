#' Map Gulf of St. Lawrence Features
#'
#' @description Draws southern Gulf of St. Lawrence geographical features.
#'
#' @param layer Layer specification
#'
#' @seealso \code{\link{read.gulf.spatial}}
#'
#' @examples
#' map()
#' map(layer = "strata", survey = "rv")
#'
#' @export map
#' @export map.default
#'
map <- function(x, ...) UseMethod("map")

#' @describeIn map Default map method.
map.default <- function(x, ...){
   v <- read.gulf.spatial(x, ...)

   plot(v, ...)
}
