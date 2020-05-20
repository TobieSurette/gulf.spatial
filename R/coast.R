#' Display Gulf Coast
#'
#' @description Displays the coast and landmass surrounding the Gulf of Saint Lawrence.
#'
#' @param resolution Character string specifying the resolution of the
#' coastline, topographical and bathymetric data file to be loaded. It may take
#' the following values: \sQuote{low} (low resolution), \sQuote{intermediate} (intermediate resolution),
#' \sQuote{high} (high resolution), or \sQuote{full} (full resolution).
#'
#' @param col The colour to be used for in-filling the landmass. If
#'
#' @param border The border colour to be used for displaying the coastline.
#'
#' @param lwd Border line width (see \code{\link[graphics]{par}}).
#'
#' @param \dots Further arguments to be passed to the \code{\link[graphics]{polygon}} function.
#'
#' @return A spatial object containing the coastline coordinates is returned invisibly.
#'
#' @seealso \code{\link[graphics]{polygon}}
#'
#' @examples
#' # Display simple map:
#' plot(c(-66.5, -60), c(45, 49), type = "n")
#' coast()
#'
#' # Read coastline data:
#' x <- coast()
#'
#' @export coast
#' @export coastline
#'
coast <- function(resolution = "high", col = 'grey80', border = 'grey50', lwd = 0.4, ...){

   resolution <- match.arg(tolower(resolution), c("low", "intermediate", "high", "full"))

   # Load coastline data file:
   file <- paste0("data/gulf.coast.", substr(resolution, 1, 1), ".rda")
   load(file)

   # Display:
   graphics::polygon(x, col = col, border = border, lwd = lwd, ...)

   invisible(x)
}

#' @describeIn coast Alias for \code{coast}
coastline <- coast

