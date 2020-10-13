#' Display Bathymetry
#'
#' @description This function is used to display bathymetry (water depth) and isobath lines
#' on a map.  The data may be displayed in Digital Elevation Map (i.e. as an
#' image) and/or contour lines which show the lines of water depth.
#'
#' This function uses the \code{\link[graphics]{image}} function to display
#' Digital Elevation Map (DEM) and the \code{\link[graphics]{contour}} function
#' to derive the bathymetry lines from the DEM data. The DEM image changes
#' colours at each value defined by the \sQuote{breaks} argument, while contour
#' lines are drawn at each value in the \sQuote{levels} argument.
#'
#' @param dem Logical value stating whether or not the Digital Elevation Map
#' (DEM) image is to be plotted.
#'
#' @param dem.col Vector of colours to be used in drawing the Digital Elevation
#' Map. A colour scale is interpolated between the pair of specified colours,
#' or if a single colour is specified, the interpolation is made between
#' \sQuote{white} and the specified colour.
#'
#' @param breaks A numeric vector of values specifying at which bathymetric
#' values the DEM map is to change colour, as given by the interpolated colour
#' scale generated from \sQuote{dem.col}.
#'
#' @param contour Logical value stating whether or not to draw bathymetry
#' contour lines.
#'
#' @param contour.col Vector of colours to be used in drawing the bethymetry
#' contour lines. A colour scale is interpolated between the pair of specified
#' colours, or if a single colour is specified, the interpolation is made
#' between \sQuote{white} and the specified colour.
#'
#' @param contour.lwd A vector of line width(s) to be used for bathymetry
#' lines. Either one or two numeric values may be passed.
#'
#' @param contour.cex Contour label size(s) to be used for bathymetry lines.
#' Either one or two numeric values may be passed.
#'
#' @param levels A numeric vector or a list of two numeric vectors specifying
#' the levels at which contour lines are to be drawn. When a list is given, the
#' first is used to plot minor lines and the second is used to draw major
#' lines.
#'
#' @param x A numeric vector specifying the horizontal locations of the DEM
#' data specified by \code{z}. Either the length of \code{x} itself or the
#' product of \code{length(x)} and \code{length(y)} must be the same as that of
#' \code{z}.
#'
#' @param y A numeric vector specifying the vertical locations of the DEM data
#' specified by \code{z}. Either the length of \code{y} itself or the product
#' of \code{length(x)} and \code{length(y)} must be the same as that of
#' \code{z}.
#'
#' @param z A numeric vector, a numeric matrix or a
#' \sQuote{SpatialGridDataFrame} object specifying the Digital Elevation Map
#' (DEM) values to be used by the \code{bathymetry} function. This alternate
#' specification the DEM will be used instead of the internal call to
#' \code{data(gulf.dem)}.
#'
#' @param \dots Further arguments to be passed to other functions.
#'
#' @return No value is returned.
#'
#' @examples
#' # Draws the bathymetry for the current window:
#' map()
#' bathymetry()
#' coast()
#' box()
#'
#' # Draw only red contour lines at every 10 meters:
#' map()
#' bathymetry(dem = FALSE, contour.col = "red", levels = seq(-500, 0, by = 10))
#'
#' @export
bathymetry <- function(dem = TRUE,
                       dem.col = c("lightblue", "deepskyblue4"),
                       breaks = c(-5000, -4000, -3000, -2000, seq(-1000, 0, by = 50)),
                       contour = TRUE, contour.col = c("skyblue", "skyblue2"),
                       contour.lwd = c(0.4, 0.6), contour.cex = c(0.4, 0.5),
                       levels = list(minor = seq(0, -200, by = -10),
                                     major = seq(0, -5000, by = -50)),
                       x = NULL, y = NULL, z = NULL, ...){

   # Read data file:
   v <- read.gulf.spatial("bathymetry")

   # Convert 'gulf.dem' to DEM list:
   xx <- seq(v@bbox[1, 1], v@bbox[1, 2], len = v@grid@cells.dim[1])
   yy <- seq(v@bbox[2, 1], v@bbox[2, 2], len = v@grid@cells.dim[2])
   zz <- v@data[, 1]
   dim(zz) <- c(v@grid@cells.dim)
   zz <- zz[, dim(zz)[2]:1]
   v <- list(x = xx, y = yy, z = zz)

   # Define graph limits:
   xlim <- graphics::par("usr")[1:2]
   ylim <- graphics::par("usr")[3:4]

   # Truncate DEM data set to axis limits:
   index <- (xlim[1] <= v$x) &  (xlim[2] >= v$x)
   v$z <- v$z[index, ]
   v$x <- v$x[index]
   index <- (ylim[1] <= v$y) &  (ylim[2] >= v$y)
   v$z <- v$z[, index]
   v$y <- v$y[index]

   # Draw sea Digital Elevation Map:
   if (dem){
      if (length(dem.col) == 1) dem.col <- c("white", dem.col)
      temp <- v
      n <- length(breaks) - 1
      temp$z[temp$z >= 0] <- NA
      colour.scale <- grDevices::colorRamp(rev(dem.col))
      sea.colours <- grDevices::rgb(colour.scale(seq(0, 1, length.out = n)), max = 255)
      graphics::image(temp$x, temp$y, temp$z, col = sea.colours, breaks = breaks, add = TRUE)
   }

   # Draw bathymetry lines:
   if (contour){
      temp <- v
      temp$z[temp$z >= 0] <- NA
      if (is.numeric(levels)) levels = list(minor = levels)
      contour(temp, levels = levels[[1]], add = TRUE, col = contour.col[1],
              lwd = contour.lwd[1], labcex = contour.cex[1], ...)
      if ((is.list(levels)) & (length(levels) == 2)){
         if (length(contour.col == 1)) contour.col = rep(contour.col, 2)
         if (length(contour.lwd == 1)) contour.lwd = rep(contour.lwd, 2)
         if (length(contour.cex == 1)) contour.cex = rep(contour.cex, 2)
         contour(temp, levels = levels[[2]], add = TRUE, col = contour.col[2],
                 lwd = contour.lwd[2], labcex = contour.cex[2], ...)
      }
   }
}
