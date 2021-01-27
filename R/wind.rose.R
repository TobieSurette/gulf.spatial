#' Draw Wind Rose
#'
#' @description Display a wind rose on a map.
#'
#' @aliases wind.rose wind rose
#' @param longitude.latitude Coordinates specifying the center location of the wind rose is to be drawn.
#' @param radius Numerical value specifying the size (as a radius) of the wind rose. The distance units are specified by \code{units}.
#' @param units Character string specifying the length units to be used when interpreting the \sQuote{radius} argument. 
#'              It may take the following values: \sQuote{user} (user coordinate system), \sQuote{in} or \sQuote{inches} 
#'              (in inches) or \sQuote{km} or \sQuote{kilometers} (in kilometers). The default is \sQuote{inches}.
#' @param pos Numerical value specifying the position of the wind rose on the graph. It may take the following values: 
#'            1 = Lower-left corner, 2 = Lower-right corner, 3 = Upper-right corner and 4 = Upper-left corner.  The 
#'            default is \code{3}.
#' @param bearing Numerical value specifying the degrees in radians that the wind rose is to be rotated counter-clockwise.
#' @param convert Whether or not to convert radius units to degree scale when units = \sQuote{km} or \sQuote{kilometers}.
#' @param cex Character expansion value for the wind rose labels.
#' 
#' @examples
#' # Gulf of Saint-Lawrence base map:
#' map()
#' wind.rose()

#' @export wind.rose
wind.rose <- function(longitude, latitude, radius = 0.3, pos = 3, bearing = 0, 
                      units = "inches", margin = 0.04, convert = TRUE, cex = 0.6, ...){

   # Default colors are white and black
   colours <- rep(c("white","black"),8)
   
   # Get axis limits:
   xlim <- graphics::par("usr")[1:2]
   ylim <- graphics::par("usr")[3:4]
   
   # Calculate aspect ratio:
   ratio <- (diff(xlim) / graphics::par("pin")[1]) / (diff(ylim) / graphics::par("pin")[2])
   
   # Calculate radii:
   if (is.element(units, c("in", "inches"))) radius <- (radius / graphics::par("pin")[1]) * diff(graphics::par("usr")[1:2])
   if (convert & is.element(units, c("km", "kilometers"))) radius <- km2deg(radius,0, 46, -66)$longitude  + 66
   radii <- rep(radius/c(1,4,2,4),4)
   
   # Define default positions if latitude and longitude are missing:
   if (missing(latitude) | missing(longitude)){
      if (pos == 1){
         latitude <-  graphics::par("usr")[3] + (1/ratio) * (margin * diff(graphics::par("usr")[3:4]) + radius * 1.3)
         longitude <- graphics::par("usr")[1] + margin * diff(graphics::par("usr")[1:2]) + radius * 1.3
      }
      if (pos == 2){
         latitude <-  graphics::par("usr")[3] + (1/ratio) * (margin * diff(graphics::par("usr")[3:4]) + radius * 1.3)
         longitude <- graphics::par("usr")[2] - (margin * diff(graphics::par("usr")[1:2]) + radius * 1.3)
      }
      if (pos == 3){
         latitude <-  graphics::par("usr")[4] - (1/ratio) * (margin * diff(graphics::par("usr")[3:4]) + radius * 1.3)
         longitude <- graphics::par("usr")[2] - (margin * diff(graphics::par("usr")[1:2]) + radius * 1.3)
      }
      if (pos == 4){
         latitude <-  graphics::par("usr")[4] - (1/ratio) * (margin * diff(graphics::par("usr")[3:4]) + radius * 1.3)
         longitude <- graphics::par("usr")[1] + margin * diff(graphics::par("usr")[1:2]) + radius * 1.3
      }
   }
   
   # Calculate
   x <- radii[(0:15)+1] * cos((0:15) * pi/8 + bearing) + longitude
   y <- (1/ratio) * radii[(0:15)+1] * sin((0:15) * pi/8 + bearing) + latitude
   
   # Draw each polygon:
   for (i in 1:15) {
      x1 <- c(x[i], x[i+1], longitude)
      y1 <- c(y[i], y[i+1], latitude)
      polygon(x1, y1, col = colours[i])
   }
   
   # Draw the last polygon:
   polygon(c(x[16], x[1], longitude), c(y[16],y[1],latitude), col = colours[16])
   # drawing letters
   b <- c("E","N","W","S")
   for (i in 0:3){
      graphics::text(radius * 1.2 * cos(bearing + i*pi/2) + longitude,
                     radius * 1.2 * (1/ratio) * sin(bearing+i*pi/2) + latitude, b[i+1],
                     adj = c(0.5, 0.5), cex = cex)
   }
}
