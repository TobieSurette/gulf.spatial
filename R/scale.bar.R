#' Draw Scale Bar
#'
#' @description Display a scale bar on a map.
#'
#' @param longitude A numerical value specifying the longitude where the scale bar is to be drawn.
#' @param latitude A numerical value specifying the latitude where the scale bar is to be drawn.
#' @param length A numerical value specifying the length of the scale bar to be drawn. The units are specified by the \code{units} argument.
#' @param units A character string specifying the length units to be used. At present, the only options are \sQuote{km} or \sQuote{kilometers}.
#' @param convert A logical value specifying whether or not to convert the \code{length} argument to the degree scale.
#' @param number A numerical value specifying the number of intervals to include on the scale bar.
#' @param double A logical value specifying whether the scale bar is to be composed of a double or single bar.
#' @param thickness A numerical value specifying the thickness of the scale bar in inches.
#' @param pos A numerical value specifying the position of the scale bar. It may take the following values: 1 = Lower-left corner, 
#'            2 = Lower-right corner, 3 = Upper-right corner and 4 = Upper-left corner.
#' @param margin A numerical value specifying the margin of separation between scale bar and the axis borders in inches. 
#'               It is used in conjunction with the \code{pos} argument.
#' @param cex Character expansion value for the scale bar labels.
#' @param colours Scale bar colours. 
#' 
#' @examples
#' map.new() # Create new window.
#' map("coast")    # Plot empty map.
#' scale.bar()
#'
#' @export scale.bar
scale.bar <- function(longitude, latitude, length = 100, units = "km", convert = TRUE, number = 4, double = TRUE,
                      thickness = 0.05, pos = 1 , margin = 0.25, cex = 0.6, colours = "black", ...){
   
   # Convert scale bar length units to degrees:
   if (convert){
      if (units == "km") len.deg <- km2deg(length, 0, -66, 46)$longitude + 66
   }else{
      len.deg <- length
   }
   
   # Parse 'colours':
   if (length(colours) == 1) colours <- c("white", colours)
   
   # Get axis limits:
   xlim <- par("usr")[1:2]
   ylim <- par("usr")[3:4]
   
   # Convert line thickness from inches to user units:
   thickness <- (thickness/par("pin")[2])*diff(ylim)
   
   # Define default positions if latitude and longitude are missing:
   if (missing(latitude) | missing(longitude)){
      # Convert margin widths from inches to user units:
      margin.latitude <- (margin / par("pin")[2]) * diff(ylim)
      margin.longitude <- (margin / par("pin")[1]) * diff(xlim)
      if (pos == 1){
         latitude <- par("usr")[3] + margin.latitude
         longitude <- par("usr")[1] + margin.longitude
      }
      if (pos == 2){
         latitude <- par("usr")[3] + margin.latitude
         longitude <- par("usr")[2] - margin.longitude - len.deg - strwidth(units, cex = cex)
      }
      if (pos == 3){
         latitude <- par("usr")[4] - margin.latitude - 1.3*thickness - strheight("1", cex = cex)
         longitude <- par("usr")[2] - margin.longitude - len.deg - strwidth(units, cex = cex)
      }
      if (pos == 4){
         latitude <- par("usr")[4] - margin.latitude - 1.3*thickness - strheight("1", cex = cex)
         longitude <- par("usr")[1] + margin.longitude
      }
   }
   
   # Determine positions of scale bar breaks:
   x <- c(seq(0, len.deg, length.out = (number+ 1))) + longitude
   y <- c(0, thickness, 1.3*thickness) + latitude
   
   # Draw scale bar:
   if (!double){
      for (i in 1:number) rect(x[i],y[1],x[i+1],y[2], col = colours[((i-1) %% 2) + 1])
      for (i in 1:(number+1)) segments(x[i],y[2],x[i],y[3])
   }else{
      for (i in 1:number) rect(x[i],y[1],x[i+1],y[1] + (diff(y[1:2])/2), col = colours[((i-1) %% 2) + 1])
      for (i in 1:number) rect(x[i],y[1] + (diff(y[1:2])/2),x[i+1],y[2], col = colours[(i %% 2) + 1])
      for (i in 1:(number+1)) segments(x[i],y[2],x[i],y[3])
   }
   
   # Scale bar labels:
   labels <- as.character(c(seq(0, length, length.out = (number + 1))))
   text(x[1:(number + 1)], y[3], labels = labels, cex = cex, pos = 3, offset = 0.2)
   if (units == "km") text(x[number + 1], mean(y[1:2]), labels = "km", pos = 4, cex = cex, offset = 0.2)
}
