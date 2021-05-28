#' Draw Axes on a Map
#'
#' @description Draws axes on a map.
#'
#' @param side An integer(s) specifying which side(s) of the plot the axis is to be drawn on. The axis is placed
#'             as follows: 1=below, 2=left, 3=above and 4=right.
#' @param at A vector in decimal degrees specifying the points at which tick-marks are to be drawn.
#' @param major.interval A numerical scalar specifying the modulus which define where major tick marks lie.
#'                       The default value is 1.
#' @param major.cex A numerical value specifying the major tick mark character expansion value.
#' @param minor.cex A numerical value specifying the minor tick mark character expansion value.
#' @param \dots Further parameters passed on to the \code{\link[graphics]{axis}} function.
#'
#' @seealso \code{\link{map}}, \code{\link[graphics]{axis}}
#' map.new()
#' map.axis()
#'
#' @export map.axis
map.axis <- function(side = 1, at, major.interval = 1, major.cex = 0.6, minor.cex = 0.4, cex = 1, language = "english", ...){
   # Loop over multiple axes:
   if (length(side) > 1){
      for (i in 1:length(side)) map.axis(side = side[i], at = at, major.interval = major.interval, major.cex = major.cex, minor.cex = minor.cex, ...)
   }else{
      # Define 'padj' parameter:
      padj <- switch(as.character(side), "1" = c(-2.5, -4), "2" = c(1, 1.5), "3" = c(1.5, 2), "4" = c(-2.5, -4))

      # Define 'at' argument if missing:
      if (missing(at)) at <- graphics::axTicks(side)

      # Define index of major axis labels:
      index <- (abs(at) %% major.interval) == 0

      # Reset 'padj' values if cex values are changed:
      if ((major.cex != 0.6) | (minor.cex != 0.4)) padj <- 0

      # Major tick labels:
      if (sum(index) > 0){
         if (side %in% c(1, 3)) lab <- deg2str(long = at[index], ...)
         if (side %in% c(2, 4)) lab <- deg2str(lat = at[index], ...)
         graphics::axis(side, at = at[index], labels = lab, cex.axis = cex * major.cex, padj = padj[1], ...)
      }

      # Minor tick labels:
      if (sum(!index) > 0){
         temp <- (abs(at[!index]) %% 1) * 60
         temp <- round(temp, 4)
         lab <- paste(temp, "'", sep = "")
         graphics::axis(side, at = at[!index], labels = lab, cex.axis = cex * minor.cex, padj = padj[2],  ...)
      }
   }
}
