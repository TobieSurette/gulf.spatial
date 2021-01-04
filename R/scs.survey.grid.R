#' Identify Survey Sampling Grid
#'
#' @description Returns the survey sampling grid to which a coordinate or data observation belongs.
#'
#' @param x Data object.
#' @param longitude,latitude Vector of decimal
#'
#' @examples
#' # Single point:
#' scs.survey.grid(-63, 47.5)
#'
#' # Vector of points:
#' s <- read.scsset(2020)
#' scs.survey.grid(lon(s), lat(s))
#' scs.survey.grid(s) # Use data directly.
#'
#' # Return survey grid definitions:
#' scs.survey.grid()
#'

#' @export
scs.survey.grid <- function(x, ...) UseMethod("scs.survey.grid")

#' @rawNamespace S3method(scs.survey.grid,default)
scs.survey.grid.default <- function(longitude, latitude, ...){
   # Read survey grids:
   mif <- read.gulf.spatial(c("scs", "mif"))

   if (missing(longitude)) return(mif)

   # Convert degrees to kilometers:
   tmp <- deg2km(longitude, latitude)
   x <- tmp$x;
   y <- tmp$y

   # Identify snow crab survey grid:
   v <- rep(NA, length(longitude))
   for (i in 1:length(mif)){
      p <- as.polygon(mif[[i]]$x, mif[[i]]$y)
      v[which(in.polygon(p, x, y))] <- i
   }

   return(v)
}

#' @rawNamespace S3method(scs.survey.grid,scsset)
scs.survey.grid.scsset <- function(x, ...) return(scs.survey.grid(longitude(x), latitude(x)))

#' @export scs.survey.grids
scs.survey.grids <- scs.survey.grid
