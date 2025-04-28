#' @title Group Sampling Stations by Location
#'
#' @description Group sampling stations by spatial proximity.
#'
#' @param longitude,latitude Numeric vector in decimal degrees.
#' @param time Numeric vector or time (\sQuote{POSIX}) objects.
#' @param distance.tolerance Distance (in kilometers) below which a pair of points is considered to belong to the same sampling station.
#' @param time.tolerance Numeric value specifying the time-difference below which a pair of points is considered to have occured at the same time.
#' @param ... Other arguments (not used).

#' @describeIn station Assign a spatial location identifier based on spatio-temporal proximity.
#'
#' @export station
station <- function(longitude, latitude, time, distance.tolerance = 1, time.tolerance, ...){
   # Check input arguments:
   if (missing(longitude) | missing(latitude)) stop("'longitude' and 'latitude' must be specified.")
   if (length(longitude) != length(latitude)) stop("'longitude' and 'latitude' vectors must be the same length.")
   if (!missing(time)) if (length(time) != length(longitude)) stop("'time' and coordinate vectors must have the same lengths.")

   # Tolerance values:
   distance.tolerance <- abs(distance.tolerance)
   if (!missing(time.tolerance)) time.tolerance <- abs(time.tolerance)

   # Check whether there are NA values in the coordinates:
   ix <- which(!is.na(longitude) & !is.na(latitude))

   # Non-NA data:
   lon <- longitude[ix]
   lat <- latitude[ix]

   # Cluster by proximate distance:
   tree <- stats::hclust(stats::dist(deg2km(lon, lat)))
   if (any(tree$height < distance.tolerance)){
      k <- min(which(rev(tree$height) < distance.tolerance))
      k <- max(k, 2)
      v <- stats::cutree(tree, k = k)
   }else{
      v <- 1:length(lon)
   }

   # Cluster by proximate time:
   if (!missing(time.tolerance)){
      tree <- stats::hclust(stats::dist(as.numeric(time) / 60))
      if (any(tree$height < time.tolerance)){
         k <- min(which(rev(tree$height) < time.tolerance))
         k <- max(k, 2)
         v <- paste(v, "-", cutree(tree, k = k))
         v <- match(v, unique(v))
      }else{
         v <- 1:length(lon)
      }
   }

   # Exapand to original data set:
   t <- rep(NA, length(longitude))
   t[ix] <- v

   return(t)
}

