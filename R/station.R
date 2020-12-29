#' Return a Station Number
#'
#' @description Assign or retrieve a sampling station identifier.
#'
#' @param longitude,latitude Coordinates in decimal degrees.
#' @param time Numeric or (\sQuote{POSIX}) time objects.
#' @param distance.tolerance Numeric value specifying the distance (in kilometers) below which a pair of points
#'                           is considered to belong to the same group or station.
#' @param time.tolerance Numeric value specifying the time-difference below which a pair of points
#'                       is considered to belong to the same group or station. The units are in minutes
#'                       if the \code{time} argument belongs to a \sQuote{POSIX} class.

# @describeIn station Generic 'station' function.
#' @export
station <- function(x, ...){
   UseMethod("station")
}

#' @describeIn station Assign a spatial location identifier based on spatio-temporal proximity.
#' @export
station.default <- function(longitude, latitude, time, distance.tolerance = 1, time.tolerance, ...){
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

#' @describeIn station Return Northumberland Strait survey sampling station identifier.
#' @export
station.nssset <- function(x, tolerance, method = "observed"){
   # Check 'method' argument:
   method <- match.arg(tolower(method), c("observed", "latlong"))

   # Fetch nearest master station:
   if (method == "latlong"){
      # Load NS master station list:
      s <- read.gulf.spatial("nss stations")

      # Calculate all pairwise distances between 'x' and 'y' and the complete NS master station list:
      lon <- -dmm2deg(lon(x))
      lat <- dmm2deg(lat(x))
      d <- distance(lon, lat, s$longitude, s$latitude, pairwise = TRUE)

      # Find the minimum distance in each row:
      min.d <- apply(d, 1, min)

      # Create matrix of repeated vectors
      index <- gulf.utils::repvec(min.d, ncol = nrow(s)) == d

      # Identify the index of each station:
      index <- apply(index, 1, function(x) which(x)[1])

      # Extract station numbers from the NS master station list:
      v <- gulf.utils::deblank(s$station.number[index])

      # Set results with minimum distances beyond the threshold value to NA:
      if (!missing(tolerance)){
         tolerance <- abs(tolerance)
         v[min.d > tolerance] <- NA
      }
   }

   # Fetch observed value:
   if (method == "observed") v <- x$station

   return(v)
}
