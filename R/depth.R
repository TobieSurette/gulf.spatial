#' Estimate Water Depth
#'
#' @description Returns an linearly interpolated estimate of water depth for a given a set
#' of coordinates, based on a reference bathymetry map.
#'
#' @details The sign of \code{longitude} has no influence on the result.
#'
#' @param longitude,latitude Numerical vector of longitudes in decimal degree format.
#'
#' @param units Depth units to be returned. May be in meters (\code{units = "meter"}), feet (\code{units
#' = "ft"} or \code{units = "feet"}) or fathoms (\code{units = "fth"} or (\code{units = "fathoms"}).
#'
#' @return Returns a numerical vector the same size as \code{latitude} and \code{longitude} containing
#' the water depth at the specified coordinates. Coordinates on land have negative values.
#'
#' @examples
#'    # Find the water depth at a single point:
#'    depth(63.8, 47.05)
#'
#'    # Find water depth for multiple points:
#'    lat <- c(48, 47, 46.5)
#'    long <- c(-64, -61.5, -62)
#'    depth(long, lat)
#'
#' @export depth
#' @export depth.default
#'
depth <- function(x, ...) UseMethod("depth")

#' @describeIn depth Default \code{depth} function.
depth.default <- function(longitude, latitude, units = "m"){
   units <- match.arg(tolower(units), c("meters", "ft", "feet", "fth", "fathoms"))

   # Load gulf bathymetry spatial grid:
   data(gulf.dem)

   # Return all bathymetry data:
   if (missing(longitude) & missing(latitude)) return(gulf.dem)

   # Argument checks:
   if (missing(longitude) | missing(latitude)) stop("'longitude' and 'latitude' must be specified.")
   if (length(longitude) != length(latitude)) stop("'longitude' and 'latitude' must be the same length.")
   if (!is.numeric(longitude) | !is.numeric(latitude)) stop("'longitude' and 'latitude' must be numeric.")

   # Remove NA values from consideration:
   v <- rep(NA, length(longitude))
   ii <- which(!is.na(longitude) & !is.na(latitude))
   longitude <- longitude[ii]
   latitude <- latitude[ii]

   # Convert to image:
   I <- list()
   I$z <- gulf.dem@data$z
   dim(I$z) <- gulf.dem@grid@cells.dim
   I$z <- I$z[, dim(I$z)[2]:1]
   wx <- gulf.dem@grid@cellsize[1]
   I$x <- seq(gulf.dem@bbox[1, 1] + wx/2, gulf.dem@bbox[1, 2] - wx/2, by = wx)
   wy <- gulf.dem@grid@cellsize[2]
   I$y <- seq(gulf.dem@bbox[2, 1] + wy/2, gulf.dem@bbox[2, 2] - wy/2, by = wy)

   # Take absolute value of latitude and longitude:
   latitude = abs(latitude)
   longitude = -abs(longitude)

   # Image dimensions:
   dz <- dim(I$z)

   # Convert 'x' and 'y' to pixel coordinates:
   xp <- (((longitude - I$x[1]) / (I$x[length(I$x)] - I$x[1])) * (length(I$x)-1)) + 1
   yp <- (((latitude - I$y[1]) / (I$y[length(I$y)] - I$y[1])) * (length(I$y)-1)) + 1

   # Initialize result variable:
   z <- rep(NA, length(xp))

   fx <- floor(xp)
   fy <- floor(yp)

   # Index of points which lie within the image bounds:
   index <- (fx >= 1) & (fx < dz[1]) & (fy >= 1) & (fy < dz[2])

   # Remove exterior points:
   fx <- fx[index]
   fy <- fy[index]

   # Calculate pixel weights:
   wx <- 1 - (xp[index] - fx)
   wy <- 1 - (yp[index] - fy)

   # Calculate weighted 'z' value:
   z[index] <- wx * wy * I$z[fy * dz[1] + fx] +
               (1-wx) * wy * I$z[fy * dz[1] + fx + 1] +
               wx * (1-wy) * I$z[(fy+1) * dz[1] + fx] +
               (1-wx) * (1-wy) * I$z[fy * dz[1] + fx + 1]

   # Convert to other units if required:
   if (units %in% c("ft", "feet"))  z <- z * 3.280839
   if (units %in% c("fth", "fathoms")) z <- z * 0.546806

   v[ii] <- -z

   return(v)
}

#' @describeIn depth Depth function for a list obejct.
depth.list <- function(x, ...){
   lon <- longitude(x)
   lat <- latitude(x)
   if (is.null(lon) | is.null(lat)) stop("Unable to extract lat-lon coordinates from object.")
   return(depth(lon, lat, ...))
}
