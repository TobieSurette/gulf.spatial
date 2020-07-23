#' Degree and Kilometer Conversion
#'
#' @description Convert geographical coordinates to kilometers and vice versa.
#'
#' @param x,y A numeric vector of longitude and latitude coordinates. Coordinates are assumed
#' to be in decimal degree format.
#'
#' @param long.ref,lat.ref Reference coordinate from which the coordinate
#' distances will be calculated. Assumed to be in decimal degree format.
#'
#' @param method Character string specifying the method to be used for calculating the distances
#'               between coordinate points. The options are \sQuote{utm} which first converts the
#'               coordinates into UTM NAD83 coordinates, and \sQuote{ellipsoid} which models the
#'               Earth as an ellipsoid with major and minor radii of 6378.16 and 6356.775 kilomters,
#'               respectively. The default is \sQuote{utm}.
#'
#' @return A data frame containing the converted coordinates.
#'
#' @seealso loran2deg deg2dms deg2dmm deg2str
#'
#' @examples
#' # Convert a decimal degree coordinate to kilometers format:
#' deg2km(-63.123, 47.56)
#'
#' # Convert a vector of decimal degree coordinates to kilometer format:
#' deg2km(seq(-65, -63, len = 21), seq(46, 48, len = 21))
#'
#' #Convert a coordinate in kilometer format to decimal degree format:
#' km2deg(-227.3639, 111.1331)
#'
#' # Return the value of the reference coordinate:
#' km2deg(0, 0)
#'
#' # Convert a vector of kilometer-format coordinates to decimal degree format:
#' km2deg(seq(0, -200, len = 21), seq(0, 100, len = 21))
#'
#' @export
#'
deg2km <- function(x, y, long.ref, lat.ref, method = "utm"){

   method <- match.arg(tolower(method), c("utm", "ellipsoid"))

   # Calculate coordinates using 'rgdal' package methods:
   if (method == "utm"){
      # Load 'rgdal' R Geospatial Data Abstraction Library:
      # require(rgdal)

      # Put coordinates in a data frame:
      x <- data.frame(longitude = x, latitude = y)

      # Define which variables are coordinates:
      sp::coordinates(x) <- c("longitude", "latitude")

      # Define projection:
      sp::proj4string(x) <- sp::CRS("+proj=longlat +datum=NAD83")

      # Convert to UTM projection:
      x.utm <- sp::spTransform(x , sp::CRS("+proj=utm +zone=20 +datum=NAD83 +units=m"))

      # Extract coordinates:
      v <- as.data.frame(x.utm@coords)

      # Rename coordinate columns:
      names(v) <- c("x", "y")

      # Convert from meters to kilometers:
      v <- v / 1000

      # Adjust for reference coordinates:
      if (!missing(long.ref) & !missing(lat.ref)){
         ref <- deg2km(long.ref, lat.ref)
         v$x <- v$x - ref$x
         v$y <- v$y - ref$y
      }
   }

   # Calculate coordinates using 'ellipsoid' method:
   if (method == "ellipsoid"){
      if (missing(long.ref) | missing(lat.ref)) stop("Reference coordinates 'long.ref' and 'lat.ref' must be specified.")

      const <- sqrt (6378.16 * 6356.775) / 57.29578

      # Transform degrees to radians:
      y <- y * pi / 180
      x <- x * pi / 180
      lat.ref <- lat.ref * pi / 180
      long.ref <- long.ref * pi / 180

      # Calculate y coordinates:
      zzx <- sin(y) * sin(y) + (cos(y) * cos(y) * cos(x-long.ref))
      zzx <- acos(zzx)
      zzx <- zzx * 180 / pi  # Tranform to degrees.
      xx <- zzx * const
      xx <- xx * sign(long.ref-x)

      # Calculate y coordinates:
      zzy <- sin(y) * sin(lat.ref) + (cos(y) * cos(lat.ref))
      zzy <- acos(zzy)
      zzy <- zzy * 180 / pi # Tranform to degrees.
      yy <- zzy * const
      yy <- yy * sign(y-lat.ref)

      # Store result as a list:
      v <- data.frame(x = -xx, y = yy)
   }

   return(v)
}

#' @describeIn deg2km Convert coordinates from kilometers to decimal lat-lon.
#' @export
km2deg <- function(x, y, long.ref, lat.ref, method = "utm"){

   method <- match.arg(tolower(method), c("utm", "ellipsoid"))

   # Calculate coordinates using 'rgdal' package methods:
   if (method == "utm"){
      # Load 'rgdal' R Geospatial Data Abstraction Library:
      # require(rgdal)

      # Adjust for reference coordinates:
      if (missing(long.ref) & missing(lat.ref)){
         ref <- deg2km(long.ref, lat.ref)
         x <- x + ref$x
         y <- y + ref$y
      }

      # Put coordinates in a data frame:
      z <- data.frame(x = 1000*x, y = 1000*y)

      # Define which variables are corodinates:
      sp::coordinates(z) <- c("x", "y")

      # Define projection:
      sp::proj4string(z) <- CRS("+proj=utm +zone=20 +datum=NAD83 +units=m")

      # Convert to UTM projection:
      z <- sp::spTransform(z, CRS("+proj=longlat +datum=NAD83"))

      # Extract coordinates:
      v <- as.data.frame(z@coords)

      names(v) <- c("longitude", "latitude")

      return(v)
   }

   # Calculate coordinates using 'ellipsoid' method:
   if (method == "ellipsoid"){
      if (missing(long.ref) | missing(lat.ref)) stop("'long.ref' and 'lat.ref' reference coordinates must be specified.")

      # Transform reference coordinates into radians:
      long.ref <- (long.ref / 180) * pi
      lat.ref <- (lat.ref / 180) * pi

      # Earth's radius at 45 degree latitude:
      radius <- sqrt(6378.16 * 6356.775);

      # Calcultate latitude in radians:
      yp <- ((y / radius) + lat.ref);

      # Calculate longitude in radians:
      xp <- acos((cos(x / radius) - (sin(yp))^2) / ((cos(yp))^2)) + long.ref

      # Transform coordinates into degrees:
      long <- (xp * 180) / pi
      lat <- (yp * 180) / pi

      # Store result as a list:
      v <- list(longitude = long, latitude = lat)
   }

   return(v)
}
