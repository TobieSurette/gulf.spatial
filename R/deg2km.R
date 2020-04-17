deg2km <- function(x, y, long.ref = NULL, lat.ref = NULL, method = "utm"){
   # DEG2KM - Convert lat-lon coordinates from decimal degrees to kilometers.

   # Check 'method' argument:
   method <- tolower(method)
   if (!(method %in% c("utm", "ellipsoid"))) stop("Invalid 'method' argument.")
   
   # Calculate coordinates using 'rgdal' package methods:
   if (method == "utm"){
      # Load 'rgdal' R Geospatial Data Abstraction Library:
      require(rgdal)

      # Put coordinates in a data frame:
      x <- data.frame(longitude = x, latitude = y)

      # Define which variables are corodinates:
      coordinates(x) <- c("longitude", "latitude")

      # Define projection:
      proj4string(x) <- CRS("+proj=longlat +datum=NAD83")

      # Convert to UTM projection:
      x.utm <- spTransform(x , CRS("+proj=utm +zone=20 +datum=NAD83 +units=m"))

      # Extract coordinates:
      v <- as.data.frame(x.utm@coords)

      # Rename coordinate columns:
      names(v) <- c("x", "y")

      # Convert from meters to kilometers:
      v <- v / 1000
      
      # Adjust for reference coordinates:
      if (!is.null(long.ref) & !is.null(lat.ref)){
         ref <- deg2km(long.ref, lat.ref)
         v$x <- v$x - ref$x
         v$y <- v$y - ref$y
      }
      
      return(v)
   }
      
   # Calculate coordinates using 'ellipsoid' method:
   if (method == "ellipsoid"){
      if (is.null(long.ref) | is.null(lat.ref))
         stop("Reference coordinates 'long.ref' and 'lat.ref' must be specified.")

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
      result <- data.frame(x = -xx, y = yy)

      return(result)
   }
   
   stop("'deg2km' 'method' argument is invalid.")
}
