km2deg <- function(x, y, long.ref = NULL, lat.ref = NULL, method = "utm"){
   # KM2DEG - Convert coordinates from kilometers to decimal lat-lon.

   # Check 'method' argument:
   method <- tolower(method)
   if (!(method %in% c("utm", "ellipsoid"))) stop("Invalid 'method' argument.")

   # Calculate coordinates using 'rgdal' package methods:
   if (method == "utm"){
      # Load 'rgdal' R Geospatial Data Abstraction Library:
      require(rgdal)

      # Adjust for reference coordinates:
      if (!is.null(long.ref) & !is.null(lat.ref)){
         ref <- deg2km(long.ref, lat.ref)
         x <- x + ref$x
         y <- y + ref$y
      }

      # Put coordinates in a data frame:
      z <- data.frame(x = 1000*x, y = 1000*y)

      # Define which variables are corodinates:
      coordinates(z) <- c("x", "y")

      # Define projection:
      proj4string(z) <- CRS("+proj=utm +zone=20 +datum=NAD83 +units=m")

      # Convert to UTM projection:
      z <- spTransform(z, CRS("+proj=longlat +datum=NAD83"))

      # Extract coordinates:
      v <- as.data.frame(z@coords)

      names(v) <- c("longitude", "latitude")
   
      return(v)
   }

   # Calculate coordinates using 'ellipsoid' method:
   if (method == "ellipsoid"){
      if (is.null(long.ref) | is.null(lat.ref))
         stop("Reference coordinates 'long.ref' and 'lat.ref' must be specified.")

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
      result <- list(longitude = long, latitude = lat)

      return(result)
   }
   
   stop("'km2deg' 'method' argument is invalid.")
}
