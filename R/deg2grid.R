#' Coordinates and Grids
#'
#' @description Functions to assign a grid label(s) given coordinates, and vice versa.
#'
#' @param x,y Coordinate vectors (e.g. decimal longitude or latitude) or grid name.
#' @param longitude,latitude Vector of decimal longitude or latitude.
#' @param xref,yref Numerical values specifying the horizontal and vertical reference coordinates. Reference coordinates specify the location of the the lower-left corner of reference grid.
#' @param dx,dy Numerical values specifying the width or height of each grid.
#' @param reference A four-character string of the form XXYY, where XX are two letters and YY are two digits.
#' @param quarter.grid A logical value specifying whether the grid is to include northing and easting qualifiers.
#'
#' @examples
#' deg2grid(-64, 47) # Returns "HD37".
#' grid2deg("HD37")  # Return corner coordinates for "HD37".
#' grid2deg("HD37", vertices = TRUE)  # Return vertex coordinates for "HD37".
#'
#' # Generate 100 random coordinates:
#' lon <- -66 + 6 * runif(100)
#' lat <- 45 + 4 * runif(100)
#' deg2grid(lon, lat)
#'
#' # Single point:
#' grid.scs(-63, 47.5)
#'
#' # Vector of points:
#' s <- read.scsset(2020)
#' grid.scs(lon(s), lat(s))
#' grid.scs(s) # Apply directly.
#'
#' # Return survey grid definitions:
#' grid.scs()
#'
#' @seealso \code{\link{plot.grid}}

#' @describeIn deg2grid Returns a grid name for a given coordinate(s).
#' @export deg2grid
deg2grid <- function(x, y,  quarter.grid = FALSE, xref = -66-1/3, yref = 45, dx = 1/6, dy = 1/6, reference = "HP23"){
   # Parse 'x':
   if (missing(y)) stop("'y' coordinates required.")
   x <- -abs(x)

   # Calculate 'x' index:
   xi <- as.numeric(substr(reference, 3, 4)) + floor(round((x - xref)/dx, 10))
   xstr <- gsub(" ", "0", formatC(xi, width = 2))

   # Calculate 'y' index:
   ystr <- num2abc(abc2num(reference) - floor(round((y-yref)/dy, 10)))
   str <- paste0(ystr, xstr)

   if (quarter.grid){
      ystr <- paste0(ystr, c("E", "W")[(((x/dx) - floor(x/dx)) >= 0.5)+1])
      xstr <- paste0(c("S", "N")[(((y/dy) - floor(y/dy)) >= 0.5)+1], xstr)
   }

   return(paste0(ystr, xstr))
}

# Internal functions:
abc2num <- function(x) return(26*(match(substr(toupper(x), 1, 1), LETTERS)-1) + match(substr(toupper(x), 2, 2), LETTERS)-1)
num2abc <- function(x) return(paste0(LETTERS[floor(x / 26) + 1], LETTERS[x %% 26 + 1]))

#' @describeIn deg2grid Returns the corner coordinates of a grid.
#' @export grid2deg
grid2deg <- function(x, xref = -66-1/3, yref = 45, dx = 1/6, dy = 1/6, reference = "HP23", vertices = FALSE){
   x <- toupper(x)

   # Calculate horizontal lower left coordinate:
   xr <- xref + dx * (as.numeric(substr(x, 3, 4)) - as.numeric(substr(reference, 3, 4)))

   # Calculate vertical lower left coordinate:
   yr <- yref - dy * (abc2num(substr(x, 1, 2)) - abc2num(reference))

   # Add corner coordinates:
   if (!vertices){
      v <- data.frame(left = xr, bottom = yr, right = xr + dx, top = yr + dy)
   }else{
      xx  <- cbind(xr, xr + dx, xr + dx, xr, NA)
      yy  <- cbind(yr, yr, yr + dy, yr + dy, NA)
      gg <- cbind(repvec(x, ncol = 4), "")
      v <- data.frame(grid = as.vector(t(gg)),
                      longitude = as.vector(t(xx)),
                      latitude = as.vector(t(yy)),
                      stringsAsFactors = FALSE)
   }

   return(v)
}


#' @describeIn deg2grid Generic method for identifying a grid from the snow crab survey spatial design.
#' @export grid.scs
grid.scs <- function(x, ...) UseMethod("grid.scs")

#' @describeIn deg2grid Default method for identifying a grid from the snow crab survey spatial design.
#' @rawNamespace S3method(grid.scs,default)
grid.scs.default <- function(longitude, latitude, ...){
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
      p <- gulf.graphics::as.polygon(mif[[i]]$x, mif[[i]]$y)
      v[which(gulf.graphics::in.polygon(p, x, y))] <- i
   }

   return(v)
}

#' @describeIn deg2grid 'scsset' method for identifying a grid from the snow crab survey spatial design.
#' @rawNamespace S3method(grid.scs,scsset)
grid.scs.scsset <- function(x, correct = TRUE, ...){
   years <- sort(unique(year(x)))

   if (length(years) > 1){
      r <- rep(NA, nrow(x))
      for (i in 1:length(years)){
         ix <- year(x) == years[i]
         r[ix] <- grid.scs(x[ix, ], correct = correct)
      }
   }else{
      # Plot survey grids:
      mif <- read.gulf.spatial(layer = c("scs", "grid", "mif"))
      for (i in 1:length(mif)){
         tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
         mif[[i]]$longitude <- tmp$longitude
         mif[[i]]$latitude <- tmp$latitude

         p <- gulf.graphics::as.polygon(mif[[i]]$longitude, mif[[i]]$latitude)
         ix <- gulf.graphics::in.polygon(p, lon(x), lat(x))
         mif[[i]]$tow.id <- x$tow.id[ix]
      }

      if (correct){
         #map.new()
         #rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "lightblue")
         #map("coast")
         #for (i in 1:length(mif)){
         #   polygon(mif[[i]]$longitude, mif[[i]]$latitude)
         #}

         # Survey grids with no survey tows:
         empties <- which(unlist(lapply(mif, function(x) length(x$tow.id) == 0)))

         # Survey grids with multiple survey tows:
         multiples <- which(unlist(lapply(mif, function(x) length(x$tow.id) > 1)))

         #for (i in 1:length(empties)){
         #   polygon(mif[[empties[i]]]$longitude, mif[[empties[i]]]$latitude, col = "red")
         #}


         #for (i in 1:length(multiples)){
         #   polygon(mif[[multiples[i]]]$longitude, mif[[multiples[i]]]$latitude, col = "green")
         #}

         # Assign tows which are not in any grid to an empty grid:
         ix <- which(!(x$tow.id %in% unique(unlist(lapply(mif, function(x) x$tow.id)))))
         if (length(ix) > 0){
            for (i in 1:length(ix)){
               lons <- unlist(lapply(mif[empties], function(x) mean(x$longitude[1:4])))
               lats <- unlist(lapply(mif[empties], function(x) mean(x$latitude[1:4])))
               d <- distance(lon(x)[ix[i]], lat(x)[ix[i]], lons, lats)
               if (min(d) < 15){
                  mif[[empties[which.min(d)]]]$tow.id <- x$tow.id[ix[i]]
                  empties <- setdiff(empties, empties[which.min(d)])
               }
            }
         }

         # Separate grids with multiple tows:
         for (i in 1:length(multiples)){
            lons <- unlist(lapply(mif[empties], function(x) mean(x$longitude[1:4])))
            lats <- unlist(lapply(mif[empties], function(x) mean(x$latitude[1:4])))
            ix <- which(x$tow.id %in% mif[[multiples[i]]]$tow.id)
            d <- distance(lon(x)[ix], lat(x)[ix], lons, lats)
            if (any(as.numeric(d) < 15)){
               iy <- which.min(apply(d, 1, min))

               mif[[empties[which.min(d[iy, ])]]]$tow.id <- mif[[multiples[i]]]$tow.id[iy]
               mif[[multiples[i]]]$tow.id <- setdiff(mif[[multiples[i]]]$tow.id, mif[[multiples[i]]]$tow.id[iy])
               empties <- setdiff(empties, empties[which.min(d[iy, ])])
            }
         }
      }

      # Assign survey grid indices to each tow:
      r <- rep(NA, nrow(x))
      for (i in 1:length(mif)){
         ix <- (x$tow.id %in% mif[[i]]$tow.id)
         r[ix] <- i
      }
   }

   return(r)
}

