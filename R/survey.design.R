#' Generate Survey Sampling Stations
#'
#' @description Returns a set of randomly-selected snow crab survey sampling stations.
#'              Each station is drawn from a square grid, a lattice which overlays the
#'              survey area. Longitude and latitude coordinates of the survey polygon's
#'              vertices are converted to UTM coordinates (NAD83). A square grid lattice is
#'              overlaid over the survey area. The dimensions of the grid vary according to
#'              number of required sampling stations and the area of the survey polygon.
#'
#' @param n Integer specifying the desired number of sampling stations.
#'
#' @param stations Data frame specifying the set of survey stations to be sampled.
#'
#' @param longitude,latitude Numeric vector specifying, in decimal degrees, the
#'    longitude coordinate of each vertex of the survey polygon, which specifies
#'    the survey area. Multiple, disjoint polygons may be separated by NA values.
#'    If not specified, then the snow crab survey polygon is used.
#'
#' @param hole Logical vector. When multiple polygons are specified in
#'    \code{longitude} and \code{latitude}, then \code{hole} may be used to
#'    specify if some are \sQuote{holes}. The length of \code{hole} may be the
#'    length of the coordinate vectors or the number of polygons defined within
#'    them.
#'
#' @param alternates Integer specifying the number of additional, alternate sampling stations
#'                   per grid to be generated.  The default value is zero.
#'
#' @param grids Logical value specifying whether the grid coordinates are to be
#'    included in the output for the snow crab survey. Alternatively, the output from a
#'    previous call of \code{scs} may be used. Specifically, the grid
#'    definitions specified in the \code{grids} element of the output is used to generate
#'    points within each element. Alternately, if the complete of grids which overlay
#'    the sGSL survey area is dersired, set \code{grids = \sQuote{complete}}
#'    or\code{\sQuote{all}}. The default value is \code{FALSE}.
#'
#' @param type Character string specifying the 'type' label to be assigned in the resulting sample.
#'
#' @return If \code{grids} is \code{FALSE}, a data frame is returned with
#'    sampling station coordinates (\code{longitude} and \code{latitude}), the
#'    station type (whether the station is a primary or alternate station) and a
#'    grid label which identifies from which grid the sample was drawn.
#'
#'    If \code{grids} is \code{TRUE}, then a two-element list is returned. The
#'    first element contains the station table described above, while the second
#'    element contains a list defining the coordinates of each grid vertex as well
#'    as the grid label.
#'
#' @examples
#' # Snow crab survey:
#' x <- scs(355) # Generate 355 survey stations.
#' x <- scs(355, alternates = 2) # Include 2 alternate station in the output.
#' x <- scs(355, alternates = 2, grids = TRUE) # Include grids in the export.
#'
#' # Northumberland Strait survey:
#' x <- nss(100) # Generate 100 survey stations.
#' x <- nss(80, alternates = 2) # Generate 80 x survey stations plus 2 alternates sets

#' @describeIn survey.design Snow crab survey station design.
#' @export scs
scs <- function(n, longitude, latitude, hole = NULL, alternates = 0, grids = FALSE){
   # Parse 'grids' argument:
   include.grids <- FALSE
   if (is.logical(grids) & length(grids) == 1) include.grids <- grids
   complete <- FALSE
   if (is.character(grids)){
      include.grids <- TRUE
      args <- c("full", "complete", "all")
      grids <- match.arg(tolower(grids), args)
      if (grids %in% args) complete <- TRUE else stop("Invalid 'grid' value.")
   }

   # Parse 'grids' variable:
   if (is.list(grids)){
      if ("grids" %in% names(grids)) grids <- grids$grids
      if (!all(c("x", "y") %in% tolower(names(grids[[1]])))){
         if (all(c("longitude", "latitude") %in% tolower(names(grids[[1]])))){
            for (j in 1:length(grids)){
               tmp <- deg2km(grids[[j]]$longitude, grids[[j]]$latitude)
               grids[[j]]$x <- tmp$x
               grids[[j]]$y <- tmp$y
            }
         }else{
            grids <- NULL
         }
      }
   }

   # Set 'n' to the number of grids being provided:
   if (missing(n) & is.list(grids)) n <- length(grids)
   if (missing(n)) stop("'n' must be specified.")

   # Check 'n' argument:
   if (missing(n) | (length(n) != 1) | !is.numeric(n)) stop("'n' must be a positive integer.")
   if (is.list(grids)) if (n > length(grids)) stop("Number 'n' may not exceed the number of specified grids.")

   # Load default polygon if none is specified:
   if (missing(longitude) | missing(latitude)){
      gulf <- read.gulf.spatial(layer = "bounds", survey = "scs")
      longitude <- gulf$longitude
      latitude <- gulf$latitude
   }
   longitude <- -abs(longitude) # Make sure that longitudes have negative signs.

   # Create a survey polygon in 'utm' coordinates (in kilometers):
   poly <- gulf.spatial::deg2km(longitude, latitude)
   poly <- gulf.graphics::as.polygon(poly$x, poly$y, hole)

   # Calculate area of survey polygon:
   area <- sum(gulf.graphics::area(poly))

   # Draw samples from grid definitions:
   if (is.list(grids)){
      if (all(c("longitude", "latitude") %in% names(grids[[1]]))){
         xx <- matrix(nrow = length(grids), ncol = 5);
         yy <- xx
         labels <- rep(NA, length(grids))
         for (i in 1:length(grids)){
            xx[i,] <- grids[[i]]$x
            yy[i,] <- grids[[i]]$y
            labels[i] <- grids[[i]]$label
         }
      }
      index <- rep(FALSE, nrow(xx))
      temp <- data.frame(x = rep(NA, n), y = rep(NA, n), grid = NA)
      while (n > 0){
         # Draw candidate draws:
         cx <- xx[!index,1] + stats::runif(sum(!index)) * (xx[!index,3] - xx[!index,1])
         cy <- yy[!index,1] + stats::runif(sum(!index)) * (yy[!index,2] - yy[!index,1])
         v <- which(gulf.graphics::in.polygon(poly, cx, cy))
         if (length(v) > 1) ci <- sample(v) else ci <- v
         if (length(ci) > 0){
            if (any(!gulf.graphics::in.polygon(poly, cx[ci], cy[ci]))) print(cx[ci])
            ci <- ci[1:min(n, length(ci))]
            cx <- cx[ci]
            cy <- cy[ci]
            cl <- labels[which(!index)[ci]]
            index[which(!index)[ci]] <- TRUE
            ii <- min(which(is.na(temp$x))):(min(which(is.na(temp$x)))+length(cx)-1)
            if (any(!gulf.graphics::in.polygon(poly, cx, cy))) print(NA)
            temp$x[ii] <- cx
            temp$y[ii] <- cy
            temp$grid[ii] <- cl
            n <- sum(is.na(temp$x))
         }
      }
      temp$type <- "Primary"

      # Convert station coordinates to latitude-longitude:
      res <- gulf.spatial::km2deg(temp$x, temp$y)
      res$type <- temp$type
      res$grid <- temp$grid

      # Match original grid order:
      res <- res[order(match(res$grid, labels)), ]
      rownames(res) <- NULL
   }else{
      # Calculate dimension of square grids:
      d <- sqrt(area / n)
      cat(paste("Polygon area is", round(area, 4), " square kilometers.\n"))
      cat(paste("Square grid dimension is", round(d, 4), "kilometers.\n"))

      # Find bounding box for survey polygon:
      bb <- c(min(poly[[1]]$x, na.rm = TRUE), min(poly[[1]]$y, na.rm = TRUE),
              max(poly[[1]]$x, na.rm = TRUE), max(poly[[1]]$y, na.rm = TRUE))

      # Expand bounding box so that top-right coordinate matches grid scale:
      bb[3] <- bb[1] + d*(floor((bb[3]-bb[1]) / d) + 1)
      bb[4] <- bb[2] + d*(floor((bb[4]-bb[2]) / d) + 1)

      # Define coordinates of superimposed grid lattice:
      x <- seq(bb[1], bb[3], by = d)
      y <- seq(bb[2], bb[4], by = d)
      xx <- as.vector(gulf.utils::repvec(x, nrow = length(y)))
      yy <- as.vector(gulf.utils::repvec(y, ncol = length(x)))

      # Simulate stations:
      flag <- FALSE  # Termination flag.
      i <- 0
      while (!flag){
         temp <- list()
         temp$x <- xx + d*stats::runif(length(xx))
         temp$y <- yy + d*stats::runif(length(yy))
         index <- which(gulf.graphics::in.polygon(poly, temp$x, temp$y))
         if (length(index) == n) flag <- TRUE
         if (flag){
            # Remove points outside the survey polygon:
            temp$x <- temp$x[index]
            temp$y <- temp$y[index]

            # Assign 'Primary' label to point:
            temp$label <- rep("Primary", n)

            # Assign grid identifier to generated point:
            temp$grid <- deg2grid(temp$x, temp$y, xref = bb[1], yref = bb[2], dx = d, dy = d)

         }
      }

      # Convert station coordinates to latitude-longitude:
      coords <- gulf.spatial::km2deg(temp$x, temp$y)
      coords$type <- temp$label
      coords$grid <- temp$grid
      res <- coords

      # Subset of selected lower-left grid coordinates:
      xx <- xx[index]
      yy <- yy[index]

      # Convert to 'grids' format:
      # Convert corner coordinates to latitude-longitude:
      a1 <- gulf.spatial::km2deg(xx, yy)
      a2 <- gulf.spatial::km2deg(xx, yy+d)
      a3 <- gulf.spatial::km2deg(xx+d, yy+d)
      a4 <- gulf.spatial::km2deg(xx+d, yy)

      # Assign grid identifier to each grid using mid-point:
      grid.names <- deg2grid(xx+d/2, yy+d/2, xref = bb[1], yref = bb[2], dx = d, dy = d)

      # Concatenate grid corner coordinates:
      xxx <- cbind(a1$longitude, a2$longitude, a3$longitude, a4$longitude, a1$longitude)
      yyy <- cbind(a1$latitude, a2$latitude, a3$latitude, a4$latitude, a1$latitude)

      # Contruct list of polygons defining grid cell coordinates and label:
      temp <- vector("list", n)
      for (i in 1:n){
         temp[[i]]$longitude <- xxx[i, ]
         temp[[i]]$latitude  <- yyy[i, ]
         temp[[i]]$label <- grid.names[i]
      }
      grids <- temp

      # Sort grid data:
      grids <- grids[order(res$grid)]
      res <- res[order(res$grid), ]
      rownames(res) <- NULL
   }

   # Generate alternate stations:
   if (alternates > 0){
      for (j in 1:alternates){
         temp <- scs(longitude = longitude, latitude = latitude, hole = hole, grids = grids)
         temp$type <- paste0("Alternate ", j)
         res <- rbind(res, temp)
      }
   }

   # Define grid coordinates:
   if (include.grids){
      if (complete){
         # Define coordinates of superimposed grid lattice:
         x <- seq(bb[1]-d, bb[3]+d, by = d)
         y <- seq(bb[2]-d, bb[4]+d, by = d)
         xx <- as.vector(repvec(x, nrow = length(y)))
         yy <- as.vector(repvec(y, ncol = length(x)))

         xx <- cbind(xx, xx, xx+d, xx+d)
         yy <- cbind(yy, yy+d, yy+d, yy)

         # Check which grids are within the survey area using a finer grid:
         gx <- seq(bb[1], bb[3]+0.5, by = 0.5)
         gy <- seq(bb[2], bb[4]+0.5, by = 0.5)
         gxx <- as.vector(repvec(gx, nrow = length(gy)))
         gyy <- as.vector(repvec(gy, ncol = length(gx)))

         index <- gulf.graphics::in.polygon(poly, gxx, gyy)
         g.names <- deg2grid(gxx, gyy, xref = bb[1], yref = bb[2], dx = d, dy = d)
         tmp <- data.frame(x = gxx, y = gyy, grid = g.names, inside = index, stringsAsFactors = FALSE)
         tmp <- aggregate(tmp["inside"], by = tmp["grid"], function(x) any(x))

         # Assign grid identifier to each grid using mid-point:
         grid.names <- deg2grid(apply(xx, 1, mean), apply(yy, 1, mean), xref = bb[1], yref = bb[2], dx = d, dy = d)

         # Isolate grids which contain interior points:
         index <- which(tmp$inside[match(grid.names, as.character(tmp$grid))])
         xx <- xx[index, ]
         yy <- yy[index, ]
         grid.names <- grid.names[index]

         # Close grid polygons:
         xx <- cbind(xx, xx[,1])
         yy <- cbind(yy, yy[,1])

         # Contruct list of polygons defining grid cell coordinates and label:
         temp <- vector("list", nrow(xx))
         for (i in 1:nrow(xx)){
            tmp <- km2deg(xx[i, ], yy[i, ])
            temp[[i]]$x <- as.numeric(xx[i,])
            temp[[i]]$y <- as.numeric(yy[i,])
            temp[[i]]$longitude <- tmp$longitude
            temp[[i]]$latitude <- tmp$latitude
            temp[[i]]$label <- grid.names[i]
         }
         grids <- temp
      }else{
         # Convert to 'polygon' object:
         for (i in 1:length(grids)){
            temp <- gulf.spatial::deg2km(grids[[i]]$longitude, grids[[i]]$latitude)
            grids[[i]]$x <- temp$x
            grids[[i]]$y <- temp$y
         }
      }

      # Return results as a list:
      res <- list(coordinates = res, grids = grids)
   }

   return(res)
}

#' @describeIn survey.design Northumberland Strait survey station design.
#' @export nss
nss <- function(n, alternates = 0, stations, type){
   # Check 'n' argument:
   if (missing(n)) stop("Number of desired stations 'n' must be specified.")
   if (n < 1) stop("Number of stations 'n' must be positive.")
   ntemp <- n

   # Load survey grid definitions:
   grids <- read.gulf.spatial(layer = "grids", survey = "nss")

   # Define survey station table:
   if (missing(stations)){
      x <- read.gulf.spatial(layer = "stations", survey = "nss")
   }else{
      x <- stations[c("station.number", "block.number", "latitude", "longitude")]
   }

   # Strip stations beyond a set latitude or longitude limits:
   index <- (x$latitude < (47+1/60)) & (x$longitude < (-62 - 0.0477208/2 + 1/7))
   x <- x[index, ]

   # Determine to which grid each station belongs:
   index <- unlist(gulf.graphics::which.polygon(grids, x$longitude, x$latitude))
   x <- x[!is.na(index), ]
   index <- index[!is.na(index)]
   x$grid <-  unlist(lapply(grids[index], function(y) y$name))

   # Extract area table:
   areas <- data.frame(grid = unlist(lapply(grids, function(y) y$name)),
                       area = unlist(lapply(grids, function(y) y$area)),
                       stringsAsFactors = FALSE)

   # Remove grids containing no stations:
   index <- (areas$grid %in% unique(x$grid))
   areas <- areas[index, ]
   grids <- grids[index]

   # Determine the number of sample stations per grid:
   areas$n <- 0 # Create column to hold sample counts per grid.
   while (n > 0){
      # Randomly permute grids:
      areas <- areas[sample(1:dim(areas)[1]),]

      # Calculate sample assignment probabilities
      p <- areas[,2] / max(areas[,2])
      index <- which(stats::runif(length(p)) < p)
      index <- index[1:min(n, length(index))]

      # Number of stations left to assign:
      n <- n - min(n, length(index))
      areas$n[index] <- areas$n[index] + 1
   }

   # Select random stations within grids:
   areas <- areas[areas$n > 0, ]
   x$sampled <- FALSE
   names(x) <- c("number", "block", "latitude", "longitude", "grid", "sampled") # Rename columns of 'x'.
   k <- 0
   for (i in 1:dim(areas)[1]){
      index <- which(x$grid == areas$grid[i])
      if (areas[i,"n"] > 0){
         if (areas[i,"n"] <= length(index)){
            if (length(index) == 1){
               if (areas[i,"n"] == 1) temp <- index else print("There is a problem in selecting survey stations.")
            }else{
               temp <- sample(index, areas[i,"n"])
            }
            k <- k + length(temp)
            x[temp, "sampled"] <- TRUE
         }else{
            temp <- index
            x[temp, "sampled"] <- TRUE
            print(paste("Too many points to be sampled within grid ", areas[i,"grid"], ", ", length(index), " vs ", areas[i,"n"], sep = ""))
         }
      }
   }

   # Define 'type' field:
   if (missing(type)) x$type <- "Primary" else x$type <- paste(type, alternates + 1)

   # Rename and restructure 'x':
   names(x) <- c("station.number", "block.number", "latitude", "longitude", "grid", "sampled", "type")
   y <- x[x$sampled, c("longitude", "latitude", "station.number", "block.number", "grid", "type")]

   # Generate alternate stations:
   if (alternates > 0)
      y <- rbind(y, sort(nss(n = ntemp, alternates = alternates - 1, stations = x[!x$sampled, ], type = "Alternate"), by = "type"))

   return(y)
}
