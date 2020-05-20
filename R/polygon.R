#' Polygon Functions
#'
#' @description Set of functions to create, manipulate and query polygon objects.
#'
#' @param p List vector of polygon object definitions. Each element in the list
#' corresponds to a single polygon object. They contain two fields, \code{x}
#' and \code{y}, defining the vertices of each polygon.
#'
#' @param x May be either a \sQuote{polygon} object, a list containing polygon
#' vertex definitions (with \sQuote{x} and \sQuote{y} coordinates or longitude
#' and latitude) or a numeric vector of horizontal polygon vertex coordinates.
#'
#' @param y A numeric vector of vertical polygon vertex coordinates.
#'
#' @param hole A logical vector specifying whether component polygons are to be
#' treated as holes. The length of the vector may be either the same as numeric
#' \sQuote{x} and \sQuote{y} or its length may be that of the number of
#' component polygons of the object. Polygons labelled as holes have negative
#' areas.
#'
#' @seealso \code{\link[sp]{point.in.polygon}}
#'
#' @return logical value \code{TRUE} is returned when a point (\code{x}, \code{y})
#' lies within and \code{FALSE} if it lies outside each polygon.
#'
#' @examples
#' # Define set of polygon vertices:
#' x <- c(0, 0, 0.5, 1, 1, 0)
#' y <- c(0, 1, 0.5, 1, 0, 0)
#' p <- as.polygon(x, y)
#' plot(p) # Plot polygon.
#' area(p) # Calculate area of polygon.
#'
#' # Generate set of random points:
#' x <- runif(1000)*2-0.5
#' y <- runif(1000)*2-0.5
#'
#' # Plot points:
#' plot(c(-0.5, 1.5), c(-0.5, 1.5), type = "n")
#' index <- in.polygon(p, x, y)
#' points(x[index], y[index], pch = 21, bg = "red")
#' points(x[!index], y[!index], pch = 21, bg = "blue")
#'
#' @export polygon
#' @export polygon.default
#' @export as.polygon
#' @export plot.polygon
#' @export in.polygon
#' @export which.polygon
#' @export area.polygon
#' @export as.data.frame.polygon
#' @export bbox.polygon
#' @export draw.polygon
#' @export is.polygon
#'
#' @section Functions:
#' \describe{
#'   \item{\code{polygon}, \code{as.polygon}}{Creates or converts to a \sQuote{polygon} object.}
#'   \item{\code{plot.polygon}}{Graphically display a polygon object.}
#'   \item{\code{in.polygon}}{Determine whether a coordinate point lies within a polygon. A logical (\code{n} by \code{k}) matrix is returned where \code{n} is the number of
#'                            component polygons in \code{p} and \code{k} is the length of the \code{x}
#'                            and \code{y} vectors.}
#'   \item{\code{which.polygon}}{Returns the indices specifying to which polygon(s) in a list a point belongs.}
#'   \item{\code{area.polygon}}{Calculates the area of a polygon.}
#'   \item{\code{as.data.frame.polygon}}{Convert polygon object to data frame.}
#'   \item{\code{bbox.polygon}}{Determine bounding box for a polygon object.}
#'   \item{\code{draw.polygon}}{Draw a polygon on a plot.}
#'   \item{\code{is.polygon}}{Checks whether object is a \sQuote{polygon} object.}
#' }
#'
polygon <- function(x, ...) UseMethod("polygon")

#' @rdname polygon
#' @method polygon default
polygon.default <- function(x, ...) if (!("polygon" %in% class(x))) graphics::polygon(x, ...) else return(as.polygon(x))

#' @rdname polygon
as.polygon <- function(x, y = NULL, hole = NULL, ...){
   # AS.POLYGON - Create or convert to a 'polygon' object.

   # If 'x' is a 'polygon' object, return it:
   if ("polygon" %in% class(x)) return(x)

   # Check if 'x' is a list object:
   if (is.list(x)){
      for (i in 1:length(x)){
         if (!("x" %in% names(x[[i]])) & ("longitude" %in% tolower(names(x[[i]])))) x[[i]]$x <- x[[i]]$longitude
         if (!("y" %in% names(x[[i]])) & ("latitude" %in% tolower(names(x[[i]]))))  x[[i]]$y <- x[[i]]$latitude
         if (!all(c("x", "y") %in% names(x[[i]]))) stop("If 'x' is a list then it must contain vertex coordinates.")
         x[[i]] <- as.polygon(x[[i]]$x, x[[i]]$y, x[[i]]$hole)[[1]]
      }

      # Add 'polygon' tag and return result:
      class(x) <- unique(c("polygon", class(x)))

      return(x)
   }

   # Check consistency of polygon definition:
   if (is.null(x) | is.null(x)) stop("Polygon vertices must be specified.")
   if (length(x) != length(y)) stop("'x' and 'y' must have the same number of elements.")
   if (!is.null(dim(x)) | !is.null(dim(y))) stop("'x' and 'y' must be vectors.")
   a <- which(is.na(x))
   b <- which(is.na(x))
   if (length(a) != length(b)) stop("Pattern of NA values is different in 'x' and 'y'.")
   if (length(a) > 0){
      if (any(a != b)) stop("Pattern of NA values is different in 'x' and 'y'.")
   }

   # Check consistency of 'hole' argument:
   if (!is.null(hole)){
      # Check if 'hole' is a logical vector:
      if (!is.logical(hole) | !is.null(dim(hole))) stop("'hole' must be a logical vector.")

      # Determine number of polygons defined by 'x' and 'y':
      index <- c(0, which(is.na(x)), length(x)+1)
      n <- sum(diff(index) > 1)

      # If 'hole' has length 'n', then expand it to the length of 'x':
      if (length(hole) == n){
         # Strip leading and trailing NA values:
         ind <- which(!is.na(x))
         x <- x[ind[1]:ind[length(ind)]]
         y <- y[ind[1]:ind[length(ind)]]
         index <- c(0, which(is.na(x)), length(x)+1)
         temp <- as.logical(NA * x)

         # Define expanded 'hole' vector:
         if (length(index) >= 2){
            for (i in 1:(length(index)-1)){
               if (diff(c(index[i]+1, index[i+1]-1)) >= 1){
                  temp[(index[i]+1):(index[i+1]-1)] <- hole[i]
               }
            }
            hole <- temp
         }
      }

      # Check length of 'hole' vector:
      if (length(hole) != length(x))
         stop("'hole' and 'x' and 'y' must have a consistent number of elements.")
   }

   # Add leading and trailing NA breaks, if necessary:
   if (!is.na(x[1])){
      x <- c(NA, x)
      y <- c(NA, y)
      if (!is.null(hole)) hole <- c(NA, hole)
   }
   if (!is.na(x[length(x)])){
      x <- c(x, NA)
      y <- c(y, NA)
      if (!is.null(hole)) hole <- c(hole, NA)
   }

   # Create polygon list:
   index <- which(is.na(x))
   p <- vector(mode = "list", length(index)-1)
   for (i in 1:(length(index)-1)){
      xx <- x[(index[i]+1):(index[i+1]-1)]
      yy <- y[(index[i]+1):(index[i+1]-1)]
      if ((xx[1] != xx[length(xx)]) | (yy[1] != yy[length(yy)])){
         xx <- c(xx, xx[1])
         yy <- c(yy, yy[1])
      }
      # Remove repeating coordinates:
      index <- intersect(which(diff(xx) == 0), which(diff(yy) == 0))
      if (length(index) > 0){
         p[[i]]$x <- xx[-index]
         p[[i]]$y <- yy[-index]
      }else{
         p[[i]]$x <- xx
         p[[i]]$y <- yy
      }

      # Specify whether polygon contour iss a hole:
      index <- which(is.na(x))
      if (is.null(hole)){
         p[[i]]$hole <- FALSE
      }else{
         p[[i]]$hole <- unique(hole[(index[i]+1):(index[i+1]-1)])
      }
   }

   # Add 'polygon' class attribute:
   class(p) <- c("polygon", "list")

   return(p)
}

#' @rdname polygon
#' @method plot polygon
plot.polygon <- function(p, labels = FALSE, as.lines = FALSE, ...){
   # PLOT.POLYGON - Plot a list of polygon objects.

   # Convert 'p' to 'polygon' class:
   if (all(c("x", "y") %in% names(p))){
      p <- as.polygon(x = p$x, y = p$y, hole = p$hole)
   }

   # Create new plot:
   if (is.null(dev.list())){
      bb <- bbox(p)
      plot(bb[c(1, 3)], bb[c(2, 4)], type = "n", xlab = "", ylab = "")
   }

   # Plot a single 'polygon' object:
   if (is(p, "polygon")){
      for (i in 1:length(p)){
         if (all(c("x", "y") %in% names(p[[i]]))){
            if (as.lines) lines(p[[i]]$x, p[[i]]$y, ...) else polygon(p[[i]]$x, p[[i]]$y, ...)
         }
      }
   }else{
      for (i in 1:length(p)){
         plot.polygon(p[[i]], labels = labels, as.lines = as.lines, ...)
      }
   }
}

#' @rdname polygon
in.polygon <- function(p, x, y, ...){
   # Check input arguments:
   if (missing(x) | missing(y)) stop("'x' and 'y' must be specified.")
   if (length(x) != length(y))  stop("'x' and 'y' must be the same length.")

   # Check that 'p' is a list object:
   if (!is.list(p)) stop("'p' must be a either a 'polygon' or a 'list' object.")

   # Convert 'p' to 'polygon' class:
   if (all(c("x", "y") %in% names(p))) p <- as.polygon(x = p$x, y = p$y, hole = p$hole)

   if (is(p, "polygon")){
      index <- rep(FALSE, length(x))
      # Loop over normal polygons:
      for (i in 1:length(p)){
         if (all(c("x", "y", "hole") %in% names(p[[i]]))){
            if (!p[[i]]$hole){
               index <- index | (sp::point.in.polygon(x, y, p[[i]]$x, p[[i]]$y, mode.checked = FALSE) > 0)
            }
         }
      }
      # Loop over 'holes':
      for (i in 1:length(p)){
         if (all(c("x", "y", "hole") %in% names(p[[i]]))){
            if (p[[i]]$hole){
               index <- index & !(sp::point.in.polygon(x, y, p[[i]]$x, p[[i]]$y, mode.checked = FALSE) > 0)
            }
         }
      }
   }else{
      # Collate logical matrix:
      index <- NULL
      for (i in 1:length(p)){
         index <- cbind(index, in.polygon(p[[i]], x, y))
      }
   }

   return(index)
}

#' @rdname polygon
which.polygon <- function(p, x, y, as.list = FALSE){
   # WHICH.POLYGON - Determine to which polygon a point belongs.

   # Extract 'in.polygon' logical matrix:
   result <- in.polygon(p, x, y)

   # Extract polygon membership list:
   temp <- apply(result, 1, which)

   if (length(temp) == 0) temp <- list(NA)

   if (!is.list(temp)) temp <- as.list(temp)

   # Place NA values for unmatched points:
   index <- (unlist(lapply(temp, length)) == 0)
   temp[index] <- NA

   if (!as.list){
      index <- !(unlist(lapply(temp, length)) == 0)
      result <- rep(NA, length(temp))
      result[index] <- unlist(lapply(temp[index], min))
   }else{
      result <- temp
   }

   return(result)
}

#' @rdname polygon
area.polygon <- function(p){
   # AREA.POLYGON - Calculate the area of a polygon.

   # Initialize area result variable:
   A <- rep(NA, length(p))

   # Loop over component polygons:
   for (i in 1:length(p)){
      xx <- p[[i]]$x
      yy <- p[[i]]$y

      index <- unique(c(0, which(is.na(xx)), length(xx) + 1))
      temp <- NULL
      for (j in 1:(length(index)-1)){
         xxx <- xx[(index[j]+1):(index[j+1]-1)]
         yyy <- yy[(index[j]+1):(index[j+1]-1)]

         n <- length(xxx)

         temp[j] <- 0.5 * abs(sum(xxx[1:(n-1)]*yyy[2:n] - xxx[2:n]*yyy[1:(n-1)]))
      }

       A[i] <- sum(temp)
       if (!is.null(p[[i]]$hole)) A[i] <- (-2*p[[i]]$hole + 1) * A[i]
   }

   return(A)
}

#' @rdname polygon
#' @method as.data.frame polygon
as.data.frame.polygon <- function(x){
   # AS.DATA.FRAME.POLYGON - Convert a 'polygon' object to a data frame.

   if (("x" %in% names(x)) & ("y" %in% names(x))){
      v <- data.frame(x = x$x, y = x$y, label = x$label)
      str <- setdiff(names(x), c("x", "y"))
      if (length(str) > 0){
         for (i in 1:length(str)){
            if (length(x[[str[i]]]) == 1){
               temp <- data.frame(rep(x[[str[i]]], nrow(v)))
            }else{
               temp <- data.frame(x[[str[i]]])
            }
            names(temp) <- str[i]
            v <- cbind(v, temp)
         }
      }
   }

   return(v)
}

#' @rdname polygon
bbox.polygon <- function(p, ...){
   bb <- rep(NA, 4)
   for (i in 1:length(p)){
      bb[1] <- min(bb[1], min(p[[i]]$x), na.rm = TRUE)
      bb[2] <- min(bb[2], min(p[[i]]$y), na.rm = TRUE)
      bb[3] <- max(bb[3], max(p[[i]]$x), na.rm = TRUE)
      bb[4] <- max(bb[4], max(p[[i]]$y), na.rm = TRUE)
   }

   return(bb)
}

#' @rdname polygon
draw.polygon <- function(x = NULL, y = NULL, n, tol = 5E-3, snap = TRUE,
                         snap.to.grid = FALSE, grid.increment, grid.offset = 0,
                         pch = 21, bg = "black", ...){
   # DRAW.POINTS - Draw a points interactively on a plot.

   # Points supplied by user:
   flag <- FALSE
   if (!missing(x) & !missing(y)){
      if (length(x) != length(y)) stop("Specified 'x' and 'y' coordinate vectors must have the same length.")
      flag <- TRUE
   }

   # Set tolerance flag default:
   tolerance.flag <- TRUE

   # Create default plot:
   if (is.null(grDevices::dev.list()) & !flag) graphics::plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
   if (is.null(grDevices::dev.list()) & flag)  graphics::plot(x, y, xlab = "", ylab = "")

   # Define 'grid.increment':
   if (missing(grid.increment)){
      range <- c(0, mean(c(diff(range(graphics::par("usr")[1:2])), diff(range(graphics::par("usr")[3:4])))))
      grid.increment <- unique(round(diff(pretty(range, n = 100)), 9))
   }

   if (missing(n)) n <- NULL

   # Initialize point vectors to be returned:
   xx <- NULL
   yy <- NULL

   # Packaging function for the polygon:
   package <- function(x, y){
      if ((x[length(x)] != x[1]) | (y[length(x)] != y[1])){
         x <- c(x, x[1])
         y <- c(y, y[1])
      }
      p <- as.polygon(x, y)
      plot.polygon(p)
      return(p)
   }

   # Keyboard button function:
   keyboard.fun <- function(key){
      if (toupper(key) == "M"){
         cat("\n")
         cat("'g'   : Toggle snap-to-grid option.\n")
         cat("'m'   : Display menu options.\n")
         cat("'l'   : Snap to nearest point along a line.\n")
         cat("'s'   : Toggle snap-to-nearest-point option.\n")
         cat("'t'   : Toggle tolerance option.\n")
         cat("'Esc' : Return coordinates.\n")
         cat("\n")
      }
      if (toupper(key) == "G"){
         snap.to.grid <<- !snap.to.grid
         if (snap.to.grid) cat("   Snap-to-grid is ON.\n") else cat("   Snap-to-grid is OFF.\n")
      }
      if (toupper(key) == "L"){
         if (length(x) >= 2){
            snap.to.line <<- !snap.to.line
            snap <<- FALSE
            snap.to.grid <<- FALSE
            graphics::lines(x, y, ...)
            if (snap.to.line) cat("   Snap-to-line is ON.\n") else cat("   Snap-to-line is OFF.\n")
         }else{
            cat("   Snap-to-line requires that 'x' and 'y' coordinates be specified.\n")
         }
      }
      if (toupper(key) == "S"){
         snap <<- !snap
         if (snap) cat("   Snap is ON.\n") else cat("   Snap is OFF.\n")
      }
      if (toupper(key) == "T"){
         tolerance.flag <<- !tolerance.flag
         if (tolerance.flag) cat("   Tolerance is ON.\n") else cat("   Tolerance is OFF.\n")
      }
      if (key == "ctrl-[") return(data.frame(x = xx, y = yy))

      return(NULL)
   }

   # Mouse plotting function:
   plot.point <- function(buttons, x0, y0){
      if (any(1:2 %in% buttons)) return(4)

      # Convert to user coordinates:
      x0 <- graphics::grconvertX(x0, "ndc", "user")
      y0 <- graphics::grconvertY(y0, "ndc", "user")

      # Add coordinates:
      xx <<- c(xx, x0)
      yy <<- c(yy, y0)

      # Snap last coordinate to neighbouring point:
      if (snap & (length(xx) >= 2)){
         d <- sqrt((xx[length(xx)] - c(x, xx[1:(length(xx)-1)]))^2 +
                   (yy[length(yy)] - c(y, yy[1:(length(yy)-1)]))^2)
         diag <- sqrt((graphics::par("usr")[2] - graphics::par("usr")[1])^2 + (graphics::par("usr")[4] - graphics::par("usr")[3])^2)
         index <- which(d == min(d))[1]
         if ((d[index] < (diag * tol)) | !tolerance.flag){
            xx[length(xx)] <<- c(x, xx[1:(length(xx)-1)])[index]
            yy[length(yy)] <<- c(y, yy[1:(length(yy)-1)])[index]
            cat("Point was snapped to nearest adjacent point.\n")
         }
      }

      # Snap point to grid:
      if (snap.to.grid){
         xx[length(xx)] <<- round((xx[length(xx)] + grid.offset) / grid.increment) * grid.increment - grid.offset
         yy[length(yy)] <<- round((yy[length(xx)] + grid.offset) / grid.increment) * grid.increment - grid.offset
      }

      # Remove last point if it was identical to the previous point:
      if (length(xx) >= 2){
         if ((xx[length(xx)] == xx[length(xx)-1]) & (yy[length(xx)] == yy[length(xx)-1])){
            xx <<- xx[-length(xx)]
            yy <<- yy[-length(yy)]
         }
      }

      graphics::points(xx[length(xx)], yy[length(yy)], pch = pch, bg = bg, ...)
      if (length(xx) >= 2) graphics::lines(c(xx[length(xx)-1],  xx[length(xx)]), c(yy[length(yy)-1],  yy[length(yy)]), ...)

      # Stopping case if polygon is closed:
      if ((length(xx) > 1) & (xx[length(xx)] == xx[1]) & (yy[length(xx)] == yy[1])) return(package(xx, yy))

      if (!is.null(n)){
         if (length(xx) >= n){
            return(package(xx, yy))
         }
      }

      return(NULL)
   }

   cat("Press 'm' for menu options.\n")

   grDevices::getGraphicsEvent(prompt = "Click mouse in plot to draw points.",
                    onMouseDown = plot.point,
                    onKeybd = keyboard.fun)

   return(package(xx, yy))
}

#' @rdname polygon
is.polygon <- function(p) return("polygon" %in% class(p))

