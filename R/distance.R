#' Calculate Distance
#'
#' @description Functions to retrieve or calculate distance.
#'
#' @param x Horizontal coordinates.
#' @param y Vertical coordinates.
#' @param x0 Second set of horizontal coordinates.
#' @param y0 Second set of vertical coordinates.
#' @param coordinate.units Units of the input coordinates.
#' @param distance.units Units distances to be returned.
#' @param pairwise Logical value specifying whether to calculate all possible pairwise combinations of coordinates.
#' @param method Method used to calculate distance.
#'
#' @export distance
#' @exprot distance.default
#' @seealso deg2km
distance <- function(x, ...) UseMethod("distance")

#' @describeIn distance Default distance method.
distance.default <- function(x, y, x0 = NULL, y0 = NULL,
                             coordinate.units = "latlon", distance.units = "km",
                             pairwise = TRUE, method = "utm", ...){
   # DISTANCE - Calculate distance between points.

   # Convert arguments to lower case:
   coordinate.units <- tolower(coordinate.units)
   distance.units   <- tolower(distance.units)

   # Standardize coordinate units input values:
   temp <- NULL
   if (coordinate.units %in% c("latlong", "latlon", "lat-lon", "lat-long")) temp <- "latlon"
   if (coordinate.units %in% c("m", "meter", "meters")) temp <- "m"
   if (coordinate.units %in% c("km", "kilometer", "kilometers")) temp <- "km"
   if (is.null(temp)) stop("Specifed coordinate units are invalid.")

   # Standardize distance units input values:
   temp <- NULL
   if (distance.units %in% c("m", "meter", "meters")) temp <- "m"
   if (distance.units %in% c("km", "kilometer", "kilometers")) temp <- "km"
   if (is.null(temp)) stop("Specifed distance units are invalid.")

   # Check coordinate arguments for consistency:
   if (length(x) != length(y)) stop("'x' and 'y' have inconsistent lengths.")
   if (xor(is.null(x0), is.null(y0))) stop("'x0' and 'y0' must both be specified.")
   if ((length(x0) != length(y0)) & (!is.null(x0))) stop("'x0' and 'y0' have inconsistent lengths.")

   # Check 'method' argument:
   method <- tolower(method)
   if (!(method %in% c("utm", "ellipsoid"))) stop("Invalid 'method' argument.")

   # Convert lat-lon coordinates to kilometers:
   if (coordinate.units == "latlon"){
      temp <- deg2km(x, y, method = method, ...)
      x <- temp$x
      y <- temp$y

      if (!is.null(x0)){
         temp <- deg2km(x0, y0, method = method, ...)
         x0 <- temp$x
         y0 <- temp$y
      }

      # Change the format of the coordinates:
      coordinate.units <- "km"
   }

   # Change input coordinates from meters to kilometers:
   if (coordinate.units == "m"){
      x <- x / 1000
      y <- y / 1000
      if (!is.null(x0)){
         x0 <- x0 / 1000
         y0 <- y0 / 1000
      }
   }

   # Reformat input coordinate vectors:
   flag <- FALSE
   if (!is.null(x0) & !is.null(y0)){
      if (!pairwise){
         if (length(x) != length(x0)) stop("'x', 'y', 'x0' and 'y0' have inconsistent lengths.")
      }else{
         d <- c(length(x), length(x0))
         x  <- repvec(x, ncol = d[2])
         y  <- repvec(y, ncol = d[2])
         x0 <- repvec(x0, nrow = d[1])
         y0 <- repvec(y0, nrow = d[1])
      }
   }else{
      if (!pairwise) stop("'pairwise' argument must be set to TRUE for a single set of coordinates.")
      x <- repvec(x, nrow = length(x))
      y <- repvec(y, nrow = length(y))
      x0 <- t(x)
      y0 <- t(y)
   }

   # Calculate Euclidean distance:
   dist <- sqrt((x-x0)^2 + (y-y0)^2)

   # Convert to required distance units:
   if (distance.units == "m") dist <- (dist / 1000)

   return(dist)
}
