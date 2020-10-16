#' Calculate Surface Area
#'
#' @description Functions to calculate surface area.
#'
#' @examples
#' x <- c(0, 0, 0.5, 1, 1, 0)
#' y <- c(0, 1, 0.5, 1, 0, 0)
#' area(x, y) # Calculate area of polygon.

#' @export
area <- function(x, ...) UseMethod("area")

#' @describeIn area Calculate surface area of a polygon using its vertex coordinates.
#' @export
area.default <- function(x, y, ...){
   index <- unique(c(0, which(is.na(x)), length(x) + 1))
   a <- rep(NA, length(index)-1)
   for (j in 1:(length(index)-1)){
      xx <- x[(index[j]+1):(index[j+1]-1)]
      yy <- y[(index[j]+1):(index[j+1]-1)]

      n <- length(xx)

      a[j] <- 0.5 * abs(sum(xx[1:(n-1)]*yy[2:n] - xx[2:n]*yy[1:(n-1)]))
   }

   return(sum(a))
}

#' @describeIn area Calculate the area of a \code{polygon} object.
#' @export
area.polygon <- function(p){
   # Initialize area result variable:
   A <- rep(NA, length(p))

   # Loop over component polygons:
   for (i in 1:length(p)){
      A[i] <- area(p[[i]]$x, p[[i]]$y)
      if (!is.null(p[[i]]$hole)) A[i] <- (-2*p[[i]]$hole + 1) * A[i]
   }

   return(A)
}
