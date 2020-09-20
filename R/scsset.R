#' \strong{\code{scsset}} Class Spatial Functions
#'
#' @description Spatial functions for the snow crab set \code{scsset} data class.
#'
#' @param x \code{scsset} object.
#'
#' @examples
#' x <- read.scsset(2020)
#' longitude(x)

coordinates.scsset <- function(x){
   t <- table(floor(log10(abs(x))))
   p <- as.numeric(names(which.max(t)))
   if (p == 3) return(dmm2deg(x))
   if (p == 5) return(dmm2deg(x)/100)
   return(x)
}

#' @describeIn scsset Convert numeric longitude coordinates from \code{scsset} objects to decimal degrees.
#' @export
longitude.scsset <- function(x){
   if ("longitude" %in% names(x)) v <- x$longitude else v <- rep(NA, nrow(x))
   index <- is.na(v)
   v[index] <- (x$longitude.start.logbook[index] + x$longitude.end.logbook[index]) / 2
   index <- is.na(v) & !is.na(x$longitude.start.logbook) & is.na(x$longitude.end.logbook)
   v[index] <- x$longitude.start.logbook[index]
   index <- is.na(v) & is.na(x$longitude.start.logbook) & !is.na(x$longitude.end.logbook)
   v[index] <- x$longitude.end.logbook[index]

   v <- coordinates.scsset(v)

   return(v)
}

#' @describeIn scsset Convert numeric latitude coordinates from \code{scsset} objects to decimal degrees.
#' @export
latitude.scsset <- function(x){
   if ("latitude" %in% names(x)) v <- x$latitude else v <- rep(NA, nrow(x))
   index <- is.na(v)
   v[index] <- (x$latitude.start.logbook[index] + x$latitude.end.logbook[index]) / 2
   index <- is.na(v) & !is.na(x$latitude.start.logbook) & is.na(x$latitude.end.logbook)
   v[index] <- x$latitude.start.logbook[index]
   index <- is.na(v) & is.na(x$latitude.start.logbook) & !is.na(x$latitude.end.logbook)
   v[index] <- x$latitude.end.logbook[index]

   v <- coordinates.scsset(v)

   return(v)
}
