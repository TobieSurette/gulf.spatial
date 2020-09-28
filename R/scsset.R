#' \strong{\code{scsset}} Class Spatial Functions
#'
#' @description Spatial functions for the snow crab set \code{scsset} data class.
#'
#' @param x \code{scsset} object.
#'
#' @examples
#' x <- read.scsset(2020)
#' longitude(x)
#'
#' plot(lon(x), lat(x))
#'

#' @describeIn scsset Convert numeric longitude coordinates from \code{scsset} objects to decimal degrees.
#' @export
longitude.scsset <- function(x){
   if ("longitude" %in% names(x)) v <- x$longitude else v <- rep(NA, nrow(x))
   index <- is.na(v)

   index <- which(is.na(v) & !is.na(x$longitude.start.logbook) & is.na(x$longitude.end.logbook))
   if (length(index) > 0) v[index] <- dmm2deg(x$longitude.start.logbook[index])

   index <- which(is.na(v) & is.na(x$longitude.start.logbook) & !is.na(x$longitude.end.logbook))
   if (length(index) > 0) v[index] <- dmm2deg(x$longitude.end.logbook[index])

   index <- which(is.na(v) & !is.na(x$longitude.start.logbook) & !is.na(x$longitude.end.logbook))
   if (length(index) > 0) v[index] <- (dmm2deg(x$longitude.start.logbook[index]) + dmm2deg(x$longitude.end.logbook[index])) / 2

   return(v)
}

#' @describeIn scsset Convert numeric latitude coordinates from \code{scsset} objects to decimal degrees.
#' @export
latitude.scsset <- function(x){
   if ("latitude" %in% names(x)) v <- x$latitude else v <- rep(NA, nrow(x))
   index <- is.na(v)

   index <- which(is.na(v) & !is.na(x$latitude.start.logbook) & is.na(x$latitude.end.logbook))
   if (length(index) > 0) v[index] <- dmm2deg(x$latitude.start.logbook[index])

   index <- which(is.na(v) & is.na(x$latitude.start.logbook) & !is.na(x$latitude.end.logbook))
   if (length(index) > 0) v[index] <- dmm2deg(x$latitude.end.logbook[index])

   index <- which(is.na(v) & !is.na(x$latitude.start.logbook) & !is.na(x$latitude.end.logbook))
   if (length(index) > 0) v[index] <- (dmm2deg(x$latitude.start.logbook[index]) + dmm2deg(x$latitude.end.logbook[index])) / 2

   return(v)
}

