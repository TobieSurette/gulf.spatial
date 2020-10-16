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
   if ("longitude" %in% names(x)) v <- dmm2deg(x$longitude) else v <- rep(NA, nrow(x))

   names(x) <- gsub("longitude.end", "longitude.stop", names(x))

   index <- which(is.na(v) & !is.na(x$longitude.start) & is.na(x$longitude.stop))
   if (length(index) > 0) v[index] <- dmm2deg(x$longitude.start[index])

   index <- which(is.na(v) & is.na(x$longitude.start) & !is.na(x$longitude.stop))
   if (length(index) > 0) v[index] <- dmm2deg(x$longitude.stop[index])

   index <- which(is.na(v) & !is.na(x$longitude.start) & !is.na(x$longitude.stop))
   if (length(index) > 0) v[index] <- (dmm2deg(x$longitude.start[index]) + dmm2deg(x$longitude.stop[index])) / 2

   # Use logbook values:
   index <- which(is.na(v) & !is.na(x$longitude.start.logbook) & is.na(x$longitude.stop.logbook))
   if (length(index) > 0) v[index] <- dmm2deg(x$longitude.start.logbook[index])

   index <- which(is.na(v) & is.na(x$longitude.start.logbook) & !is.na(x$longitude.stop.logbook))
   if (length(index) > 0) v[index] <- dmm2deg(x$longitude.stop.logbook[index])

   index <- which(is.na(v) & !is.na(x$longitude.start.logbook) & !is.na(x$longitude.stop.logbook))
   if (length(index) > 0) v[index] <- (dmm2deg(x$longitude.start.logbook[index]) + dmm2deg(x$longitude.stop.logbook[index])) / 2

   return(v)
}

#' @describeIn scsset Convert numeric latitude coordinates from \code{scsset} objects to decimal degrees.
#' @export
latitude.scsset <- function(x){
   if ("latitude" %in% names(x)) v <- dmm2deg(x$latitude) else v <- rep(NA, nrow(x))

   names(x) <- gsub("latitude.end", "latgitude.stop", names(x))

   index <- which(is.na(v) & !is.na(x$latitude.start) & is.na(x$latitude.stop))
   if (length(index) > 0) v[index] <- dmm2deg(x$latitude.start[index])

   index <- which(is.na(v) & is.na(x$latitude.start) & !is.na(x$latitude.stop))
   if (length(index) > 0) v[index] <- dmm2deg(x$latitude.stop[index])

   index <- which(is.na(v) & !is.na(x$latitude.start) & !is.na(x$latitude.stop))
   if (length(index) > 0) v[index] <- (dmm2deg(x$latitude.start[index]) + dmm2deg(x$latitude.stop[index])) / 2

   # Use logbook values:
   index <- which(is.na(v) & !is.na(x$latitude.start.logbook) & is.na(x$latitude.stop.logbook))
   if (length(index) > 0) v[index] <- dmm2deg(x$latitude.start.logbook[index])

   index <- which(is.na(v) & is.na(x$latitude.start.logbook) & !is.na(x$latitude.stop.logbook))
   if (length(index) > 0) v[index] <- dmm2deg(x$latitude.stop.logbook[index])

   index <- which(is.na(v) & !is.na(x$latitude.start.logbook) & !is.na(x$latitude.stop.logbook))
   if (length(index) > 0) v[index] <- (dmm2deg(x$latitude.start.logbook[index]) + dmm2deg(x$latitude.stop.logbook[index])) / 2

   return(v)
}

