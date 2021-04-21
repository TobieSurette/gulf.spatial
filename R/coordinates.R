#' Retrieve Spatial Coordinates
#'
#' @description Functions to retrieve coordinates from an object.
#'
#' @param x Object containing coordinates. Coordinates can be extracted from list and
#'          data frame objects if they either have defined \code{longitude} and \code{latitude}
#'          methods already defined, or looking for \sQuote{longitude} or \sQuote{latitude}
#'          in the field variable names. In the latter case, partial matching is used.
#'
#' @export
#'
#' @section Functions:
#' \describe{
#'   \item{\code{longitude}, \code{latitude}}{Extract longitude or latitude coordinates from a data frame or list.}
#'   \item{\code{lon}, \code{long}}{Alias functions for \code{longitude}.}
#'   \item{\code{lat}}{Alias function for \code{latitude}.}
#' }
#'

#' @examples
#' x <- read.scsset(2020, valid = 1)
#' longitude(x)
#' plot(lon(x), lat(x))
#'

#' @rdname coordinates
#' @export
longitude <- function(x, ...) UseMethod("longitude")

#' @rdname coordinates
#' @export
latitude <- function(x, ...) UseMethod("latitude")

#' @rdname coordinates
#' @export
longitude.default <- function(x, ...) return(NULL)

#' @rdname coordinates
#' @export
latitude.default <- function(x, ...) return(NULL)

#' @rdname coordinates
#' @export
longitude.data.frame <- function(x, ...){
   n <- tolower(names(x))
   if (is.null(n)) return(NULL)
   ix <- grep("longitude", n)[1]
   if (length(ix) == 0) ix <- grep("long", n)[1]
   if (length(ix) == 0) ix <- grep("lon", n)[1]
   if (length(ix) == 0) return(NULL)
   return(x[[ix]])
}

#' @rdname coordinates
#' @export
latitude.data.frame <- function(x, ...){
   n <- tolower(names(x))
   if (is.null(n)) return(NULL)
   ix <- grep("latitude", n)[1]
   if (length(ix) == 0) ix <- grep("lat", n)[1]
   if (length(ix) == 0) return(NULL)
   return(x[[ix]])
}

#' @rdname coordinates
#' @export
lon <- longitude

#' @rdname coordinates
#' @export
lat <- latitude

#' @rdname coordinates
#' @export
long <- longitude

#' @describeIn coordinates Convert numeric longitude coordinates from \code{scsset} objects to decimal degrees.
#' @export
longitude.scsset <- function(x){
   names(x) <- gsub("[.]end", ".stop", tolower(names(x)))

   # Initialize missing variables:
   if (!("longitude.stop" %in% names(x)))          x$longitude.stop <- NA
   if (!("longitude.start" %in% names(x)))         x$longitude.start <- NA
   if (!("longitude.start.logbook" %in% names(x))) x$longitude.start.logbook <- NA
   if (!("longitude.stop.logbook" %in% names(x)))  x$longitude.stop.logbook <- NA

   # Convert LoranC coordinates:
   if (all(c("loran.x", "loran.y") %in% names(x))){
      ix <- which(!is.na(x$loran.x) & !is.na(x$loran.y) & is.na(x$longitude))
      lon <- loran2deg(x$loran.x[ix], x$loran.y[ix])$long
      x$longitude[ix[lon != 0]] <- deg2dmm(lon[lon != 0])
   }
   if (all(c("loran.x.start.logbook", "loran.y.start.logbook") %in% names(x))){
      ix <- which(!is.na(x$loran.x.start.logbook) & !is.na(x$loran.y.start.logbook) & is.na(x$longitude.start.logbook))
      lon <- loran2deg(x$loran.x.start.logbook[ix], x$loran.y.start.logbook[ix])$long
      x$longitude.start.logbook[ix[lon != 0]] <- deg2dmm(lon[lon != 0])
   }
   if (all(c("loran.x.stop.logbook", "loran.y.stop.logbook") %in% names(x))){
      ix <- which(!is.na(x$loran.x.stop.logbook) & !is.na(x$loran.y.stop.logbook) & is.na(x$longitude.stop.logbook))
      lon <- loran2deg(x$loran.x.stop.logbook[ix], x$loran.y.stop.logbook[ix])$long
      x$longitude.stop.logbook[ix[lon != 0]] <- deg2dmm(lon[lon != 0])
   }

   if ("longitude" %in% names(x)) v <- dmm2deg(x$longitude) else v <- rep(NA, nrow(x))

   # Use stored values:
   ix <- which(is.na(v) & !is.na(x$longitude.start) & is.na(x$longitude.stop))
   if (length(ix) > 0) v[ix] <- dmm2deg(x$longitude.start[ix])

   ix <- which(is.na(v) & is.na(x$longitude.start) & !is.na(x$longitude.stop))
   if (length(ix) > 0) v[ix] <- dmm2deg(x$longitude.stop[ix])

   ix <- which(is.na(v) & !is.na(x$longitude.start) & !is.na(x$longitude.stop))
   if (length(ix) > 0) v[ix] <- (dmm2deg(x$longitude.start[ix]) + dmm2deg(x$longitude.stop[ix])) / 2

   # Use logbook values:
   ix <- which(is.na(v) & !is.na(x$longitude.start.logbook) & is.na(x$longitude.stop.logbook))
   if (length(ix) > 0) v[ix] <- dmm2deg(x$longitude.start.logbook[ix])

   ix <- which(is.na(v) & is.na(x$longitude.start.logbook) & !is.na(x$longitude.stop.logbook))
   if (length(ix) > 0) v[ix] <- dmm2deg(x$longitude.stop.logbook[ix])

   ix <- which(is.na(v) & !is.na(x$longitude.start.logbook) & !is.na(x$longitude.stop.logbook))
   if (length(ix) > 0) v[ix] <- (dmm2deg(x$longitude.start.logbook[ix]) + dmm2deg(x$longitude.stop.logbook[ix])) / 2

   v <- -abs(v)

   return(v)
}

#' @describeIn coordinates Convert numeric latitude coordinates from \code{scsset} objects to decimal degrees.
#' @export
latitude.scsset <- function(x){
   names(x) <- gsub("[.]end", "[.]stop", tolower(names(x)))

   # Initialize missing variables:
   if (!("latitude.stop" %in% names(x)))          x$latitude.stop <- NA
   if (!("latitude.start" %in% names(x)))         x$latitude.start <- NA
   if (!("latitude.start.logbook" %in% names(x))) x$latitude.start.logbook <- NA
   if (!("latitude.stop.logbook" %in% names(x)))  x$latitude.stop.logbook <- NA

   # Convert LoranC coordinates:
   if (all(c("loran.x", "loran.y") %in% names(x))){
      ix <- which(!is.na(x$loran.x) & !is.na(x$loran.y) & is.na(x$latitude))
      lat <- loran2deg(x$loran.x[ix], x$loran.y[ix])$lat
      x$latitude[ix[lat != 0]] <- deg2dmm(lat[lat != 0])
   }
   if (all(c("loran.x.start.logbook", "loran.y.start.logbook") %in% names(x))){
      ix <- which(!is.na(x$loran.x.start.logbook) & !is.na(x$loran.y.start.logbook) & is.na(x$latitude.start.logbook))
      lat <- loran2deg(x$loran.x.start.logbook[ix], x$loran.y.start.logbook[ix])$lat
      x$latitude.start.logbook[ix[lat != 0]] <- deg2dmm(lat[lat != 0])
   }
   if (all(c("loran.x.stop.logbook", "loran.y.stop.logbook") %in% names(x))){
      ix <- which(!is.na(x$loran.x.stop.logbook) & !is.na(x$loran.y.stop.logbook) & is.na(x$latitude.stop.logbook))
      lat <- loran2deg(x$loran.x.stop.logbook[ix], x$loran.y.stop.logbook[ix])$lat
      x$latitude.stop.logbook[ix[lat != 0]] <- deg2dmm(lat[lat != 0])
   }

   if ("latitude" %in% names(x)) v <- dmm2deg(x$latitude) else v <- rep(NA, nrow(x))

   ix <- which(is.na(v) & !is.na(x$latitude.start) & is.na(x$latitude.stop))
   if (length(ix) > 0) v[ix] <- dmm2deg(x$latitude.start[ix])

   ix <- which(is.na(v) & is.na(x$latitude.start) & !is.na(x$latitude.stop))
   if (length(ix) > 0) v[ix] <- dmm2deg(x$latitude.stop[ix])

   ix <- which(is.na(v) & !is.na(x$latitude.start) & !is.na(x$latitude.stop))
   if (length(ix) > 0) v[ix] <- (dmm2deg(x$latitude.start[ix]) + dmm2deg(x$latitude.stop[ix])) / 2

   # Use logbook values:
   ix <- which(is.na(v) & !is.na(x$latitude.start.logbook) & is.na(x$latitude.stop.logbook))
   if (length(ix) > 0) v[ix] <- dmm2deg(x$latitude.start.logbook[ix])

   ix <- which(is.na(v) & is.na(x$latitude.start.logbook) & !is.na(x$latitude.stop.logbook))
   if (length(ix) > 0) v[ix] <- dmm2deg(x$latitude.stop.logbook[ix])

   ix <- which(is.na(v) & !is.na(x$latitude.start.logbook) & !is.na(x$latitude.stop.logbook))
   if (length(ix) > 0) v[ix] <- (dmm2deg(x$latitude.start.logbook[ix]) + dmm2deg(x$latitude.stop.logbook[ix])) / 2

   v <- abs(v)

   return(v)
}

