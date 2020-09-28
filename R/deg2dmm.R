#' Latitude-Longitude Conversion
#'
#' @description Converts from one latitude-longitude numeric format to another.
#'
#' @param x Numeric vector.
#'
#' @examples
#' # Degree decimal-minute conversions:
#' deg2dmm(66.5) # Returns 6630.00
#' dmm2deg(6630) # Returns 66.5
#' dmm2deg(663000) # Returns 66.5
#' deg2dmm(60+5*runif(100)) # Convert 100 random coordinates to DMM format.
#'
#' # Degree-minute-second conversions:
#' deg2dms(-66.5) # Returns -663000
#' dms2deg(-663000) # Returns -66.5
#' deg2dms(60+5*runif(100)) # Convert 100 random coordinates to DMS format.
#'
#' @seealso \code{\link{loran2deg}}, \code{\link{deg2grid}}, \code{\link{deg2str}}

#' @describeIn deg2dmm Convert from decimal degree to degree-decimal minute format.
#' @export deg2dmm
deg2dmm <- function(x){
   deg <- floor(abs(x))
   min <- (abs(x) %% 1) * 60
   r <- sign(x)*(100 * deg + min)
   return(r)
}

#' @describeIn deg2dmm Convert from degree-decimal minute to decimal degree format.
#' @export dmm2deg
dmm2deg <- function(x){
   x <- x / (10^(floor(log10(abs(x))) - 3)) # Convert to ddmm.mm.
   deg <- floor(abs(x) / 100)
   min <- (abs(x) / 100  - deg) * 100
   r <- sign(x) * (deg + min / 60)
   return(r)
}

#' @describeIn deg2dmm Convert from decimal degree to degree-minute-second format.
#' @export deg2dms
deg2dms <- function(x){
   deg <- floor(abs(x))
   min <- 60 * (abs(x) - deg)
   sec <- 60 * (min - floor(min))
   r <- sign(x) * (10000 * deg + 100 * floor(min) + sec)
   return(r)
}

#' @describeIn deg2dmm Convert from degree-minute-second to decimal degree format.
#' @export dms2deg
dms2deg <- function(x){
   x <- x / (10^(floor(log10(abs(x))) - 5)) # Convert to ddmmss.ss
   deg <- floor(abs(x) / 10000)
   min <- round(abs(x) - (deg*10000), -2) / 100
   sec <- 100 * ((abs(x) / 100) %% 1)
   r <- sign(x) * (deg + min / 60 + sec / 3600)
   return(r)
}
