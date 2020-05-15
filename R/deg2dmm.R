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
#' deg2dmm(60+5*runif(100)) # Convert 100 random coordinates to DMM format.
#'
#' # Degree-minute-second conversions:
#' deg2dms(66.5) # Returns 663000
#' dms2deg(663000) # Returns 66.5
#' deg2dms(60+5*runif(100)) # Convert 100 random coordinates to DMS format.
#'
#' @export deg2dmm
#' @export dmm2deg
#' @export deg2dms
#' @export dms2deg
#'
#' @seealso loran2deg deg2grid deg2grid
#'
#' Convert from decimal degree to degree decimal-minute format.
deg2dmm <- function(x){
   deg <- floor(abs(x))
   min <- (abs(x) %% 1) * 60
   r <- sign(x)*(100 * deg + min)
   return(r)
}

#' @describeIn deg2dmm Convert from degree-decimal minute to decimal degree format.
dmm2deg <- function(x){
   deg <- round(abs(x), -2) / 100
   min <- (abs(x) - round(abs(x), -2))
   r <- sign(x) * (deg + min / 60)
   return(r)
}

#' @describeIn deg2dmm Convert from decimal degree to degree-minute-second format.
deg2dms <- function(x){
   deg <- floor(abs(x))
   min <- 60 * (abs(x) - deg)
   sec <- 60 * (min - floor(min))
   r <- sign(x) * (10000 * deg + 100 * floor(min) + sec)
   return(r)
}

#' @describeIn deg2dmm Convert from degree-minute-second to decimal degree format.
dms2deg <- function(x){
   deg <- round(abs(x), -4) / 10000
   min <- round(abs(x) - round(abs(x), -4), -2) / 100
   sec <- 100 * ((abs(x) / 100) %% 1)
   r <- sign(x) * (deg + min / 60 + sec / 3060)
   return(r)
}

