#' Coordinate Conversion Functions
#'
#' @description Convert a decimal-degree coordinate to a formatted character string suitable for printing.
#'
#' @param deg A numeric vector of coordinates in decimal degree format.
#'
#' @param longitude,latitude Numeric decimal degree coordinate value(s). If \code{northing = TRUE} or
#'           \code{easting = TRUE} then the corresponding Northing (i.e. "N" or "S") or Easting character
#'           (i.e. "E" or "W") is included in the output character string.
#'
#' @param clip A logical value. Whether to strip away coordinate seconds or
#' minutes and seconds when they have zero values. The resulting output strings
#' are of an abridged form. The default is \code{TRUE}.
#'
#' @param northing,easting Logical value specifying whether to include a Northing (i.e. "N" or "S")
#'                         or Easting (i.e. "E" or "W") character in the output string when \code{longitude}
#'                         or \code{latitude} are specified.
#'
#' @param as.list Logical value specifying whether to return the output as a list object. The default is \code{FALSE}.
#'
#' @return If a single argument is provided and as.list is FALSE, then the
#' returned output is a character string vector. Otherwise, the output is
#' returned as a list with fields \code{longitude} and \code{latitude}.
#'
#' @examples
#' # Convert generic coordinate:
#' deg2str(c(45, 47.5))
#' deg2str(c(45, 47.5), clip = FALSE)
#'
#' # Convert latitude coordinate, which adds the northing character to the output:
#' deg2str(lat = c(45, 47.5))
#' deg2str(long = c(-66, -66.5))
#'
#' # Convert vectors of lat-long coordinates, which are returned as a list:
#' deg2str(latitude = 45:47, longitude = -64:-66, clip = FALSE)
#'
#' @seealso \code{\link{deg2dms}}, \code{\link{deg2dmm}}

#' @export deg2str
deg2str <- function(deg = NULL, latitude = NULL, longitude = NULL, clip = TRUE,
                    northing = TRUE, easting = TRUE, as.list = FALSE){

   count <- 1 # Initialize counter for number of output values.
   result <- list() # Initialize result variable.

   # Convert generic degree to a string:
   if (!is.null(deg)){
      deg.str = rep("", length(deg))
      for (i in 1:length(deg)){
         deg.deg <- abs(deg[i])
         deg.min <- (deg.deg - floor(deg.deg)) * 60
         deg.sec <- round((deg.min - floor(deg.min)) * 60, 1)

         deg.str[i] <- paste(as.character(floor(deg.deg)))
         # deg.str[i] <- paste0(deg.str[i], "°") # Add degree symbol.
         deg.str[i] <- paste0(deg.str[i], '\u00B0') # Add degree symbol.

         # Get minute string:
         if ((deg.min != 0) | (deg.sec != 0)){
            deg.min.str <- as.character(floor(deg.min))
            if (nchar(deg.min.str) < 2) deg.min.str <- paste0("0", deg.min.str)
            deg.str[i] <- paste0(deg.str[i], deg.min.str, "'")
         }else{
            if (!clip) deg.str[i] <- paste0(deg.str[i], "00", "'")
         }

         # Get seconds string:
         if (deg.sec != 0){
            deg.sec.str <- as.character(deg.sec,1)
            if (nchar(deg.sec.str) < 2) deg.sec.str <- paste0("0", deg.sec.str)
            deg.str[i] <- paste(deg.str[i], deg.sec.str, "''", sep = "")
         }else{
            if (!clip) deg.str[i] <- paste0(deg.str[i], "00", "''")
         }
      }
      result[["deg"]] <- deg.str
      count <- count + 1
   }

   # Convert latitudes:
   if (!is.null(latitude)){
      latitude.str <- rep("", length(latitude))
      for (i in 1:length(latitude)){
         latitude.str[i] <- deg2str(latitude[i], clip = clip)
         if (northing){
            if (latitude[i] < 0) latitude.str[i] <- paste0(latitude.str[i], "S") else latitude.str[i] <- paste0(latitude.str[i], "N")
         }
      }
      result[["lat"]] <- latitude.str
      count <- count + 1
   }

   # Convert longitudes:
   if (!is.null(longitude)){
      longitude.str <- rep("", length(longitude))
      for (i in 1:length(longitude)){
         longitude.str[i] <- deg2str(longitude[i], clip = clip)
         if (easting){
            if (longitude[i] < 0) longitude.str[i] <- paste0(longitude.str[i], "W") else longitude.str[i] <- paste0(longitude.str[i], "E")
         }
      }
      result[["long"]] <- longitude.str
      count <- count + 1
   }

   # If there are multiple arguments and as.list is FALSE, then output as a
   # character vector rather than a list.
   if ((length(result) == 1) & (!as.list)){
      result <- result[[1]]
   }

   return(result)
}
