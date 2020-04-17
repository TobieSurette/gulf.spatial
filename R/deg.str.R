deg.str <- function(deg = NULL, latitude = NULL, longitude = NULL, clip = TRUE,
                    northing = TRUE, easting = TRUE, as.list = FALSE){

   # DMS.STR - Converts coordinates from decimal degrees to a character string.

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
         deg.str[i] <- paste(deg.str[i], "°", sep = "") # Add degree symbol.

         # Get minute string:
         if ((deg.min != 0) | (deg.sec != 0)){
            deg.min.str <- as.character(floor(deg.min))
            if (nchar(deg.min.str) < 2){
               deg.min.str <- paste("0", deg.min.str, sep = "")
            }
            deg.str[i] <- paste(deg.str[i], deg.min.str, "'", sep = "")
         }else{
            if (!clip){
               deg.str[i] <- paste(deg.str[i], "00", "'", sep = "")
            }
         }

         # Get seconds string:
         if (deg.sec != 0){
            deg.sec.str <- as.character(deg.sec,1)
            if (nchar(deg.sec.str) < 2){
               deg.sec.str <- paste("0", deg.sec.str, sep = "")
            }
            deg.str[i] <- paste(deg.str[i], deg.sec.str, "''", sep = "")
         }else{
            if (!clip){
               deg.str[i] <- paste(deg.str[i], "00", "''", sep = "")
            }
         }
      }
      result[["deg"]] <- deg.str
      count <- count + 1
   }

   # Convert latitudes:
   if (!is.null(latitude)){
      latitude.str <- rep("", length(latitude))
      for (i in 1:length(latitude)){
         latitude.str[i] <- deg.str(latitude[i], clip = clip)
         if (northing){
            if (latitude[i] < 0){ # Check if coordinate is in northern of southern hemisphere.
               latitude.str[i] <- paste(latitude.str[i], "S", sep = "")
            }else{
               latitude.str[i] <- paste(latitude.str[i], "N", sep = "")
            }
         }
      }
      result[["lat"]] <- latitude.str
      count <- count + 1
   }

   # Convert longitudes:
   if (!is.null(longitude)){
      longitude.str <- rep("", length(longitude))
      for (i in 1:length(longitude)){
         longitude.str[i] <- deg.str(longitude[i], clip = clip)
         if (easting){
            if (longitude[i] < 0){ # Check if coordinate is in northern of southern hemisphere.
               longitude.str[i] <- paste(longitude.str[i], "W", sep = "")
            }else{
               longitude.str[i] <- paste(longitude.str[i], "E", sep = "")
            }
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
