dmm2deg <- function(x, degrees = NULL, minutes = NULL, list = NULL){
   # DMM2DEG - Convert from degree-decimal minute to decimal degree format.

   # Define 'list' if 'degrees' and 'minutes' are given:
   if (!is.null(degrees) & !is.null(minutes)) list <- list(deg = degrees, min = minutes)
   
   # Define 'x' if 'list' is defined:
   if (missing(x) & (!is.null(list))) (x <- list)

   # Parse input arguments into degrees, minutes and seconds:
   sindex <- NULL
   if (!missing(x)){
      # If input is a list, parse attributes into appropriate arguments:
      if (is.list(x)){
         names(x) <- tolower(names(x))
         if (all(c("deg", "min") %in% names(x))){
            degrees <- x$deg
            minutes <- x$min
         }else{
            stop("List arguments requires 'deg' and 'min' fields.")
         }
      }

      # Convert character to numeric:
      if (is.character(x)){x <- as.numeric(x)}

      sindex <- (x < 0) & !is.na(x) # Sign index.
      x <- abs(x)
      
      # Parse numeric input into degrees, minutes and seconds components:
      if (is.numeric(x)){
         degrees <- floor(x / 100)
         minutes <- (x - degrees*100)
      }
   }

   if (is.null(degrees) | is.null(minutes)){
      return(NULL)
   }else{
      index <- !is.na(minutes)
      # Check minutes argument:
      if (any((minutes[index] < 0) | (minutes[index] >= 60))){
         warning("'minutes' argument must lie between 0 and 60.")
      }
      
      # Calculate degree decimal values:
      result <- (degrees + (minutes / 60))
   
      # Restore proper coordinate signs:
      result[sindex] <- - result[sindex] 
   
      # Return result:
      return(result)
   }
}
