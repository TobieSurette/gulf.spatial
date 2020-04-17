deg2dms <- function(x, as.list = FALSE, digits = NULL){
   # DEG2DMS - Convert from decimal degree to degree-minute-second format.

   index <- (x < 0) & !is.na(x)
   x <- abs(x)
   
   # Parse 'x' into degrees, minutes and seconds:
   deg <- x
   min <- (deg - floor(deg)) * 60
   sec <- (min - floor(min)) * 60
   deg <- floor(deg)
   min <- floor(min)

   # Perform rounding if required:
   if (!is.null(digits)) sec <- round(sec * (10^digits)) / (10^digits)
   
   # Perform threshold corrections:
   index2 <- !is.na(x) & (sec >= 60) 
   sec[index2] <- sec[index2] - 60
   min[index2] <- min[index2] + 1   
   index2 <- !is.na(x) & (min >= 60)
   min[index2] <- min[index2] - 60
   deg[index2] <- deg[index2] + 1

   if (!as.list){
      # Return results in numeric format:
      result <- deg * 10000 + min * 100 + sec
      result[index] <- -result[index]
   }else{
      # Return results as a list:
      result <- list(deg = deg, min = min, sec = sec)
   }

   return(result)
}
