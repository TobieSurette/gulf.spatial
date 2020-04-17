deg2dmm <- function(x, as.list = FALSE, digits = NULL){
   # DEG2DMM - Convert from decimal degree to degree decimal-minute format.

   index <- (x < 0) & !is.na(x)
   x <- abs(x)
   
   # Parse 'x' into degrees, minutes:
   deg <- floor(x)
   min <- (x - deg) * 60
   
   # Perform rounding if required:
   if (!is.null(digits)) min <- round(min * (10^digits)) / (10^digits)

   # Perform threshold correction:
   index2 <- !is.na(x) & (min >= 60) 
   min[index2] <- 0
   deg[index2] <- deg[index2] + 1   
      
   # Return results as a list:
   if (as.list){
      result <- list(deg = deg, min = min)
   }else{
      # Return results in numeric format:
      result <- deg * 100 + min
      result[index] <- -result[index]
   }

   return(result)
}
