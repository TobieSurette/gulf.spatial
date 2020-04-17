DFOgrid.str <- function(longitude, latitude,  quarter.grid = FALSE){
   # DFOGRID.STR - Returns a grid name for a given set of coordinates.

   longitude <- -abs(longitude)
   
   # Specify reference grid:
   ref.longitude   <- -66-1/3 
   ref.latitude    <- 45.0  
   delta.longitude <- 1/6 
   delta.latitude  <- 1/6
   ref.grid        <- "HP23"
                        
   # Calculate 'x' index:
   xi <- as.numeric(substr(ref.grid, 3, 4)) + floor((longitude - ref.longitude)*(1/delta.longitude))
   x.str <- gsub(" ", "0", formatC(xi, width = 2))
   
   # Calculate 'y' index:
   ref.grid <- toupper(ref.grid)
   yi <- (match(substr(ref.grid, 1, 1), LETTERS)-1) * 26 + (match(substr(ref.grid, 2, 2), LETTERS)-1)
   yi <- yi - floor((latitude-ref.latitude)*(1/delta.latitude))
   y.str <- paste0(LETTERS[floor(yi / 26) + 1], LETTERS[yi %% 26 + 1])
   str <- paste0(y.str, x.str)
   
   if (quarter.grid){
      str <- paste0(str, c("E", "W")[(((abs(longitude)/delta.longitude) - floor(abs(longitude)/delta.longitude)) >= 0.5)+1])
      str <- paste0(substr(str,1,2), 
                    c("S", "N")[(((latitude/delta.latitude) - floor(latitude/delta.latitude)) >= 0.5)+1],
                    substr(str,3,5))
   }   
   
   return(str)
}
