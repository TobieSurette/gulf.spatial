in.polygon <- function(p, x, y){
   # IN.POLYGON - Determine whether a point lies within a list of polygon.

   require(sp) # Uses 'point.in.polygon' in the 'sp' package.

   # Check input arguments:
   if (missing(x) | missing(y)) stop("'x' and 'y' must be specified.")
   if (length(x) != length(y))  stop("'x' and 'y' must be the same length.")
   
   # Check that 'p' is a list object:
   if (!is.list(p)) stop("'p' must be a either a 'polygon' or a 'list' object.")

   # Convert 'p' to 'polygon' class:
   if (all(c("x", "y") %in% names(p))){
      p <- as.polygon(x = p$x, y = p$y, hole = p$hole)
   }

   if (is(p, "polygon")){
      index <- rep(FALSE, length(x))
      # Loop over normal polygons:
      for (i in 1:length(p)){
         if (all(c("x", "y", "hole") %in% names(p[[i]]))){
            if (!p[[i]]$hole){
               index <- index | (point.in.polygon(x, y, p[[i]]$x, p[[i]]$y, mode.checked = FALSE) > 0)
            }
         }
      }
      # Loop over 'holes':
      for (i in 1:length(p)){
         if (all(c("x", "y", "hole") %in% names(p[[i]]))){
            if (p[[i]]$hole){
               index <- index & !(point.in.polygon(x, y, p[[i]]$x, p[[i]]$y, mode.checked = FALSE) > 0)
            }
         }
      }
   }else{
      # Collate logical matrix:
      index <- NULL
      for (i in 1:length(p)){
         index <- cbind(index, in.polygon(p[[i]], x, y))
      }
   }
   
   return(index)
}
