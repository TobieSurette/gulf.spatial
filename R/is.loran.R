is.loran <- function(x){
   # IS.LORAN - Checks whether a coordinate entry is a LORAN-type coordinate.
   
   return(!is.na(as.loran(x)))
}
