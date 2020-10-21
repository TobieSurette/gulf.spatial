#' Read Spatial Data
#'
#' @name read
#' @description Read southern Gulf of Saint Lawrence spatial data files.
#'
#' @param layer Character string specifying the layer to be read.
#' @param ... Other arguments passed onto \code{\link{locate.gulf.spatial}}.
#'
#' @examples
#' read.gulf.spatial(layer = "bounds", survey = "rv")   # Read September survey boundaries.
#' read.gulf.spatial(layer = "bounds", survey = "sc")   # Read snow crab survey boundaries.
#' read.gulf.spatial(layer = "stations", survey = "rv") # Read September survey stations.
#' read.gulf.spatial(layer = "stations", survey = "ns") # Read Northumberland Strait survey stations.
#' read.gulf.spatial("port")                            # Coordinate port coordinates.
#'
#' @seealso \code{\link{locate.gulf.spatial}}

#' @export read.gulf.spatial
read.gulf.spatial <- function(layer, file, ...){
   # File extension function:
   fext <- function(x) return(tolower(unlist(lapply(strsplit(x, "[.]"), function(x) x[length(x)]))))

   # Find file:
   if (missing(file)) file <- locate.gulf.spatial(layer, ...)

   if (length(file) == 0) stop("Unable to find spatial data layer.")

   if (length(file) > 1) stop("Arguments correspond to multiple spatial data files.")

   v <- NULL
   if (length(file) == 1){
      # Read shapefile:
      if (fext(file) == "shp"){
         v <- rgdal::readOGR(file, verbose = FALSE)

         # Subset by survey and region:
         if (!missing(survey)) v <- gulf.utils::subset(v, survey = tolower(survey))
         if (!missing(region)) v <- gulf.utils::subset(v, region = tolower(region))
      }

      # Read CSV file:
      if (fext(file) == "csv") v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

      # Read MIF file:
      if (fext(file) == "mif") v <- read.mif(file)

      # Read R data files:
      if (fext(file) %in% c("rda", "rdata")){
         files <- ls()
         load(file)
         v <- setdiff(ls(), files)
         eval(parse(text = paste0("v = ", v)))
      }
   }

   #subset(v, ...)

   return(v)
}

