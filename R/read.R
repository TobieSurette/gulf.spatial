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
#' read.gulf.spatial("cities")                          # City coordinates.
#'
#' # Fishing zones:
#' read.gulf.spatial("fishing zone polygon", file = "shp", species = 2526,  region = "gulf", zone = 12)
#' read.gulf.spatial("fishing zone vertices", file = "shp", species = 2550,  region = "gulf", lfa = 24)
#' read.gulf.spatial("kriging", survey = "scs")
#' @seealso \code{\link{locate.gulf.spatial}}

#' @export read.gulf.spatial
read.gulf.spatial <- function(layer, region,  species, zone, area, lfa, ...){
   # File extension function:
   fext <- function(x) return(tolower(unlist(lapply(strsplit(x, "[.]"), function(x) x[length(x)]))))

   # Find file:
   file <- locate.gulf.spatial(layer, ...)
   if (length(file) == 0) stop("Unable to find spatial data layer.")
   if (length(file) > 1) stop("Arguments correspond to multiple spatial data files.")

   v <- NULL
   if (length(file) == 1){
      # Read shapefile:
      if (fext(file) == "shp"){
         v <- rgdal::readOGR(file, verbose = FALSE)
         names(v) <- gsub("spcs_cd", "species", names(v))

         # Subset by survey and region:
         if (!missing(species)) if ("species" %in% names(v)) v <- v[v@data$species %in% species, ]
         if (!missing(region))  if ("region" %in% names(v))  v <- v[v@data$region %in% region, ]
         if (!missing(zone))    if ("label" %in% names(v))   v <- v[v@data$label %in% zone, ]
         if (!missing(area))    if ("label" %in% names(v))   v <- v[v@data$label %in% area, ]
         if (!missing(lfa))     if ("label" %in% names(v))   v <- v[v@data$label %in% lfa, ]
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

