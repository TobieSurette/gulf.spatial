#' Read Spatial Data
#'
#' @description Read southern Gulf spatial data files.
#'
#' @param layer Character string specifying the layer to be read.
#' @param survey Character string specifying the name of the science survey to be read.
#' @param project Character string specifying the name of the science project to be read.
#'
#' @examples
#' read.gulf.spatial(layer = "bounds", survey = "rv") # Read Septmeber survey boundaries.
#' read.gulf.spatial(layer = "bounds", survey = "sc") # Read snow crab survey boundaries.
#' read.gulf.spatial(layer = "stations", survey = "rv") # Read Septmeber survey stations.
#' read.gulf.spatial(layer = "stations", survey = "ns") # Read Northumberland Strait survey stations.
#'
#' @section Geographic Layers:
#' \describe{
#'   \item{\code{altitude}, \code{bathymetry}, \code{dem}, \code{depth}}{Load altitude and bathymetry data.}
#'   \item{\code{borders}, \code{bounds}}{Load the bounds or borders of a specified area.}
#'   \item{\code{cities}, \code{towns}}{Load towns and cities.}
#'   \item{\code{fishing.zones}}{Load fishing zone polygons and borders.}
#'   \item{\code{kriging}}{Load polygons used in area abundance and biomass estimation.}
#'   \item{\code{stations}}{Load survey or project sampling stations.}
#'   \item{\code{strata}, \code{stratum}}{Load survey strata.}
#'   \item{\code{seaports}, \code{ports}}{Load sea ports.}
#'   \item{\code{geography}, \code{features}}{Load land and marine geographical features.}
#' }
#'
#' @export read.gulf.spatial
#'

# File extension function:
fext <- function(x) return(tolower(unlist(lapply(strsplit(x, "[.]"), function(x) x[length(x)]))))

read.gulf.spatial <- function(layer, survey, region, project, file.extensions =  c("csv", "shp", "txt", "tab"), ...){
   if (!missing(survey) & missing(project)) project <- paste0(survey, "s")

   # Parse map layer argument:
   layer <- match.arg(tolower(layer), c("bounds", "stations", "stratum", "strata", "fishing.zones",
                                        "kriging", "coast", "altitude", "bathymetry", "depth", "dem",
                                        "ports", "cities", "geography", "features"))
   layer <- gsub("strata", "stratum", layer)

   # Default project:
   if (missing(project)) if (!missing(survey)) project <- project(survey) else project <- ""

   # Get list of all data files and paths:
   file <- file.locate(package = "gulf.spatial", layer)
   file <- file[fext(file) %in% file.extensions]
   if (length(file) > 1){
      file <- file.locate(package = "gulf.spatial", c(layer, project))
      file <- file[fext(file) %in% file.extensions]
   }

   if (length(file) == 0) stop("Unable to find spatial data.")
   if (length(file) > 1) stop("Arguments correspond to multiple spatial data files.")
   if (length(file)== 1){
      if (fext(file) == "shp"){
         v <- rgdal::readOGR(file, verbose = FALSE)

         # Subset by survey and region:
         if (!missing(survey)) v <- v[v$survey %in% tolower(survey), ]
         if (!missing(region)) v <- v[v$region %in% tolower(region), ]
      }
      if (fext(file) == "csv") v <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
   }



   subset(v, ...)

   return(v)
}

