#' Locate Spatial Data Files
#'
#' @name locate
#' @description Read southern Gulf spatial data files.
#'
#' @param layer Character string specifying the layer to be read.
#' @param survey Character string specifying the name of the science survey to be read.
#' @param project Character string specifying the name of the science project to be read.
#'
#' @examples
#' locate.gulf.spatial(layer = "bounds", survey = "rv")   # Locate September survey boundaries.
#' locate.gulf.spatial(layer = "bounds", survey = "sc")   # Locate snow crab survey boundaries.
#' locate.gulf.spatial(layer = "stations", survey = "rv") # Locate September survey stations.
#' locate.gulf.spatial(layer = "stations", survey = "ns") # Locate Northumberland Strait survey stations.
#' locate.gulf.spatial("grids", survey = "ns")            # Locate Northumberland Strait survey grids.
#'
#' @section Geographic Layers:
#' \describe{
#'   \item{\code{altitude}, \code{bathymetry}, \code{dem}, \code{depth}}{Load altitude and bathymetry data.}
#'   \item{\code{borders}, \code{bounds}}{Load the bounds or borders of a specified area.}
#'   \item{\code{grids}}{Science survey spatial grids designs.}
#'   \item{\code{cities}}{Load city coordinates.}
#'   \item{\code{fishing.zones}}{Load fishing zone polygons and borders.}
#'   \item{\code{kriging}}{Load polygons used in area abundance and biomass estimation.}
#'   \item{\code{stations}}{Load survey or project sampling stations.}
#'   \item{\code{strata}, \code{stratum}}{Load survey strata.}
#'   \item{\code{seaports}, \code{ports}}{Load sea ports.}
#'   \item{\code{geography}, \code{features}}{Load land and marine geographical features.}
#' }
#'

#' @export locate.gulf.spatial
locate.gulf.spatial <- function(layer, survey, region, project,
                                file.extensions =  c("csv", "shp", "txt", "tab", "rda", "rdata", "mif"),
                                resolution = "high",
                                ...){
   # File extension function:
   fext <- function(x) return(unlist(lapply(strsplit(x, "[.]"), function(x) x[length(x)])))

   # Parse map layer argument:
   layer <- gsub("strata", "stratum", layer)
   layer <- gsub("coastline", "coast", layer)
   layer <- gsub("ports", "port", layer)
   layer <- gsub("cities", "city", layer)
   layer <- unlist(strsplit(layer, "[. ,;]"))

   # Parse coastline resolution argument:
   resolution <- match.arg(tolower(resolution), c("low", "intermediate", "high", "full"))

   # Default project:
   if (missing(project)) if (!missing(survey)) project <- gulf.metadata::project(survey) else project <- ""

      # Get list of all data files and paths:
   if (missing(project))  file <- gulf.utils::locate(package = "gulf.spatial", keywords = layer, ...)
   if (!missing(project)) file <- gulf.utils::locate(package = "gulf.spatial", keywords = c(layer, project), ...)

   # Filter out by file extension:
   if (length(file) > 0) file <- file[fext(file) %in% file.extensions]

   # Add specific file filters if necessary:
   if (length(file) > 1){
      tmp <- file
      file <- file[grep(resolution, file)]
      if (length(file) == 0) file <- tmp
   }

   return(file)
}
