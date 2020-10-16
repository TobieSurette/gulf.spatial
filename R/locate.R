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
#' locate.gulf.spatial(layer = "bounds", survey = "rv")   # Read September survey boundaries.
#' locate.gulf.spatial(layer = "bounds", survey = "sc")   # Read snow crab survey boundaries.
#' locate.gulf.spatial(layer = "stations", survey = "rv") # Read September survey stations.
#' locate.gulf.spatial(layer = "stations", survey = "ns") # Read Northumberland Strait survey stations.
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

#' @export locate.gulf.spatial
locate.gulf.spatial <- function(layer, survey, region, project,
                                file.extensions =  c("csv", "shp", "txt", "tab", "rda", "rdata"),
                                resolution = "high",
                                ...){
  # File extension function:
  fext <- function(x) return(unlist(lapply(strsplit(x, "[.]"), function(x) x[length(x)])))
  
  # Parse map layer argument:
  layer <- match.arg(tolower(layer), c("bounds", "stations", "stratum", "strata", "fishing.zones",
                                       "kriging", "coastline", "altitude", "bathymetry", "depth", "dem",
                                       "ports", "cities", "geography", "features"))
  layer <- gsub("strata", "stratum", layer)
  layer <- gsub("coastline", "coast", layer)
  
  # Parse coastline resolution argument:
  resolution <- match.arg(tolower(resolution), c("low", "intermediate", "high", "full"))
  
  # Default project:
  if (missing(project)) if (!missing(survey)) project <- gulf.metadata::project(survey) else project <- ""
  
  # Get list of all data files and paths:
  if (missing(project)) file <- gulf.utils::locate(package = "gulf.spatial", file = layer)
  if (!missing(project)) file <- gulf.utils::locate(package = "gulf.spatial", file = c(layer, project))
  
  # Filter out by file extension:
  file <- file[fext(file) %in% file.extensions]
  
  # Add specific file filters if necessary:
  if (length(file) > 1) file <- file[grep(resolution, file)]
  
  return(file)
}
