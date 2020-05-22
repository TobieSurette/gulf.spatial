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
read.gulf.spatial <- function(layer, survey, project, ...){
   if (!missing(survey) & missing(project)) project <- paste0(survey, "s")

   # Parse map layer argument:
   layer <- match.arg(tolower(layer), c("bounds", "stations", "stratum", "strata", "fishing.zones",
                                        "kriging", "coast", "altitude", "bathymetry", "depth", "dem",
                                        "ports", "cities", "geography", "features"))
   layer <- gsub("strata", "stratum", layer)

   # Get list of all data files and paths:
   paths <- dir(path = find.package(package = "gulf.spatial"), recursive = TRUE, full.names = TRUE)
   paths <- paths[grep("data/", paths)]
   files <- unlist(lapply(strsplit(paths, "/"), function(x) x[length(x)]))
   f <- data.frame(path = paths, file = files, stringsAsFactors = FALSE)
   f$extension <- substr(f$file, nchar(f$file)-2, nchar(f$file))
   f$path <- substr(f$path, 1, nchar(f$path)-nchar(f$file))

   # Subset by project:
   if (!missing(project)) f <- f[grep(tolower(project), f$file), ]
   f <- f[grep(tolower(layer), f$file), ]

   # Remove unwanted file types:
   f <- f[tolower(f$extension) %in% c("csv", "shp", "txt", "tab"), ]

   if (nrow(f) == 0) stop("Unable to find spatial data.")
   if (nrow(f) > 1) stop("Arguments correspond to multiplespatial data files.")
   if (nrow(f) == 1){
      if (f$extension == "shp") v <- readOGR(paste0(f$path, f$file), verbose = FALSE)
      if (f$extension == "csv") v <- read.csv(paste0(f$path, f$file), header = TRUE, stringsAsFactors = FALSE)
   }

   subset(v, ...)

   return(v)
}

