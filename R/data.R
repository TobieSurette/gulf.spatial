#' Polygons of fishing zones boundaries
#'
#' A dataset containing the longitudes and latitudes of polygon vertices and polygons
#' with coastline for fishing zones in the Gulf of St. Lawrence.
#'
#'
#' @format An R object of class simple feature data frame
#' \describe{
#'   \item{type}{type of polygon, in this case "fishing zone"}
#'   \item{species.code}{species code of the species for a given fishing zone}
#'   \item{region}{region of the fishing zone}
#'   \item{label}{identifier for a fishing zone}
#'   \item{geometry}{simple feature geometry of fishing zone as a MULTIPOLYGON}
#'   ...
#' }#'
"fishing.zone.polygons"
