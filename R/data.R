
#' Polygons of fishing zones.
#'
#' A dataset containing the polygons of fishing zones. Each polygon is
#' constructed using the vertices defined in fishing.zone.vertices.
#' The coastline that is added to the vertices to create polygons is from
#' the Atlas of Canada.
#'
#' @format A simple feature data frame with 116 rows and 5 variables:
#' \describe{
#'   \item{type}{type of simple feature}
#'   \item{species.code}{species code of fishing zone}
#'   \item{region}{region of fishing zone}
#'   \item{label}{label of fishing zone}
#'   \item{geometry}{simple feature geometry}
#' }
#' @source \url{https://laws-lois.justice.gc.ca/eng/regulations/SOR-86-21/index.html}
#' @source \url{https://open.canada.ca/data/en/dataset/fec926ca-25ad-5c92-a9a7-6009c21d17b3}
#'
"fishing.zone.polygons"

#' Vertices of fishing zones.
#'
#' A dataset containing the vertices of fishing zones.
#'
#' @format A simple feature data frame with 116 rows and 5 variables:
#' \describe{
#'   \item{type}{type of simple feature}
#'   \item{species.code}{species code of fishing zone}
#'   \item{region}{region of fishing zone}
#'   \item{label}{label of fishing zone}
#'   \item{geometry}{simple feature geometry}
#' }
#' @source \url{https://laws-lois.justice.gc.ca/eng/regulations/SOR-86-21/index.html}
#'
"fishing.zone.vertices"
