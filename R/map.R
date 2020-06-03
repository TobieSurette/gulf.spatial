#' Map Gulf of St. Lawrence Features
#'
#' @description Draws southern Gulf of St. Lawrence geographical features.
#'
#' @param layer Layer specification
#'
#' @seealso \code{\link{read.gulf.spatial}}
#'
#' @examples
#' map()
#' map(layer = "strata", survey = "rv", region = "gulf")
#'
#' @export map
#' @export map.default
#'

#' @rdname map
map <- function(x, ...) UseMethod("map")

#' @rdname map
map.default <- function(x, layer, xlim, ylim, region = "gulf", add = TRUE, ...){
   if (!add | length(dev.list()) == 0){
      # Parse 'region':
      region <- match.arg(tolower(region), c("gsl", "gulf", "4rst", "sgsl", "southern gulf",
                                             "4t",  "ngsl", "northern gulf", "4s",
                                             "cb", "cape breton", "wcb", "western cape breton",
                                             "ns", "northumberland strait"))

      if (region %in% c("4t", "sgsl", "gulf"))       region <- "southern.gulf"
      if (region %in% c("4rst"))                     region <- "gsl"
      if (region %in% c("4s", "ngsl"))               region <- "northern.gulf"
      if (region %in% c("cb", "cape breton", "wcb")) region <- "western.cape.breton"
      if (region %in% c("ns"))                       region <- "northumberland.strait"

      # Define axis limits:
      if (missing(xlim)){
         if (region == "gsl")                   xlim <- c(-70, -56)
         if (region == "southern.gulf")         xlim <- c(-66.5, -60)
         if (region == "northern.gulf")         xlim <- c(-70, -59)
         if (region == "western.cape.breton")   xlim <- c(-62.5, -60)
         if (region == "northumberland.strait") xlim <- c(-65.5, -61.0)
      }
      if (missing(ylim)){
         if (region == "gsl")                   ylim <- c(45, 52)
         if (region == "southern.gulf")         ylim <- c(45.5, 49.3)
         if (region == "northern.gulf")         ylim <- c(47.5, 51)
         if (region == "western.cape.breton")   ylim <- c(45.5, 47.5)
         if (region == "northumberland.strait") ylim <- c(45.5, 47.75)
      }
      plot(xlim, ylim, type = "n", xlab = NA, ylab = NA)
   }

   # Load gulf coast:
   if (!missing(layer)){
       for (i in 1:length(layer)){
          v <- read.gulf.spatial(layer = layer[i], region = region, ...)
          plot(v, add = TRUE)
       }
   }
}
