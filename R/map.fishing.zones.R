#' @export map.fishing.zones
map.fishing.zones <- function(region = "gulf", species = NULL, fishing.zone = NULL,
                              labels = FALSE, cex = 0.7, ...){
   # MAP.FISHING.ZONES - Plot fishing zone boundaries for various species.

   # Load stratum polygon definitions:
   data("fishing.zone.polygons")
   p <- subset(fishing.zone.polygons, region = region, species = species, fishing.zone = fishing.zone)

   # Draw polygons:
   plot.polygon(p, ...)

   # Plot labels:
   if (labels){
      if (all(c("label.x", "label.y") %in% names(p[[i]]))){
         x <- p[[i]]$label.x
         y <- p[[i]]$label.y
      }else{
         x <- mean(p[[i]]$x)
         y <- mean(p[[i]]$y)
      }
      if ("label.angle" %in% names(p[[i]])) angle <- p[[i]]$label.angle else angle <- 0
      text(x, y, p[[i]]$label, srt = angle, cex = cex)
   }
}
