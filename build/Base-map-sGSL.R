
library(gulf)

clg()

xlim <- c(-66.5, -60.15)
ylim <- c(45.5, 48.5)
ratio <- distance(xlim[1], ylim[1], xlim[1], ylim[2]) / distance(xlim[1], ylim[1], xlim[2], ylim[1])
width <- 9
windows(height = ratio * width, width = width);
plot(xlim, ylim, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
coastline(col = grey(0.9), border = grey(0.6))
bathymetry(dem = FALSE, levels =  -c(10, 25, 50, 100, 150, 200, 300, 400))
map.place.names(language = "english")
box()

# Plot DFO 10 x 10 minute grids:

DFOgrid.str(-64.5, 46.5)



# Load stratum polygon definitions:
data("fishing.zone.polygons")
p <- subset(fishing.zone.polygons, region = region, species = species, fishing.zone = fishing.zone)

   # Draw polygons:
   plot.polygon(p, ...)

box()
   