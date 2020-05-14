library(gulf.utils)
library(gulf.spatial)

xx <- seq(-66.5 + 1/12, -60 - 1/12, by  = 1/6)
yy <- seq(45.5-1/12, 49 + 1/12, by  = 1/6)
x <- expand.grid(xx, yy)
x$grid <- deg2grid(x[,1], x[,2])

x <- grid2deg(x$grid, vertices = TRUE)

x$depth <- depth(x$longitude, x$latitude)

b <- read.csv("inst/extdata/scs.bounds.csv")
p <- as.polygon(b$longitude, b$latitude)

grids <- unique(x$grid[which(in.polygon(p, x$longitude, x$latitude))])
x <- x[which((x$grid %in% grids) | (x$grid == "")), ]

plot(c(-66.5, -60), c(45, 49), type = "n")
coast()
plot.grid(grids)
excel(grid2deg(grids, vertices = TRUE))
