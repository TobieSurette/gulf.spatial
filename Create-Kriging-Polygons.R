load("/Users/crustacean/Desktop/gulf.spatial/kriging.polygons.revised.rda")

p <- list()
for (i in 1:length(kriging.polygons)){
   x <- kriging.polygons[[i]]
   tmp <- Polygon(cbind(x$longitude, x$latitude), hole = FALSE)
   p[[i]] <- Polygons(list(tmp), i)
}
SP <- SpatialPolygons(p)

data <- data.frame(name = names(kriging.polygons),
                   species = "snow crab", stringsAsFactors = FALSE)

y <- SpatialPolygonsDataFrame(SP, data)

writeOGR(y, layer = "scs.kriging.polygons",
         dsn = 'inst/extdata/shapefiles', driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# Read and plot shape files:
z <- readOGR(layer = "scs.kriging.polygons", dsn = "inst/extdata/shapefiles")
#plot(z, ID = "gulf")

plot(c(-66, -60), c(45, 49))
polygon(z@polygons[[3]]@Polygons[[1]]@coords)
title(main = z@data[3, 1])

dir(path = paste0(find.package("gulf.spatial"), "/extdata/"))






