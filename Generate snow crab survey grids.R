library(sp)

load("/Users/crustacean/Desktop/gulf.spatial/stratum.polygons.rda")

p <- list()
for (i in 1:length(mif)){
   x <- as.data.frame(mif[[i]][c("longitude", "latitude")])
   tmp <- list(Polygon(x[, 1:2], hole = FALSE))
   p[[i]] <- Polygons(tmp, i)
}
SP <- SpatialPolygons(p)

# Compile survey summary data:
vars <- "tow.id"
data <- NULL
for (i in 1:length(vars)){
   tmp <- lapply(mif, function(x) if (is.null(x[[vars[i]]])) return(NA) else return(x[[vars[i]]]))
   data <- cbind(data, unlist(tmp))
}
data <- as.data.frame(data)

# Combine polygons and data:
y <- SpatialPolygonsDataFrame(SP, data)

writeOGR(y, layer = "scs.grids",
         dsn = "scs.grids", driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# Read and plot shape files:
z <- readOGR(layer = "survey.stratum.polygons", dsn = "inst/extdata/shapefiles")
#plot(z, ID = "gulf")

plot(c(-66, -60), c(45, 49))
polygon(z@polygons[[3]]@Polygons[[1]]@coords)
title(main = z@data[3, 1])

dir(path = paste0(find.package("gulf.spatial"), "/extdata/"))