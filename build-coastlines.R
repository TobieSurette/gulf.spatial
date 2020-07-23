library(rgdal)
library(raster)
library(rgeos)

clip <- function(x, xlim, ylim){
   bbox <- rbind(xlim, ylim)
   if (class(bbox) == "matrix") b_poly <- as(extent(as.vector(t(bbox))), "SpatialPolygons") else b_poly <- as(extent(bbox), "SpatialPolygons")
   return(gIntersection(x, b_poly, byid = TRUE))
}

library(sf)
w <- st_read("/Users/crustacean/Desktop/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp")
#w <- readOGR("/Users/crustacean/Desktop/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp")
w = clip(w, xlim = c(-69, -52), ylim = c(43, 52))

plot(w, xlim = c(-66.5, -57), ylim = c(43, 52))

writeOGR(w, "Test.shp", driver = "ESRI Shapefile")

