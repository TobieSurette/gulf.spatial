## use CSV files of coordinates to produce a simple features version
## of the path of the COGIM cables

c1 <- readxl::read_xlsx("COGIM1-locations.xlsx")
c1$longitude <- gulf::dmm2deg(paste0(c1$longitude.degree, ifelse(c1$longitude.decimal.minute<10, paste0("0",c1$longitude.decimal.minute), c1$longitude.decimal.minute)))
c1$latitude <- gulf::dmm2deg(paste0(c1$latitude.degree, ifelse(c1$latitude.decimal.minute<10, paste0("0",c1$latitude.decimal.minute), c1$latitude.decimal.minute)))





c2 <- readxl::read_xlsx("COGIM2-locations.xlsx")
c2$longitude <- gulf::dmm2deg(paste0(c2$longitude.degree, ifelse(c2$longitude.decimal.minute<10, paste0("0",c2$longitude.decimal.minute), c2$longitude.decimal.minute)))
c2$latitude <- gulf::dmm2deg(paste0(c2$latitude.degree, ifelse(c2$latitude.decimal.minute<10, paste0("0",c2$latitude.decimal.minute), c2$latitude.decimal.minute)))

#library(gulf.spatial)
#map(region="gulf")
library(gulf)
gulf.map(xlim=c(-65,-61.5), ylim=c(47,48.75))
lines(c1$longitude, c1$latitude, lwd=2, col="blue")
lines(c2$longitude, c2$latitude, lwd=2, col="red")
legend("topright", c("COGIM 1","COGIM 2"), col=c("blue","red"), lwd=2)

library(sf)
c1$geometry <- st_sfc(st_linestring(as.matrix(cbind(c1$longitude, c1$latitude))), crs=4326)
c2$geometry <- st_sfc(st_linestring(as.matrix(cbind(c2$longitude, c2$latitude))), crs=4326)

c1.sf <- st_sf(data.frame(name="COGIM1", geometry=c1$geometry))
c2.sf <- st_sf(data.frame(name="COGIM2", geometry=c2$geometry))

out.sf <- rbind(c1.sf, c2.sf)

library(here)
write_sf(out.sf, file.path(here(), "inst/extdata/shapefiles/COGIM.shp")) ## silently overwrites shapefile
write_sf(out.sf, file.path(here(), "inst/extdata/shapefiles/COGIM.kml")) ## google earth format

