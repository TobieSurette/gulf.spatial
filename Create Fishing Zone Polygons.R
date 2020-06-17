library(gulf.spatial)
library(here)
library(sf)

library(tidyverse)

#################
## Gulf



#################
## Quebec
## https://inter-l01-uat.dfo-mpo.gc.ca/infoceans/sites/infoceans/files/Homard.pdf
## https://inter-l01-uat.dfo-mpo.gc.ca/infoceans/en/commercial-fisheries#carte

## polygons with points on land, to be used with a land overlay for mapping, and below for building polygons with coastline
zph.15 <- data.frame(
   x=-dms2deg(c(570645, 592330, 595440, 602000, 571000, 570645)),
   y=dms2deg(c(512500, 495100,501655, 505000, 515000, 512500))
)
poly.15 <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA15",
      st_sfc(st_multipolygon(list(list(cbind(zph.15$x,zph.15$y)))), crs=4326)
   )
)



zph.16 <- data.frame(
   x=-dms2deg(c(595440, 592330, 600000, 611600, 611600,611600, 600000, 595440)),
   y=dms2deg(c(501655, 495100, 492500, 495030, 501025, 502000, 502000, 501655))
)
poly.16 <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA16",
      st_sfc(st_multipolygon(list(list(cbind(zph.16$x,zph.16$y)))), crs=4326)
   )
)

zph.17 <- data.frame(
   x=-dms2deg(c(600000,600000,630000,650000,642030,611600,600000)),
   y=dms2deg(c(492500,475000,483000,495130,500630,495030,492500))
)
poly.17 <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA17",
      st_sfc(st_multipolygon(list(list(cbind(zph.17$x,zph.17$y)))), crs=4326)
   )
)

zph.17A <- data.frame(
   x=-dms2deg(c(642200,643730,640500,634600,632400,632400,640000,642200)),
   y=dms2deg(c(494842,493630,491330,490130,492100,492500,494842,494842))
)
poly.17A <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA17A",
      st_sfc(st_multipolygon(list(list(cbind(zph.17A$x,zph.17A$y)))), crs=4326)
   )
)

zph.17B <- data.frame(
   x=-dms2deg(c(640000,642200,643730,650000,642030,611600,600000,600000,630000,634600,632400,640000)),
   y=dms2deg(c(494842,494842,493630,495130,500630,495030,492500,475000,483000,490130,492100,494842))
)
poly.17B <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA17B",
      st_sfc(st_multipolygon(list(list(cbind(zph.17B$x,zph.17B$y)))), crs=4326)
   )
)

zph.18 <- data.frame(
   x=-dms2deg(c(694203,693530,690000,674009,662200,650000, 642030,611600,611600,660000,680000,694203,694203)),
   y=dms2deg(c(480806,480230,482700,485634,492405,495130, 500630,495030,501025,503000,500000,483000, 480806))
)
poly.18 <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA18",
      st_sfc(st_multipolygon(list(list(cbind(zph.18$x,zph.18$y)))), crs=4326)
   )
)

zph.19A <- data.frame(
   x=-dms2deg(c(645500, 644400, 650000, 651633, 655239, 662200, 674009, 690000, 693530, 694203, 704840, 704411, 645500)),
   y=dms2deg(c(491200, 494030, 495130, 494605, 493404, 492405, 485634, 482700, 480230, 480806, 470257, 465606, 491200))
)
poly.19A <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA19A",
      st_sfc(st_multipolygon(list(list(cbind(zph.19A$x,zph.19A$y)))), crs=4326)
   )
)

zph.19B <- data.frame(
   x=-dms2deg(c(645500, 644400, 640500, 642400, 643000, 645500, 645500)),
   y=dms2deg(c(491200,  494030, 491330, 490032, 485500, 490400, 491200))
)
poly.19B <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA19B",
      st_sfc(st_multipolygon(list(list(cbind(zph.19B$x,zph.19B$y)))), crs=4326)
   )
)

zph.19C <- data.frame(
   x=-dms2deg(c(642400,640500,630000,641330,642400)),
   y=dms2deg(c(490032,491330,483000,484300,490032))
)
poly.19C <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA19C",
      st_sfc(st_multipolygon(list(list(cbind(zph.19C$x,zph.19C$y)))), crs=4326)
   )
)

zph.20A <- data.frame(
   x=-dms2deg(c(641330,630000,640210,643000,644136,641330)),
   y=dms2deg(c(484300,483000,481215,481215,481936,484300))
)
poly.20A <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA20A",
      st_sfc(st_multipolygon(list(list(cbind(zph.20A$x,zph.20A$y)))), crs=4326)
   )
)

zph.20B <- data.frame(
   x=-dms2deg(c(644136,643000,650330,652910,652910,644136)),
   y=dms2deg(c(481936,481215,475745,475142,480200,481936))
)
poly.20B <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA20B",
      st_sfc(st_multipolygon(list(list(cbind(zph.20B$x,zph.20B$y)))), crs=4326)
   )
)

zph.21A <- data.frame(
   x=-dms2deg(c(655410,655410,655000,652910,652910,655410)),
   y=dms2deg(c(481236,480147,480130,475142,480200,481236))
)
poly.21A <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA21A",
      st_sfc(st_multipolygon(list(list(cbind(zph.21A$x,zph.21A$y)))), crs=4326)
   )
)

zph.21B <- data.frame(
   x=-dms2deg(c(655410,655410,661921,662130,662048,655410)),
   y=dms2deg(c(481236,480147,480316,480504,480557,481236))
)
poly.21B <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA21B",
      st_sfc(st_multipolygon(list(list(cbind(zph.21B$x,zph.21B$y)))), crs=4326)
   )
)

zph.22 <- data.frame(
   x=-dms2deg(c(600000, 604500, 634730, 640210, 640210, 630000, 600000)),
   y=dms2deg(c(475000, 470218, 470758, 471825, 481215, 483000, 475000))
)
poly.22 <- st_sf(
   data.frame(
      type="fishing zone polygon",
      species.code=2550,
      region="Quebec",
      name="LFA22",
      st_sfc(st_multipolygon(list(list(cbind(zph.22$x,zph.22$y)))), crs=4326)
   )
)


quebec.lfas.1 <- rbind(poly.15, poly.16, poly.17, poly.17A, poly.17B, poly.18, poly.19A, poly.19B, poly.19C, poly.20A, poly.21A, poly.21B, poly.22)

## now create another set of polygons that are bounded by the coastline
## coastline file from the Atlas of Canada
# download.file(
#    paste(
#       "http://ftp.geogratis.gc.ca",
#       "pub/nrcan_rncan/vector/framework_cadre/Atlas_of_Canada_1M",
#       "boundary/AC_1M_BoundaryPolygons.shp.zip",
#       sep="/"
#    ),
#    "AC_1M_BoundaryPolygons.shp.zip"
# )
#unzip("AC_1M_BoundaryPolygons.shp.zip", exdir="AC")

boundaries <- read_sf("AC/AC_1M_BoundaryPolygons_shp/AC_1M_BoundaryPolygons.shp")

boundaries_simple <- boundaries %>%
   filter(
      POL_DIV %in% c(
         "Quebec" #,
         #"New York", "New Hampshire", "Vermont",
         #"Maine", "New Brunswick", "Nova Scotia",
         #"Prince Edward Island"
      ),
      SELECTION == "sparse"
   ) %>%
   st_transform(4326)




## create 2 shapefiles:
## 1 - shapefile of polygon vertices without coastline (suitable to make a map with a coastline overlay)
## 2 - shapefile of polygons with coastlines

lfas.1 <- rbind(quebec.lfas.1)

write_sf(lfas.1, file.path(here(), "inst/extdata/shapefiles/fishing.zone.vertices.shp")) ## silently overwrites shapefile
#write_sf(lfas.2, file.path(here(), "inst/extdata/shapefiles/fishing.zone.polygons.shp")) ## silently overwrites shapefile

## test map for Quebec LFAs

plot(lfas.1)

#################
## Newfoundland
