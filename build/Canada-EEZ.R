## Canada EEZ
eez <- read_sf("C:/Users/RicardD/Downloads/eez/eez.shp")

library(ggplot2)
g <- ggplot(data=eez) +
   geom_sf(fill=grey(0.8), color=grey(0.3))
g


## a polygon to clip the EEZ 200nm off Labrador and Newfoundland

t.df <- rbind(
   c(-60,60),
   c(-55.5,55.5),
   c(-51.5,51.5),
   c(-50.5,46.5),
   c(-52.5,44.5),
   c(-47.5,44.5),
   c(-46.5,46.5),
   c(-46.5,51.5),
   c(-50.5,55.5),
   c(-55,60)
)

nfld.eez.poly <- st_sfc(st_cast(st_linestring(t.df), "POLYGON"), crs=4326)


g+geom_sf(data=nfld.eez.poly, aes(fill="mistyrose"))

## get the
st_crop(eez, nfld.eez.poly)
