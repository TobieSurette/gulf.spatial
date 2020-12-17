library(gulf.utils)
library(gulf.spatial)
library(sf)

b <- read.gulf.spatial("kriging polygons revised")

zones <- c("zone12", "zoneE", "zoneF", "zone19_F_buffer", "zoneEF_unassigned", "zone19_12_buffer")

# Join polygons together:
p = st_sf(st_sfc(st_polygon(list(cbind(b[[zones[1]]]$longitude, b[[zones[1]]]$latitude)))))
for (i in 2:length(zones)){
   tmp = st_sf(st_sfc(st_polygon(list(cbind(b[[zones[i]]]$longitude, b[[zones[i]]]$latitude)))))
   p <- st_union(p, tmp)
}

# Splice Maggies hole into polygon:
p1 <- as.matrix(p)[[1]][[1]]
p2 <- as.matrix(p)[[1]][[2]]

ix <- which(p1[,1] == -61.04130)
a <- rbind(p1[1:ix, ], p2, p1[ix:nrow(p1), ])

plot(a[, 1], a[, 2], type = "l")

b$zone12_expanded <- list(longitude = a[, 1], latitude = a[,2], name = "zone12_expanded")

kriging.polygons <- b
save(kriging.polygons, file = "inst/extdata/kriging.polygons.revised.rda")
