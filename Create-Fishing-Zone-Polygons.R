## creation of the fishing.zones.polygons and fishing.zones.vertices for inclusion in gulf.spatial
##
library(gulf.spatial)
library(here)
library(sf)
library(tidyverse)

## coastline
## coastline file from the Atlas of Canada
## https://open.canada.ca/data/en/dataset/fec926ca-25ad-5c92-a9a7-6009c21d17b3
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
         "Quebec", "Newfoundland and Labrador" ,
         #"New York", "New Hampshire", "Vermont",
         #"Maine",
         "New Brunswick", "Nova Scotia",
         "Prince Edward Island"
      ),
      SELECTION == "sparse" #"dense"
   ) %>%
   st_transform(4326)


##########################################################################
## Newfoundland and Labrador, zones 3,4,5,6,7,8,9,10,11,12,13A,13B,14A,14B,14C
## this is the file obtained from Elisabeth from NFLD
nfld.shp <- read_sf("inst/extdata/shapefiles/LobsterFishingAreas.shp")
## this shapefile was shared with me from Newfoundland, the geometry appears as a linestring
fz.nfld.sf.lines <-  st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="newfoundland",
      label=nfld.shp$id,
      geometry=nfld.shp$geometry
   )
)

## cast to multipolygon
nfld.sf <- st_transform(st_cast(st_cast(nfld.shp, "POLYGON"), "MULTIPOLYGON"), 4326)
nfld.sf <- st_transform(st_cast(st_cast(nfld.shp, "POLYGON"), "MULTIPOLYGON"), 4326)


## function to add coastline
vertices.to.multipolygon <- function(multipoly.in){
   bb <- st_bbox(st_buffer(multipoly.in,0.1))
   boundaries.temp <- st_crop(boundaries_simple, bb)
   poly.coast <- st_difference(multipoly.in, st_union(boundaries.temp$geometry))
   poly.coast$type <- "fishing zone polygon"
   return(poly.coast)
}

temp.list <- list()
for(i in 1:nrow(nfld.sf)){
   print(i)
   temp.list[[i]] <- vertices.to.multipolygon(nfld.sf[i,])
}

nfld.coast <- do.call(rbind, temp.list)
nfld.coast$species.code <- 2550
nfld.coast$region <- "newfoundland"
nfld.coast$label = nfld.coast$id

fz.nfld.sf.polygons <- nfld.coast
## we now have all the Newfondland lobster fishing areas in simple features

#

## now do Quebec and Gulf from the AFR points
##
## https://inter-l01-uat.dfo-mpo.gc.ca/infoceans/en/commercial-fisheries#carte
## I captured the points appearing in the Atlantic Fisheries Regulations in a text file, load that
lobster.afr <- read.table(file="lobster-atlantic-fishery-regulations-points.txt", header=TRUE, sep=" ", colClasses=c("numeric",rep("character", 6)))
lobster.afr$longitude <- -dms2deg(as.numeric(paste0(lobster.afr$lon.d,lobster.afr$lon.m,lobster.afr$lon.s)))
lobster.afr$latitude <- dms2deg(as.numeric(paste0(lobster.afr$lat.d,lobster.afr$lat.m,lobster.afr$lat.s)))

## each LFA is a series of points from this list
lfa.list <- list()
lfa.list[[1]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="15", points=c(1,116,45,44)) ## DR adding the point between LFA 14A and 14B, so as to remove the sliver where there is ocean that is in no LFA
lfa.list[[2]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="16", points=c(44,45,2,42,43))
lfa.list[[3]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="17A", points=c(72,73,78,74,75))
lfa.list[[4]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="17B", points=c(75,74,31,3,2,42,68,66,64,41,62,40,77,73,72))
lfa.list[[5]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18A", points=c(36,37,38,54,55))
lfa.list[[6]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18B", points=c(55,54,39,56,57))
lfa.list[[7]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18C", points=c(57,56,58,59))
lfa.list[[8]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18D", points=c(59,58,60,61))
lfa.list[[9]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18E", points=c(61,60,40,62,63))
lfa.list[[10]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18F", points=c(63,62,41,64,65))
lfa.list[[11]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18G", points=c(65,64,66,67))
lfa.list[[12]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18H", points=c(67,66,68,69))
lfa.list[[13]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18I", points=c(69,68,42,43))
lfa.list[[14]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="19A", points=c(34,35), points=c(36,37,38,54,39,56,58,60,40,77,76))
lfa.list[[15]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="19B", points=c(76,77,73,78,79))
lfa.list[[16]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="19C", points=c(79,78,74,31,32,33))
lfa.list[[17]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="20A", points=c(32,31,30,29,51))
lfa.list[[18]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="20B", points=c(51,29,28,26,27))
lfa.list[[19]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="21A", points=c(27,26,25,70,71))
lfa.list[[20]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="21B", points=c(21,22,24,70,71))
lfa.list[[21]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="22", points=c(3,12,53,52,30,31,3))
lfa.list[[22]] <- list(type="fishing zone vertices", species.code=2550, region="gulf", label="23", points=c(23,24,70,25,26,28,29,30,52,19,20))
lfa.list[[23]] <- list(type="fishing zone vertices", species.code=2550, region="gulf", label="24", points=c(18,19,52,53,12,50,13,14))
lfa.list[[24]] <- list(type="fishing zone vertices", species.code=2550, region="gulf", label="25", points=c(20,19,18), points=c(15,16,17))
lfa.list[[25]] <- list(type="fishing zone vertices", species.code=2550, region="gulf", label="26A", points=c(15,16,17), points=c(14,13,50,49,48,47,46,10,9))
lfa.list[[26]] <- list(type="fishing zone vertices", species.code=2550, region="gulf", label="26B", points=c(9,10,46,47,48,49,50,12,11))
lfa.list[[27]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="27", points=c(11,12,3,4,5,6))


create.sf.fct <- function(list.in){
   n.ls <- length(list.in)-4 ## number of linestrings for this LFA
   if(n.ls==1){
      list.in$geometry <- st_linestring(as.matrix(lobster.afr[list.in$points, c("longitude","latitude")]))
   }
   else{
      ll <- list()
      for(i in 1:n.ls){
         ll[[i]] <- st_linestring(as.matrix(lobster.afr[unlist(list.in[i+4]), c("longitude","latitude")]))
      }
      list.in$geometry <- st_multilinestring(ll)
      }
   return(list.in)
}

lfa.list.sf <- lapply(lfa.list, create.sf.fct)

sf.fct <- function(li){
   df <- data.frame(
      type=li$type,
      species.code=li$species.code,
      region=li$region,
      label=li$label,
      geometry=st_sfc(li$geometry, crs=4326)
   )
   return(df)
}

lfa.sf <- st_sf(do.call(rbind, lapply(lfa.list.sf, sf.fct)))

fz.gulf.quebec.sf.lines <- lfa.sf

fz.sf.lines <- rbind(fz.nfld.sf.lines, fz.gulf.quebec.sf.lines)


##########################################################################
## now need to create polygons with coastlines
## already done for Newfoundland
fz.sf.polygons <- fz.nfld.sf.polygons


#################
## this is good, but it won't work to add the coastline, need points on land!
## so add points on land (after point 79 in the ARC text file) and use st_difference
lfa.list <- list()
lfa.list[[1]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="15", points=c(80,1,116,45,44,81,82,80)) ## good
lfa.list[[2]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="16", points=c(81,44,45,2,42,43,83,81)) ## good
lfa.list[[3]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="17A", points=c(72,73,78,74,75,84,72)) ## good
lfa.list[[4]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="17B", points=c(75,74,31,3,2,42,68,66,64,41,62,40,77,73,72,84,75)) ## good
lfa.list[[5]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18A", points=c(36,37,38,54,55,85,36)) ## good
lfa.list[[6]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18B", points=c(55,54,39,56,57,55)) ## good
lfa.list[[7]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18C", points=c(57,56,58,59,86,57)) ## good
lfa.list[[8]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18D", points=c(59,58,60,61,87,59)) ## good
lfa.list[[9]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18E", points=c(61,60,40,62,63,88,61)) ## good
lfa.list[[10]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18F", points=c(63,62,41,64,65,89,63)) ## good
lfa.list[[11]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18G", points=c(65,64,66,67,90,65)) ## good
lfa.list[[12]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18H", points=c(67,66,68,69,91,67)) ## good
lfa.list[[13]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="18I", points=c(69,68,42,43,92,69)) ## good
lfa.list[[14]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="19A", points=c(34,35,93,36,37,38,54,39,56,58,60,40,77,76,94,95,96,34)) ## good
lfa.list[[15]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="19B", points=c(76,77,73,78,79,97,76)) ## good
lfa.list[[16]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="19C", points=c(79,78,74,31,32,33,98,79)) ## good
lfa.list[[17]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="20A", points=c(32,31,30,29,51,32)) ## good
lfa.list[[18]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="20B", points=c(51,29,28,26,27,51)) ## good
lfa.list[[19]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="21A", points=c(27,26,25,70,71,27)) ## good
lfa.list[[20]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="21B", points=c(21,22,24,70,71,99,21)) ## good
lfa.list[[21]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="22", points=c(3,12,53,52,30,31,3)) ## good
lfa.list[[22]] <- list(type="fishing zone vertices", species.code=2550, region="gulf", label="23", points=c(23,24,70,25,26,28,29,30,52,19,20,100,101,102,23)) ## good
lfa.list[[23]] <- list(type="fishing zone vertices", species.code=2550, region="gulf", label="24", points=c(18,19,52,53,12,50,13,14,103,104,105,106,107,18)) ## good
lfa.list[[24]] <- list(type="fishing zone vertices", species.code=2550, region="gulf", label="25", points=c(20,19,18,107,106,105,17,16,15,108,109,20)) ## good
lfa.list[[25]] <- list(type="fishing zone vertices", species.code=2550, region="gulf", label="26A", points=c(15,16,17,105,104,103,14,13,50,49,48,47,46,10,9,110,111,15)) ## good
lfa.list[[26]] <- list(type="fishing zone vertices", species.code=2550, region="gulf", label="26B", points=c(9,10,46,47,48,49,50,12,11,112,113,9)) ## good
lfa.list[[27]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="27", points=c(11,12,3,4,5,6,114,115,11)) ## good

lfa.list.sf <- lapply(lfa.list, create.sf.fct)
lfa.sf <- st_sf(do.call(rbind, lapply(lfa.list.sf, sf.fct)))

## now use st_difference to add coast
difference.fct <- function(li){
   temp <- st_cast(li, "POLYGON")
   bb <- st_bbox(temp)
   boundaries.temp <- st_crop(boundaries_simple, bb)
   temp.coast <- st_difference(temp, st_union(boundaries.temp$geometry))
   return(temp.coast)
}

lfa.coast.sf <- difference.fct(lfa.sf[1,])
for(i in 2:27){
   print(i)
   lfa.coast.sf <- rbind(lfa.coast.sf, difference.fct(lfa.sf[i,]))
}


vars <- c("type", "species.code", "region", "label", "geometry")
fz.sf.polygons <- rbind(lfa.coast.sf, fz.nfld.sf.polygons[,vars])

## now do the snow crab zones
## https://inter-l01-uat.dfo-mpo.gc.ca/infoceans/sites/infoceans/files/Crabe_des_Neiges_en.pdf
crab.afr <- read.table(file="snow-crab-atlantic-fishery-regulations-points.txt", header=TRUE, sep=" ", colClasses=c("numeric",rep("character", 6)))
crab.afr$longitude <- -dms2deg(as.numeric(paste0(crab.afr$lon.d,crab.afr$lon.m,crab.afr$lon.s)))
crab.afr$latitude <- dms2deg(as.numeric(paste0(crab.afr$lat.d,crab.afr$lat.m,crab.afr$lat.s)))

## each LFA is a series of points from this list
cfa.list <- list()
cfa.list[[1]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="12", points=c(40,39,38,37,16,24,35,34,32,31,30,29,43,58,57))
cfa.list[[2]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="12A", points=c(36,16,37,38,39,40))
cfa.list[[3]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="12B", points=c(15,24,23,22,20,13,14))
cfa.list[[4]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="12C", points=c(7,8,10,13,20,19,7))
cfa.list[[5]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="12E", points=c(32,33,35,34,32))
cfa.list[[6]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="12F", points=c(30,31,32,33,25,26,27,28,29,30))
cfa.list[[7]] <- list(type="fishing zone vertices", species.code=2526, region="quebec", label="13", points=c(1,2,3), points=c(4,5,6))
cfa.list[[8]] <- list(type="fishing zone vertices", species.code=2526, region="quebec", label="14", points=c(6,5,7,8,9))
cfa.list[[9]] <- list(type="fishing zone vertices", species.code=2526, region="quebec", label="15", points=c(9,8,10,11,12))
cfa.list[[10]] <- list(type="fishing zone vertices", species.code=2526, region="quebec", label="16", points=c(12,11,18), points=c(15,24,37,16,17))
cfa.list[[11]] <- list(type="fishing zone vertices", species.code=2526, region="quebec", label="16A", points=c(14,13,10,11,18))
cfa.list[[12]] <- list(type="fishing zone vertices", species.code=2526, region="quebec", label="17", points=c(59,60), points=c(17,16,36)) ## DR adding the same westmost points as LFA 19A near Quebec City
cfa.list[[13]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="18", points=c(53,52,51,58,57))
cfa.list[[14]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="19", points=c(54,43,29,56,55))
cfa.list[[15]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="25", points=c(50,51,58,43,48,49))
cfa.list[[16]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="26", points=c(41,42,48,49))


create.sf.fct <- function(list.in){
   n.ls <- length(list.in)-4 ## number of linestrings for this LFA
   if(n.ls==1){
      list.in$geometry <- st_linestring(as.matrix(crab.afr[list.in$points, c("longitude","latitude")]))
   }
   else{
      ll <- list()
      for(i in 1:n.ls){
         ll[[i]] <- st_linestring(as.matrix(crab.afr[unlist(list.in[i+4]), c("longitude","latitude")]))
      }
      list.in$geometry <- st_multilinestring(ll)
   }
   return(list.in)
}

cfa.list.sf <- lapply(cfa.list, create.sf.fct)

cfa.sf <- st_sf(do.call(rbind, lapply(cfa.list.sf, sf.fct)))

fz.sf.lines <- rbind(cfa.sf, fz.sf.lines)


##########################################################################
## now need to create polygons with coastlines, so add points on land where necessary
cfa.list <- list()
cfa.list[[1]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="12", points=c(40,39,38,37,16,24,35,34,32,31,30,29,43,58,57,89,90,91,92,93,94,95,96,97,40))
cfa.list[[2]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="12A", points=c(61,36,16,37,38,39,40,62))
cfa.list[[3]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="12B", points=c(15,24,23,22,20,13,14))
cfa.list[[4]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="12C", points=c(7,8,10,13,20,19,7)) ## completely at sea, no need to add coastline  ## good
cfa.list[[5]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="12E", points=c(32,33,35,34,32)) ## completely at sea, no need to add coastline  ## good
cfa.list[[6]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="12F", points=c(30,31,32,33,25,26,27,28,29,30)) ## completely at sea, no need to add coastline  ## good
cfa.list[[7]] <- list(type="fishing zone vertices", species.code=2526, region="quebec", label="13", points=c(6,69,63,1,2,3,64,65,66,67,68,4,5,6)) ## good
cfa.list[[8]] <- list(type="fishing zone vertices", species.code=2526, region="quebec", label="14", points=c(6,5,7,8,9,87,88,6))
cfa.list[[9]] <- list(type="fishing zone vertices", species.code=2526, region="quebec", label="15", points=c(9,8,10,11,12,85,86,9))
cfa.list[[10]] <- list(type="fishing zone vertices", species.code=2526, region="quebec", label="16", points=c(12,11,18,76,77,15,24,37,16,17,78,79,12))
cfa.list[[11]] <- list(type="fishing zone vertices", species.code=2526, region="quebec", label="16A", points=c(14,13,10,11,18))
cfa.list[[12]] <- list(type="fishing zone vertices", species.code=2526, region="quebec", label="17", points=c(70,17,16,36,71,72,73,59,60,74,75,70)) ## DR adding the same westmost points as LFA 19A near Quebec City
cfa.list[[13]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="18", points=c(53,52,51,58,57))
cfa.list[[14]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="19", points=c(54,43,29,56,55))
cfa.list[[15]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="25", points=c(50,51,58,43,48,49))
cfa.list[[16]] <- list(type="fishing zone vertices", species.code=2526, region="gulf", label="26", points=c(41,42,48,49,80,81,82,83,84,41)) ## good


## create.sf.fct(cfa.list[[2]])
cfa.list.sf <- lapply(cfa.list, create.sf.fct)
cfa.sf <- st_sf(do.call(rbind, lapply(cfa.list.sf, sf.fct)))

## now use st_difference to add coast
difference.fct <- function(li){
   temp <- st_cast(li, "POLYGON")
   bb <- st_bbox(temp)
   boundaries.temp <- st_crop(boundaries_simple, bb)
   temp.coast <- st_difference(temp, st_union(boundaries.temp$geometry))
   return(temp.coast)
}

cfa.coast.sf <- difference.fct(cfa.sf[1,])
for(i in 2:3){
   print(i)
   cfa.coast.sf <- rbind(cfa.coast.sf, difference.fct(cfa.sf[i,]))
}
cfa.coast.sf <- rbind(cfa.coast.sf, st_cast(cfa.sf[4:6,], "POLYGON"))
for(i in 7:16){
   print(i)
   cfa.coast.sf <- rbind(cfa.coast.sf, difference.fct(cfa.sf[i,]))
}


fz.sf.polygons <- rbind(cfa.coast.sf, fz.sf.polygons)



### final steps, make some maps showing the assembled lines and polygons for the different species, write shapefiles and KML files, and save as an .rda file

## maps
## lobster
lobster.lines <- fz.sf.lines[fz.sf.lines$species.code==2550,]
lobster.lines <- cbind(lobster.lines, st_coordinates(st_centroid(lobster.lines)))

lobster.polygons <- fz.sf.polygons[fz.sf.polygons$species.code==2550,]
lobster.polygons <- cbind(lobster.polygons, st_coordinates(st_centroid(lobster.polygons)))


## snow crab
snow.crab.lines <- fz.sf.lines[fz.sf.lines$species.code==2526,]
snow.crab.lines <- cbind(snow.crab.lines, st_coordinates(st_centroid(snow.crab.lines)))

snow.crab.polygons <- fz.sf.polygons[fz.sf.polygons$species.code==2526,]
snow.crab.polygons <- cbind(snow.crab.polygons, st_coordinates(st_centroid(snow.crab.polygons)))


g <- ggplot(data=boundaries_simple) +
   geom_sf(fill=grey(0.8), color=grey(0.3)) +
   xlim(-72,-48) + ylim(43,53)

g1 <- g+geom_sf(data=snow.crab.lines, color="red", fill="mistyrose")+geom_label(data=snow.crab.polygons, aes(X, Y, label=label), size=2)
ggsave(file="Gulf-of-St-Lawrence-snow-crab-areas-lines.pdf", g1, width = 30, height = 20, units = "cm")

g2 <- g+geom_sf(data=snow.crab.polygons, color="red", fill="mistyrose")+geom_label(data=snow.crab.polygons, aes(X, Y, label=label), size=2)
ggsave(file="Gulf-of-St-Lawrence-snow-crab-areas-polygons.pdf", g2, width = 30, height = 20, units = "cm")

g3 <- g+geom_sf(data=lobster.lines, color="red", fill="mistyrose")+geom_label(data=lobster.polygons, aes(X, Y, label=label), size=2)
ggsave(file="Gulf-of-St-Lawrence-lobster-areas-lines.pdf", g3, width = 30, height = 20, units = "cm")

g4 <- g+geom_sf(data=lobster.polygons, color="red", fill="mistyrose")+geom_label(data=lobster.polygons, aes(X, Y, label=label), size=2)
ggsave(file="Gulf-of-St-Lawrence-lobster-areas-polygons.pdf", g4, width = 30, height = 20, units = "cm")



## write to files
## lines
write_sf(fz.sf.lines, file.path(here(), "inst/extdata/shapefiles/fishing.zone.vertices.shp")) ## silently overwrites shapefile
write_sf(fz.sf.lines, file.path(here(), "inst/extdata/shapefiles/fishing.zone.vertices.kml")) ## google earth format
save(fz.sf.lines, file="./data/fishing.zone.vertices.rda")

## polygons
write_sf(fz.sf.polygons, file.path(here(), "inst/extdata/shapefiles/fishing.zone.polygons.shp")) ## silently overwrites shapefile
write_sf(fz.sf.polygons, file.path(here(), "inst/extdata/shapefiles/fishing.zone.polygons.kml")) ## google earth format
save(fz.sf.polygons, file="./data/fishing.zone.polygons.rda")



## still to do,
## - remove the lines on land in Nfld, for consistency with the other region
## - add herring zones
## - add groundfish zones
## DONE - generate Gulf lobster zones with coastlines from GSHHG instead of using those in the Gulf package
## DONE - add snow crab zones
