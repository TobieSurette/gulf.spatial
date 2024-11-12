## creation of the fishing.zones.polygons and fishing.zones.vertices for inclusion in gulf.spatial
##
library(gulf.spatial)
library(here)
library(sf)
library(tidyverse)

##
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

boundaries <- read_sf("build/AC/AC_1M_BoundaryPolygons_shp/AC_1M_BoundaryPolygons.shp")

boundaries_simple <- boundaries %>%
   filter(
      POL_DIV %in% c(
         "Quebec", "Newfoundland and Labrador" ,
         #"New York", "New Hampshire", "Vermont",
         "Maine",
         "New Brunswick", "Nova Scotia",
         "Prince Edward Island"
      ),
      SELECTION == "sparse" #"dense"
   ) %>%
   st_transform(4326)

####################
#### LOBSTER
#############################
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
## we now have all the Newfoundland lobster fishing areas in simple features

## Newfoundland regions from AFR points
lobster.nfld.afr <- read.table(file="./build/lobster-atlantic-fishery-regulations-points-Newfoundland.txt", header=TRUE, sep=" ", colClasses=c("numeric",rep("character", 6)))
lobster.nfld.afr$longitude <- -dms2deg(as.numeric(paste0(lobster.nfld.afr$lon.d,lobster.nfld.afr$lon.m,lobster.nfld.afr$lon.s)))
lobster.nfld.afr$latitude <- dms2deg(as.numeric(paste0(lobster.nfld.afr$lat.d,lobster.nfld.afr$lat.m,lobster.nfld.afr$lat.s)))

## each LFA is a series of points from this list
lfa.list <- list()
lfa.list[[1]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="3", points=c(1,2,3), points=c(1,2,4))
lfa.list[[2]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="4", points=c(5,6), points=c(8,7))
lfa.list[[3]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="5", points=c(8,7), points=c(9,10))
lfa.list[[4]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="6", points=c(9,10), points=c(12,11))
lfa.list[[5]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="7", points=c(12,11), points=c(13,14))
lfa.list[[6]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="8", points=c(13,14), points=c(16,15))
lfa.list[[7]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="9", points=c(16,15), points=c(17,18,19))
lfa.list[[8]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="10", points=c(17,18,19,20), points=c(22,21,20))
lfa.list[[9]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="11", points=c(22,21,36,24,23))
lfa.list[[10]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="12", points=c(23,24,25,26))
lfa.list[[11]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="13A", points=c(26,25,27,28,29))
lfa.list[[12]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="13B", points=c(29,28,31,30))
lfa.list[[13]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="14A", points=c(30,31,53,32,33))
lfa.list[[14]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="14B", points=c(33,32,35,34))
lfa.list[[15]] <- list(type="fishing zone vertices", species.code=2550, region="newfoundland", label="14C", points=c(1,2,4), points=c(35,34))

create.sf.nfld.fct <- function(list.in){
   print(list.in$label)
   n.ls <- length(list.in)-4 ## number of linestrings for this LFA
   if(n.ls==1){
      list.in$geometry <- st_linestring(as.matrix(lobster.nfld.afr[list.in$points, c("longitude","latitude")]))
   }
   else{
      ll <- list()
      for(i in 1:n.ls){
         ll[[i]] <- st_linestring(as.matrix(lobster.nfld.afr[unlist(list.in[i+4]), c("longitude","latitude")]))
      }
      list.in$geometry <- st_multilinestring(ll)
   }
   return(list.in)
}

lfa.list.sf <- lapply(lfa.list, create.sf.nfld.fct)

lfa.sf <- st_sf(do.call(rbind, lapply(lfa.list.sf, sf.fct)))

fz.nfld.sf.lines <- lfa.sf



#library(PBSmapping)
#plotMap(mar.csv[mar.csv$PID==27,])

#g <- ggplot(data=boundaries_simple) +
#   geom_sf(fill=grey(0.8), color=grey(0.3)) +
#   geom_point(data=mar.csv[mar.csv$PID==27,], aes(x=X, y=Y)) +
#   xlim(-72,-48) + ylim(43,53)


## now do Quebec and Gulf from the AFR points
##
## https://inter-l01-uat.dfo-mpo.gc.ca/infoceans/en/commercial-fisheries#carte
## I captured the points appearing in the Atlantic Fisheries Regulations in a text file, load that
lobster.afr <- read.table(file="./build/lobster-atlantic-fishery-regulations-points.txt", header=TRUE, sep=" ", colClasses=c("numeric",rep("character", 6)))
lobster.afr$longitude <- -dms2deg(as.numeric(paste0(lobster.afr$lon.d,lobster.afr$lon.m,lobster.afr$lon.s)))
lobster.afr$latitude <- dms2deg(as.numeric(paste0(lobster.afr$lat.d,lobster.afr$lat.m,lobster.afr$lat.s)))

## each LFA is a series of points from this list
lfa.list <- list()
lfa.list[[1]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="15", points=c(1,118,45,44)) ## DR adding the point between LFA 14A and 14B, so as to remove the sliver where there is ocean that is in no LFA
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
lfa.list[[27]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="27", points=c(11,12,3,4,5,6), points=c(7,8))
lfa.list[[28]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="28", points=c(7,8), points=c(119,120))


create.sf.fct <- function(list.in){
   print(list.in$label)
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

lfa.sf <- st_sf(do.call(rbind, lapply(lfa.list.sf, sf.fct)))

fz.gulf.quebec.sf.lines <- lfa.sf

fz.sf.lines <- rbind(fz.nfld.sf.lines, fz.gulf.quebec.sf.lines)


## Maritimes LFAs
## Part IV at https://laws-lois.justice.gc.ca/eng/regulations/SOR-86-21/page-25.html#h-892770

## Part V at https://laws-lois.justice.gc.ca/eng/regulations/SOR-86-21/page-25.html#h-892770
lobster.mar.afr <- read.table(file="./build/lobster-atlantic-fishery-regulations-points-Maritimes.txt", header=TRUE, sep=" ", colClasses=c("numeric",rep("character", 6)))
lobster.mar.afr$longitude <- -dms2deg(as.numeric(paste0(lobster.mar.afr$lon.d,lobster.mar.afr$lon.m,lobster.mar.afr$lon.s)))
lobster.mar.afr$latitude <- dms2deg(as.numeric(paste0(lobster.mar.afr$lat.d,lobster.mar.afr$lat.m,lobster.mar.afr$lat.s)))

## each LFA is a series of points from this list
lfa.list <- list()
lfa.list[[1]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="29", points=c(1,2), points=c(7,6,8), points=c(9,10))
lfa.list[[2]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="30", points=c(3,4,5,6,7))
lfa.list[[3]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="31A", points=c(8,6,5,11,12,13))
lfa.list[[4]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="31B", points=c(13,12,14,15))
lfa.list[[5]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="32", points=c(15,14,17,16))
lfa.list[[6]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="33", points=c(16,17,18,19,26,27,28))
lfa.list[[7]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="34", points=c(28,27,25,24,29,30,31,32,33,34,39,40))
lfa.list[[8]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="35", points=c(40,39,41,42,43))
lfa.list[[9]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="36", points=c(43,42,41,39,34,38,37,36,35))
lfa.list[[10]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="37", points=c(34,38,37,36,34))
lfa.list[[11]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="38", points=c(32,33,34,36,35))
lfa.list[[12]] <- list(type="fishing zone vertices", species.code=2550, region="maritimes", label="40", points=c(25,27,26,19,20,21,22,23,24,25))

create.sf.mar.fct <- function(list.in){
   print(list.in$label)
   n.ls <- length(list.in)-4 ## number of linestrings for this LFA
   if(n.ls==1){
      list.in$geometry <- st_linestring(as.matrix(lobster.mar.afr[list.in$points, c("longitude","latitude")]))
   }
   else{
      ll <- list()
      for(i in 1:n.ls){
         ll[[i]] <- st_linestring(as.matrix(lobster.mar.afr[unlist(list.in[i+4]), c("longitude","latitude")]))
      }
      list.in$geometry <- st_multilinestring(ll)
   }
   return(list.in)
}

lfa.list.sf <- lapply(lfa.list, create.sf.mar.fct)

lfa.sf <- st_sf(do.call(rbind, lapply(lfa.list.sf, sf.fct)))

fz.gulf.maritimes.sf.lines <- lfa.sf

fz.sf.lines <- rbind(fz.sf.lines, fz.gulf.maritimes.sf.lines)


##

##########################################################################
## now need to create polygons with coastlines
## already done for Newfoundland
fz.sf.polygons <- fz.nfld.sf.polygons


#################
## this is good, but it won't work to add the coastline, need points on land!
## so add points on land (after point 79 in the ARC text file) and use st_difference
lfa.list <- list()
lfa.list[[1]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="15", points=c(80,1,118,45,44,81,82,80)) ## good
lfa.list[[2]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="16", points=c(81,44,45,2,42,43,83,81)) ## good
lfa.list[[3]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="17A", points=c(72,73,78,74,75,84,72)) ## good
lfa.list[[4]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="17B", points=c(75,74,31,3,2,42,68,66,64,41,62,40,77,73,72,84,75)) ## good
lfa.list[[5]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="18A", points=c(36,37,38,54,55,85,36)) ## good
lfa.list[[6]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="18B", points=c(55,54,39,56,57,55)) ## good
lfa.list[[7]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="18C", points=c(57,56,58,59,86,57)) ## good
lfa.list[[8]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="18D", points=c(59,58,60,61,87,59)) ## good
lfa.list[[9]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="18E", points=c(61,60,40,62,63,88,61)) ## good
lfa.list[[10]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="18F", points=c(63,62,41,64,65,89,63)) ## good
lfa.list[[11]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="18G", points=c(65,64,66,67,90,65)) ## good
lfa.list[[12]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="18H", points=c(67,66,68,69,91,67)) ## good
lfa.list[[13]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="18I", points=c(69,68,42,43,92,69)) ## good
lfa.list[[14]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="19A", points=c(34,35,93,36,37,38,54,39,56,58,60,40,77,76,94,95,96,34)) ## good
lfa.list[[15]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="19B", points=c(76,77,73,78,79,97,76)) ## good
lfa.list[[16]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="19C", points=c(79,78,74,31,32,33,98,79)) ## good
lfa.list[[17]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="20A", points=c(32,31,30,29,51,32)) ## good
lfa.list[[18]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="20B", points=c(51,29,28,26,27,51)) ## good
lfa.list[[19]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="21A", points=c(27,26,25,70,71,27)) ## good
lfa.list[[20]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="21B", points=c(21,22,24,70,71,99,21)) ## good
lfa.list[[21]] <- list(type="fishing zone polygon", species.code=2550, region="quebec", label="22", points=c(3,12,53,52,30,31,3)) ## good
lfa.list[[22]] <- list(type="fishing zone polygon", species.code=2550, region="gulf", label="23", points=c(23,24,70,25,26,28,29,30,52,19,20,100,101,102,23)) ## good
lfa.list[[23]] <- list(type="fishing zone polygon", species.code=2550, region="gulf", label="24", points=c(18,19,52,53,12,50,13,14,103,104,105,106,107,18)) ## good
lfa.list[[24]] <- list(type="fishing zone polygon", species.code=2550, region="gulf", label="25", points=c(20,19,18,107,106,105,17,16,15,108,109,20)) ## good
lfa.list[[25]] <- list(type="fishing zone polygon", species.code=2550, region="gulf", label="26A", points=c(15,16,17,105,104,103,14,13,50,49,48,47,46,10,9,110,111,15)) ## good
lfa.list[[26]] <- list(type="fishing zone polygon", species.code=2550, region="gulf", label="26B", points=c(9,10,46,47,48,49,50,12,11,112,113,9)) ## good
lfa.list[[27]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="27", points=c(11,12,3,4,5,6,114,7,8,115,116,117,11)) ## good
lfa.list[[28]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="28", points=c(8,115,122,121,119,120,114,7,8))

lfa.list.sf <- lapply(lfa.list, create.sf.fct)
lfa.sf <- st_sf(do.call(rbind, lapply(lfa.list.sf, sf.fct)))


## now use st_difference to add coast
difference.fct <- function(li){
   temp <- st_cast(li, "POLYGON")
   bb <- st_bbox(temp)
   ## to remove warning "attribute variables are assumed to be spatially constant throughout all geometries"
   ## convert to a projected CRS
   ## st_transform(boundaries_simple, crs="UTM")

   boundaries.temp <- st_crop(boundaries_simple, bb)
   ## st_difference returns an error
   temp.coast <- st_difference(temp, st_union(boundaries.temp$geometry))
   return(temp.coast)
}

lfa.coast.sf <- difference.fct(lfa.sf[1,])
for(i in 2:28){
   print(i)
   lfa.coast.sf <- rbind(lfa.coast.sf, difference.fct(lfa.sf[i,]))
}



## do the same for Maritimes LFAs
## NOTE: for the function difference.fct to work, the vertices below can't be MULTILINESTRING, it must be a LINESTRING
lfa.list <- list()
lfa.list[[1]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="29", points=c(1,2,7,6,8,9,10,1))
lfa.list[[2]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="30", points=c(3,4,5,6,7,44,3))
lfa.list[[3]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="31A", points=c(8,6,5,11,12,13,45,46,8))
lfa.list[[4]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="31B", points=c(13,12,14,15,47,45,13))
lfa.list[[5]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="32", points=c(15,14,17,16,48,45,15))
lfa.list[[6]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="33", points=c(48,16,17,18,19,26,27,28,49,50,48))
lfa.list[[7]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="34", points=c(49,28,27,25,24,29,30,31,32,33,34,39,40,51,49))
lfa.list[[8]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="35", points=c(51,40,39,41,42,43,52,53,48,51))
lfa.list[[9]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="36", points=c(52,43,42,41,39,34,38,37,35,54,52))
lfa.list[[10]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="37", points=c(34,38,37,36,34))
lfa.list[[11]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="38", points=c(32,33,34,36,35))
lfa.list[[12]] <- list(type="fishing zone polygon", species.code=2550, region="maritimes", label="40", points=c(25,27,26,19,20,21,22,23,24,25))

lfa.list.sf <- lapply(lfa.list, create.sf.mar.fct)
lfa.sf <- st_sf(do.call(rbind, lapply(lfa.list.sf, sf.fct)))


lfa.coast.mar.sf <- difference.fct(lfa.sf[1,])
for(i in 2:9){
   print(i)
   lfa.coast.mar.sf <- rbind(lfa.coast.mar.sf, difference.fct(lfa.sf[i,]))
}

lfa.coast.mar.sf <- rbind(
   lfa.coast.mar.sf,
   st_sf(cbind(
      as.data.frame(lfa.sf[10,])[,c("type","species.code","region","label")],
      data.frame(geometry=st_cast(lfa.sf[10,"geometry"],"POLYGON")$geometry)
      ))
   ) ## no need to clip coastline, it is the zone near Grand Manan without any land

lfa.coast.mar.sf <- rbind(lfa.coast.mar.sf, difference.fct(lfa.sf[11,]))

lfa.coast.mar.sf <- rbind(
   lfa.coast.mar.sf,
   st_sf(cbind(
      as.data.frame(lfa.sf[12,])[,c("type","species.code","region","label")],
      data.frame(geometry=st_cast(lfa.sf[12,"geometry"],"POLYGON")$geometry)
   ))
) ## no need to clip coastline, it is the offshore zone without any land




vars <- c("type", "species.code", "region", "label", "geometry")
fz.sf.polygons <- rbind(lfa.coast.sf, lfa.coast.mar.sf, fz.nfld.sf.polygons[,vars])


####################
#### SNOW CRAB start
#############################
## now do the snow crab zones
## https://inter-l01-uat.dfo-mpo.gc.ca/infoceans/sites/infoceans/files/Crabe_des_Neiges_en.pdf
crab.afr <- read.table(file="build/snow-crab-atlantic-fishery-regulations-points.txt", header=TRUE, sep=" ", colClasses=c("numeric",rep("character", 6)))
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
cfa.list[[1]] <- list(type="fishing zone polygon", species.code=2526, region="gulf", label="12", points=c(40,39,38,37,16,24,35,34,32,31,30,29,43,58,57,89,90,91,92,93,94,95,96,97,40))
cfa.list[[2]] <- list(type="fishing zone polygon", species.code=2526, region="gulf", label="12A", points=c(61,36,16,37,38,39,40,62))
cfa.list[[3]] <- list(type="fishing zone polygon", species.code=2526, region="gulf", label="12B", points=c(15,24,23,22,20,13,14))
cfa.list[[4]] <- list(type="fishing zone polygon", species.code=2526, region="gulf", label="12C", points=c(7,8,10,13,20,19,7)) ## completely at sea, no need to add coastline  ## good
cfa.list[[5]] <- list(type="fishing zone polygon", species.code=2526, region="gulf", label="12E", points=c(32,33,35,34,32)) ## completely at sea, no need to add coastline  ## good
cfa.list[[6]] <- list(type="fishing zone polygon", species.code=2526, region="gulf", label="12F", points=c(30,31,32,33,25,26,27,28,29,30)) ## completely at sea, no need to add coastline  ## good
cfa.list[[7]] <- list(type="fishing zone polygon", species.code=2526, region="quebec", label="13", points=c(6,69,63,1,2,3,64,65,66,67,68,4,5,6)) ## good
cfa.list[[8]] <- list(type="fishing zone polygon", species.code=2526, region="quebec", label="14", points=c(6,5,7,8,9,87,88,6))
cfa.list[[9]] <- list(type="fishing zone polygon", species.code=2526, region="quebec", label="15", points=c(9,8,10,11,12,85,86,9))
cfa.list[[10]] <- list(type="fishing zone polygon", species.code=2526, region="quebec", label="16", points=c(12,11,18,76,77,15,24,37,16,17,78,79,12))
cfa.list[[11]] <- list(type="fishing zone polygon", species.code=2526, region="quebec", label="16A", points=c(14,13,10,11,18))
cfa.list[[12]] <- list(type="fishing zone polygon", species.code=2526, region="quebec", label="17", points=c(70,17,16,36,71,72,73,59,60,74,75,70)) ## DR adding the same westmost points as LFA 19A near Quebec City
cfa.list[[13]] <- list(type="fishing zone polygon", species.code=2526, region="gulf", label="18", points=c(53,52,51,58,57))
cfa.list[[14]] <- list(type="fishing zone polygon", species.code=2526, region="gulf", label="19", points=c(54,43,29,56,55))
cfa.list[[15]] <- list(type="fishing zone polygon", species.code=2526, region="gulf", label="25", points=c(50,51,58,43,48,49))
cfa.list[[16]] <- list(type="fishing zone polygon", species.code=2526, region="gulf", label="26", points=c(41,42,48,49,80,81,82,83,84,41)) ## good


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
#############################
#### SNOW CRAB end
####################


####################
#### HERRING start
#############################
## now do the herring zones
## https://inter-l01-uat.dfo-mpo.gc.ca/infoceans/sites/infoceans/files/Hareng_en.pdf
herring.afr <- read.table(file="build/herring-atlantic-fishery-regulations-points.txt", header=TRUE, sep=" ", colClasses=c("numeric",rep("character", 6)))
herring.afr$longitude <- -dms2deg(as.numeric(paste0(herring.afr$lon.d,herring.afr$lon.m,herring.afr$lon.s)))
herring.afr$latitude <- dms2deg(as.numeric(paste0(herring.afr$lat.d,herring.afr$lat.m,herring.afr$lat.s)))


## each area is a series of points from this list
hfa.list <- list()
hfa.list[[1]] <- list(type="fishing zone vertices", species.code=60, region="newfoundland", label="11", points=c(25,26,28,36,37,38))
hfa.list[[2]] <- list(type="fishing zone vertices", species.code=60, region="newfoundland", label="12", points=c(38,37,41,40,39))
hfa.list[[3]] <- list(type="fishing zone vertices", species.code=60, region="newfoundland", label="13", points=c(39,40,44,47,48))
hfa.list[[4]] <- list(type="fishing zone vertices", species.code=60, region="newfoundland", label="14", points=c(6,7,8), points=c(48,47,49))
hfa.list[[5]] <- list(type="fishing zone vertices", species.code=60, region="quebec", label="15", points=c(49,47,44,51,45,46))
hfa.list[[6]] <- list(type="fishing zone vertices", species.code=60, region="gulf", label="16A", points=c(46,45,51,50))
hfa.list[[7]] <- list(type="fishing zone vertices", species.code=60, region="gulf", label="16B", points=c(50,51,54,55,56))
hfa.list[[8]] <- list(type="fishing zone vertices", species.code=60, region="gulf", label="16C", points=c(56,55,57))
hfa.list[[9]] <- list(type="fishing zone vertices", species.code=60, region="gulf", label="16D", points=c(52,54,51,44,43))
hfa.list[[10]] <- list(type="fishing zone vertices", species.code=60, region="gulf", label="16E", points=c(57,55,54,53), points=c(58,59,60))
hfa.list[[11]] <- list(type="fishing zone vertices", species.code=60, region="gulf", label="16F", points=c(58,59,60), points=c(61,62,63), points=c(32,33))
hfa.list[[12]] <- list(type="fishing zone vertices", species.code=60, region="gulf", label="16G", points=c(53,54,52), points=c(61,62,63))
hfa.list[[13]] <- list(type="fishing zone vertices", species.code=60, region="gulf", label="17", points=c(43,44,40,41,42))
hfa.list[[14]] <- list(type="fishing zone vertices", species.code=60, region="gulf", label="18", points=c(42,41,37,36,35,34))
hfa.list[[15]] <- list(type="fishing zone vertices", species.code=60, region="gulf", label="19", points=c(34,35,36,28,29), points=c(32,33))




create.sf.fct <- function(list.in){
#   print(list.in$label)
   n.ls <- length(list.in)-4 ## number of linestrings for this LFA
   if(n.ls==1){
      list.in$geometry <- st_linestring(as.matrix(herring.afr[list.in$points, c("longitude","latitude")]))
   }
   else{
      ll <- list()
      for(i in 1:n.ls){
         ll[[i]] <- st_linestring(as.matrix(herring.afr[unlist(list.in[i+4]), c("longitude","latitude")]))
      }
      list.in$geometry <- st_multilinestring(ll)
   }
   return(list.in)
}

hfa.list.sf <- lapply(hfa.list, create.sf.fct)

hfa.sf <- st_sf(do.call(rbind, lapply(hfa.list.sf, sf.fct)))

fz.sf.lines <- rbind(hfa.sf, fz.sf.lines)

##########################################################################
## now need to create polygons with coastlines, so add points on land where necessary
hfa.list <- list()
hfa.list[[1]] <- list(type="fishing zone polygon", species.code=60, region="newfoundland", label="11", points=c(25,26,28,36,37,38,64,65,66,67))
hfa.list[[2]] <- list(type="fishing zone polygon", species.code=60, region="newfoundland", label="12", points=c(38,37,41,40,39,68))
hfa.list[[3]] <- list(type="fishing zone polygon", species.code=60, region="newfoundland", label="13", points=c(39,40,44,47,48,69,70,71))
hfa.list[[4]] <- list(type="fishing zone polygon", species.code=60, region="newfoundland", label="14", points=c(6,7,8,72,73,74,75,48,47,49,76))
hfa.list[[5]] <- list(type="fishing zone polygon", species.code=60, region="quebec", label="15", points=c(49,47,44,51,45,46,77,78))
hfa.list[[6]] <- list(type="fishing zone polygon", species.code=60, region="gulf", label="16A", points=c(79,46,45,51,50,80,81,82,85,86,83,84))
hfa.list[[7]] <- list(type="fishing zone polygon", species.code=60, region="gulf", label="16B", points=c(50,51,54,55,56,87,88,89))
hfa.list[[8]] <- list(type="fishing zone polygon", species.code=60, region="gulf", label="16C", points=c(56,55,57,90,91))
hfa.list[[9]] <- list(type="fishing zone polygon", species.code=60, region="gulf", label="16D", points=c(52,54,51,44,43,92))
hfa.list[[10]] <- list(type="fishing zone polygon", species.code=60, region="gulf", label="16E", points=c(57,55,54,53,58,59,60,93,94))
hfa.list[[11]] <- list(type="fishing zone polygon", species.code=60, region="gulf", label="16F", points=c(58,59,60,95,96,32,33,97,63,62,61,98,99,100))
hfa.list[[12]] <- list(type="fishing zone polygon", species.code=60, region="gulf", label="16G", points=c(53,54,52,92,63,62,61,98,99,100))
hfa.list[[13]] <- list(type="fishing zone polygon", species.code=60, region="gulf", label="17", points=c(43,44,40,41,42,101,102,103,104,105))
hfa.list[[14]] <- list(type="fishing zone polygon", species.code=60, region="gulf", label="18", points=c(42,41,37,36,35,34,106,107,108))
hfa.list[[15]] <- list(type="fishing zone polygon", species.code=60, region="gulf", label="19", points=c(34,35,36,28,29,109,110,32,33,111,112,113,114))

hfa.list.sf <- lapply(hfa.list, create.sf.fct)
hfa.sf <- st_sf(do.call(rbind, lapply(hfa.list.sf, sf.fct)))

## now use st_difference to add coast
difference.fct <- function(li){
   temp <- st_cast(li, "POLYGON")
   bb <- st_bbox(temp)
   boundaries.temp <- st_crop(boundaries_simple, bb)
   temp.coast <- st_difference(temp, st_union(boundaries.temp$geometry))
   return(temp.coast)
}

hfa.coast.sf <- difference.fct(hfa.sf[1,])
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[2,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[3,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[4,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[5,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[6,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[7,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[8,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[9,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[10,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[11,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[12,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[13,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[14,]))
hfa.coast.sf <- rbind(hfa.coast.sf, difference.fct(hfa.sf[15,]))



fz.sf.polygons <- rbind(hfa.coast.sf, fz.sf.polygons)
#### HERRING end
####################
################

################
####################
#### GROUNDFISH start
# https://inter-l01-uat.dfo-mpo.gc.ca/infoceans/sites/infoceans/files/PoissonFond_en.pdf
groundfish.afr <- read.table(file="build/groundfish-atlantic-fishery-regulations-points.txt", header=TRUE, sep=" ", colClasses=c("numeric",rep("character", 6)))
groundfish.afr$longitude <- -dms2deg(as.numeric(paste0(groundfish.afr$lon.d,groundfish.afr$lon.m,groundfish.afr$lon.s)))
groundfish.afr$latitude <- dms2deg(as.numeric(paste0(groundfish.afr$lat.d,groundfish.afr$lat.m,groundfish.afr$lat.s)))

## each area is a series of points from this list
gfa.list <- list()
gfa.list[[1]] <- list(type="fishing zone vertices", species.codes=253, region="gulf", label="4T1", points=c(45,46,48,11,42))
gfa.list[[2]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T2b", points=c(14,47,48,11))
gfa.list[[3]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T2a1", points=c(54,12,49,46,48,56,55))
gfa.list[[4]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T2a2", points=c(55,56,57))
gfa.list[[5]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T2a3", points=c(57,56,48,47,19,58))
gfa.list[[6]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T2a4", points=c(58,19,15,59))
gfa.list[[7]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T2a5", points=c(59,15,5,12,54))
gfa.list[[8]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T3a", points=c(17,16,18,2,15,52))
gfa.list[[9]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T3b", points=c(53,52,15,19,20))
gfa.list[[10]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T4", points=c(3,16,17))
gfa.list[[11]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T5", points=c(20,19,47,14,41))
gfa.list[[12]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T6", points=c(44,43))
gfa.list[[13]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T7", points=c(41,14), points=c(9,10,51))
gfa.list[[14]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T8", points=c(9,10,51), points=c(11,42))
gfa.list[[15]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T9a", points=c(12,6,50,49,12))
gfa.list[[16]] <- list(type="fishing zone vertices", species.code=253, region="gulf", label="4T9b", points=c(45,46,49,50,13))
gfa.list[[17]] <- list(type="fishing zone vertices", species.code=253, region="maritimes", label="4VN1", points=c(13,50,6,32,35,36,40))
gfa.list[[18]] <- list(type="fishing zone vertices", species.code=253, region="maritimes", label="4VN2", points=c(40,36,37,38,39))
gfa.list[[19]] <- list(type="fishing zone vertices", species.code=253, region="newfoundland", label="4Ra", points=c(23,24,25), points=c(26,27,8))
gfa.list[[20]] <- list(type="fishing zone vertices", species.code=253, region="newfoundland", label="4Rb", points=c(26,27,28,29))
gfa.list[[21]] <- list(type="fishing zone vertices", species.code=253, region="newfoundland", label="4Rc", points=c(29,28,7,31,30))
gfa.list[[22]] <- list(type="fishing zone vertices", species.code=253, region="newfoundland", label="4Rd", points=c(30,31,6,32,33))
gfa.list[[23]] <- list(type="fishing zone vertices", species.code=253, region="quebec", label="4S1", points=c(3,16,18,2,22,1))
gfa.list[[24]] <- list(type="fishing zone vertices", species.code=253, region="quebec", label="4S2", points=c(1,22), points=c(4,21))
gfa.list[[25]] <- list(type="fishing zone vertices", species.code=253, region="quebec", label="4S3", points=c(21,7,31,6,12,5,21))
gfa.list[[26]] <- list(type="fishing zone vertices", species.code=253, region="quebec", label="4S4", points=c(22,2,15,5,21))
gfa.list[[27]] <- list(type="fishing zone vertices", species.code=253, region="quebec", label="4S5", points=c(4,21,7,28,27,8))

create.sf.fct <- function(list.in){
   #   print(list.in$label)
   n.ls <- length(list.in)-4 ## number of linestrings for this LFA
   if(n.ls==1){
      list.in$geometry <- st_linestring(as.matrix(groundfish.afr[list.in$points, c("longitude","latitude")]))
   }
   else{
      ll <- list()
      for(i in 1:n.ls){
         ll[[i]] <- st_linestring(as.matrix(groundfish.afr[unlist(list.in[i+4]), c("longitude","latitude")]))
      }
      list.in$geometry <- st_multilinestring(ll)
   }
   return(list.in)
}

gfa.list.sf <- lapply(gfa.list, create.sf.fct)

gfa.sf <- st_sf(do.call(rbind, lapply(gfa.list.sf, sf.fct)))

fz.sf.lines <- rbind(gfa.sf, fz.sf.lines)

##########################################################################
## now need to create polygons with coastlines, so add points on land where necessary
gfa.list <- list()
gfa.list[[1]] <- list(type="fishing zone polygon", species.codes=253, region="gulf", label="4T1", points=c(45,46,48,11,42,60,61,45))
gfa.list[[2]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T2b", points=c(14,47,48,11,62,63,64,65,66,14))
gfa.list[[3]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T2a1", points=c(54,12,49,46,48,56,55,75,74,73,72,76,77,78,79,80,81,82,54))
gfa.list[[4]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T2a2", points=c(55,56,57,67,68,69,70,71,72,73,74,75,55))
gfa.list[[5]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T2a3", points=c(57,56,48,47,19,58,69,68,67,57))
gfa.list[[6]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T2a4", points=c(58,19,15,59,83,84,85,86,58))
gfa.list[[7]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T2a5", points=c(59,15,5,12,54,82,81,80))
gfa.list[[8]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T3a", points=c(17,16,18,2,15,52,53,87,88,43,44,17))
gfa.list[[9]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T3b", points=c(53,52,15,19,20,88,87,53))
gfa.list[[10]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T4", points=c(3,16,17,89,90,91,92,93,94,3))
gfa.list[[11]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T5", points=c(20,19,47,14,41,95,96,20))
gfa.list[[12]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T6", points=c(44,43,97,98,99,44))
gfa.list[[13]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T7", points=c(41,14,100,101,102,103,51,10,9,104,105,106,41))
gfa.list[[14]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T8", points=c(9,10,51,107,108,11,42,109,110,111,112,113,9))
gfa.list[[15]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T9a", points=c(12,6,50,49,12))
gfa.list[[16]] <- list(type="fishing zone polygon", species.code=253, region="gulf", label="4T9b", points=c(45,46,49,50,13,114,45))
gfa.list[[17]] <- list(type="fishing zone polygon", species.code=253, region="maritimes", label="4VN1", points=c(13,50,6,32,35,36,40,115,116,13))
gfa.list[[18]] <- list(type="fishing zone polygon", species.code=253, region="maritimes", label="4VN2", points=c(40,36,37,38,39,117,118,119,120,121,122,123,124,125,126,40))
gfa.list[[19]] <- list(type="fishing zone polygon", species.code=253, region="newfoundland", label="4Ra", points=c(26,27,8,127,23,24,25,128,129,130,131,132,133,26))
gfa.list[[20]] <- list(type="fishing zone polygon", species.code=253, region="newfoundland", label="4Rb", points=c(29,28,27,26,133,134,29))
gfa.list[[21]] <- list(type="fishing zone polygon", species.code=253, region="newfoundland", label="4Rc", points=c(29,28,7,31,30,135,136,137,138,134,29))
gfa.list[[22]] <- list(type="fishing zone polygon", species.code=253, region="newfoundland", label="4Rd", points=c(30,31,6,32,33,139,137,136,135,30))
gfa.list[[23]] <- list(type="fishing zone polygon", species.code=253, region="quebec", label="4S1", points=c(3,16,18,2,22,1,140,141,3))
gfa.list[[24]] <- list(type="fishing zone polygon", species.code=253, region="quebec", label="4S2", points=c(1,22,142,21,4,143,1))
gfa.list[[25]] <- list(type="fishing zone polygon", species.code=253, region="quebec", label="4S3", points=c(21,7,31,6,12,5,21))
gfa.list[[26]] <- list(type="fishing zone polygon", species.code=253, region="quebec", label="4S4", points=c(22,2,15,5,21,142,22))
gfa.list[[27]] <- list(type="fishing zone polygon", species.code=253, region="quebec", label="4S5", points=c(4,21,7,28,27,8,144,145,4))

gfa.list.sf <- lapply(gfa.list, create.sf.fct)
gfa.sf <- st_sf(do.call(rbind, lapply(gfa.list.sf, sf.fct)))

## now use st_difference to add coast
difference.fct <- function(li){
   temp <- st_cast(li, "POLYGON")
   bb <- st_bbox(temp)
   boundaries.temp <- st_crop(boundaries_simple, bb)
   temp.coast <- st_difference(temp, st_union(boundaries.temp$geometry))
   return(temp.coast)
}

gfa.coast.sf <- difference.fct(gfa.sf[1,])
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[2,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[3,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[4,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[5,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[6,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[7,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[8,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[9,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[10,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[11,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[12,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[13,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[14,]))
gfa.coast.sf <- rbind(gfa.coast.sf, st_cast(gfa.sf[15,], "POLYGON"))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[16,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[17,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[18,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[19,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[20,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[21,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[22,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[23,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[24,]))
gfa.coast.sf <- rbind(gfa.coast.sf, st_cast(gfa.sf[25,], "POLYGON"))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[26,]))
gfa.coast.sf <- rbind(gfa.coast.sf, difference.fct(gfa.sf[27,]))


fz.sf.polygons <- rbind(gfa.coast.sf, fz.sf.polygons)


##########################################################################
#### GROUNDFISH end
####################
################

##########################################################################
####################
#### SHRIMP start
#############################
## https://inter-l01-uat.dfo-mpo.gc.ca/infoceans/en/commercial-fisheries#carte
## https://inter-l01-uat.dfo-mpo.gc.ca/infoceans/sites/infoceans/files/Crevette_en.pdf

##########################################################################
#### SHRIMP end
####################
################


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

## herring
herring.lines <- fz.sf.lines[fz.sf.lines$species.code==60,]
herring.lines <- cbind(herring.lines, st_coordinates(st_centroid(herring.lines)))

herring.polygons <- fz.sf.polygons[fz.sf.polygons$species.code==60,]
herring.polygons <- cbind(herring.polygons, st_coordinates(st_centroid(herring.polygons)))

## groundfish
groundfish.lines <- fz.sf.lines[fz.sf.lines$species.code==253,]
groundfish.lines <- cbind(groundfish.lines, st_coordinates(st_centroid(groundfish.lines)))

groundfish.polygons <- fz.sf.polygons[fz.sf.polygons$species.code==253,] # gfa.coast.sf
groundfish.polygons <- cbind(groundfish.polygons, st_coordinates(st_centroid(groundfish.polygons)))


g <- ggplot(data=boundaries_simple) +
   geom_sf(fill=grey(0.8), color=grey(0.3)) +
   xlim(-72,-48) + ylim(42,53)

g1 <- g+geom_sf(data=snow.crab.lines, color="red", fill="mistyrose")+geom_label(data=snow.crab.polygons, aes(X, Y, label=label), size=2)
ggsave(file="build/Gulf-of-St-Lawrence-snow-crab-areas-lines.pdf", g1, width = 30, height = 20, units = "cm")

g2 <- g+geom_sf(data=snow.crab.polygons, color="red", fill="mistyrose")+geom_label(data=snow.crab.polygons, aes(X, Y, label=label), size=2)
ggsave(file="build/Gulf-of-St-Lawrence-snow-crab-areas-polygons.pdf", g2, width = 30, height = 20, units = "cm")

g3 <- g+geom_sf(data=lobster.lines, color="red", fill="mistyrose")+geom_label(data=lobster.lines, aes(X, Y, label=label), size=2)
ggsave(file="build/Gulf-of-St-Lawrence-lobster-areas-lines.pdf", g3, width = 30, height = 20, units = "cm")

g4 <- g+geom_sf(data=lobster.polygons, color="red", fill="mistyrose")+geom_label(data=lobster.polygons, aes(X, Y, label=label), size=2)
ggsave(file="build/Gulf-of-St-Lawrence-lobster-areas-polygons.pdf", g4, width = 30, height = 20, units = "cm")

g5 <- g+geom_sf(data=herring.lines, color="red", fill="mistyrose")+geom_label(data=herring.lines, aes(X, Y, label=label), size=2)
ggsave(file="build/Gulf-of-St-Lawrence-herring-areas-lines.pdf", g5, width = 30, height = 20, units = "cm")

g6 <- g+geom_sf(data=herring.polygons, color="red", fill="mistyrose")+geom_label(data=herring.polygons, aes(X, Y, label=label), size=2)
ggsave(file="build/Gulf-of-St-Lawrence-herring-areas-polygons.pdf", g6, width = 30, height = 20, units = "cm")

g7 <- g+geom_sf(data=groundfish.lines, color="red", fill="mistyrose")+geom_label(data=groundfish.lines, aes(X, Y, label=label), size=2)
ggsave(file="build/Gulf-of-St-Lawrence-groundfish-areas-lines.pdf", g7, width = 30, height = 20, units = "cm")

g8 <- g+geom_sf(data=groundfish.polygons, color="red", fill="mistyrose")+geom_label(data=groundfish.polygons, aes(X, Y, label=label), size=2)
ggsave(file="build/Gulf-of-St-Lawrence-groundfish-areas-polygons.pdf", g8, width = 30, height = 20, units = "cm")

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
## - remove the lines on land in Nfld, for consistency with the other regions
## - for Maritimes LFAs, add Canada-US border in the polygons in LFAs 36 and 38
## - add Maritimes offshore LFA 41
## - add Newfoundland snow crab zones
## - add Maritimes snow crab zones
## - add 4V and 4RS groundfish zones
## DONE - add 4T groundfish zones
## DONE - add Maritimes LFAs
## DONE - generate Gulf lobster zones with coastlines from GSHHG instead of using those in the Gulf package
## DONE - add snow crab zones
## DONE - clean up LFAs around Bras d'Or lake, LFAs 28 and 29
## DONE - add herring zones
