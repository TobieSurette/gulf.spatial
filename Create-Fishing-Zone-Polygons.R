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

## g <- ggplot(data=boundaries_simple) + geom_sf()


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

# g <- ggplot(data=boundaries_simple) +
#    geom_sf(fill=grey(0.8), color=grey(0.3)) +
#    geom_sf(data=nfld.sf, color="red", fill="mistyrose")


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

# g <- ggplot(data=boundaries_simple) +
#    geom_sf(fill=grey(0.8), color=grey(0.3)) +
#    geom_sf(data=nfld.coast, color="red", fill="mistyrose")
#

## now do Quebec and Gulf from the AFR points
##
## https://inter-l01-uat.dfo-mpo.gc.ca/infoceans/en/commercial-fisheries#carte
## I captured the points appearing in the Atlantic Fisheries Regulations in a text file, load that
lobster.afr <- read.table(file="lobster-atlantic-fishery-regulations-points.txt", header=TRUE, sep=" ", colClasses=c("integer",rep("character", 6)))
lobster.afr$longitude <- -dms2deg(as.numeric(paste0(lobster.afr$lon.d,lobster.afr$lon.m,lobster.afr$lon.s)))
lobster.afr$latitude <- dms2deg(as.numeric(paste0(lobster.afr$lat.d,lobster.afr$lat.m,lobster.afr$lat.s)))

## each LFA is a series of points from this list, and some points on land
lfa.list <- list()
lfa.list[[1]] <- list(type="fishing zone vertices", species.code=2550, region="quebec", label="15", points=c(1,45,44))
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

# g <- ggplot(data=boundaries_simple) +
#    geom_sf(fill=grey(0.8), color=grey(0.3)) +
#    geom_sf(data=fz.sf.lines, color="red", fill="mistyrose")

lobster <- fz.sf.lines[fz.sf.lines$species.code==2550,]
lobster <- cbind(lobster, st_coordinates(st_centroid(lobster)))

g <- ggplot(data=boundaries_simple) +
   geom_sf(fill=grey(0.8), color=grey(0.3)) +
   geom_label(data=lobster, aes(X, Y, label=label))
ggsave(file="Gulf-of-St-Lawrence-lobster-areas-lines.pdf", g, width = 30, height = 20, units = "cm")


## the only issue here is that the Newfoundland lines do not end at the coast but rather at some point inland
## but still usable, so write the corresponding OGC files
write_sf(fz.sf.lines, file.path(here(), "inst/extdata/shapefiles/fishing.zone.vertices.shp")) ## silently overwrites shapefile
write_sf(fz.sf.lines, file.path(here(), "inst/extdata/shapefiles/fishing.zone.vertices.kml")) ## google earth format
write_sf(fz.sf.lines, file.path(here(), "inst/extdata/shapefiles/fishing.zone.vertices.GeoJSON")) ## google earth format

save(fz.sf.lines, file="./data/fishing.zone.vertices.rda")


##########################################################################
## now need to create polygons with coastlines
## already done for Newfoundland
fz.sf.polygons <- fz.nfld.sf.polygons

# g <- ggplot(data=boundaries_simple) +
#    geom_sf(fill=grey(0.8), color=grey(0.3)) +
#    geom_sf(data=fz.sf.lines, color="red", fill="mistyrose")




## trying to add coastlines
## the following doesn't work since the polygon defined from the points does not touch the land:
temp <- st_cast(lfa.sf[1,], "POLYGON")
bb <- st_bbox(temp)
boundaries.temp <- st_crop(boundaries_simple, bb)
temp.coast <- st_difference(temp, st_union(boundaries.temp$geometry))
temp.coast$type <- "fishing zone polygon"
plot(boundaries.temp$geometry)
plot(temp.coast$geometry, add=TRUE)

#################
## this is good, but it won't work to add the coastline, need points on land!
## so add points on land and use st_difference





#################
## Gulf, start with what is already in the gulf package

load(file.path(here(), "../gulf/data/fishing.zone.polygons.rda"))

p <- fishing.zone.polygons
#p <- gulf::subset.list(p, species = 2550, region = "gulf")

## first fix the fishing zone polygons to make sure that they all have the same format
## snow crab polygons
crab.p <- list(
   p[[1]],p[[2]],p[[3]],p[[4]] ## ,p[[5]], buffer zone excluded
)

## lobster polygons
lobster.p <- list(
   p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],p[[11]],p[[12]],p[[13]]
)
lobster.p[[1]]$type="fishing.zone"
lobster.p[[2]]$type="fishing.zone"
lobster.p[[3]]$type="fishing.zone"
lobster.p[[4]]$type="fishing.zone"
lobster.p[[5]]$type="fishing.zone"
lobster.p[[6]]$type="fishing.zone"
lobster.p[[7]]$type="fishing.zone"
lobster.p[[8]]$type="fishing.zone"

## the first 4 lists must be formatted so that the columns x, y and hole are not within a list

lobster.p2 <-
   list(
      lobster.p[[1]], lobster.p[[2]], lobster.p[[3]], lobster.p[[4]],
      list(list(x=lobster.p[[5]]$x, y=lobster.p[[5]]$y, lobster.p[[5]]$hole), type=lobster.p[[5]]$type, species=lobster.p[[5]]$species, region=lobster.p[[5]]$region, label=lobster.p[[5]]$label, area=lobster.p[[5]]$area),
      list(list(x=lobster.p[[6]]$x, y=lobster.p[[6]]$y, lobster.p[[6]]$hole), type=lobster.p[[6]]$type, species=lobster.p[[6]]$species, region=lobster.p[[6]]$region, label=lobster.p[[6]]$label, area=lobster.p[[6]]$area),
      list(list(x=lobster.p[[7]]$x, y=lobster.p[[7]]$y, lobster.p[[7]]$hole), type=lobster.p[[7]]$type, species=lobster.p[[7]]$species, region=lobster.p[[7]]$region, label=lobster.p[[7]]$label, area=lobster.p[[7]]$area),
      list(list(x=lobster.p[[8]]$x, y=lobster.p[[8]]$y, lobster.p[[8]]$hole), type=lobster.p[[8]]$type, species=lobster.p[[8]]$species, region=lobster.p[[8]]$region, label=lobster.p[[8]]$label, area=lobster.p[[8]]$area)
   )

# herring polygons
herring.p <- list(
   p[[14]],p[[15]],p[[16]],p[[17]],p[[18]],p[[19]],p[[20]]
)
herring.p2 <-
   list(
      list(list(x=herring.p[[1]]$x, y=herring.p[[1]]$y, herring.p[[1]]$hole), type="fishing.zone", species=herring.p[[1]]$species, region=herring.p[[1]]$region, label=herring.p[[1]]$label, area=herring.p[[1]]$area),
      list(list(x=herring.p[[2]]$x, y=herring.p[[2]]$y, herring.p[[2]]$hole), type="fishing.zone", species=herring.p[[2]]$species, region=herring.p[[2]]$region, label=herring.p[[2]]$label, area=herring.p[[2]]$area),
      list(list(x=herring.p[[3]]$x, y=herring.p[[3]]$y, herring.p[[3]]$hole), type="fishing.zone", species=herring.p[[3]]$species, region=herring.p[[3]]$region, label=herring.p[[3]]$label, area=herring.p[[3]]$area),
      list(list(x=herring.p[[4]]$x, y=herring.p[[4]]$y, herring.p[[4]]$hole), type="fishing.zone", species=herring.p[[4]]$species, region=herring.p[[4]]$region, label=herring.p[[4]]$label, area=herring.p[[4]]$area),
      list(list(x=herring.p[[5]]$x, y=herring.p[[5]]$y, herring.p[[5]]$hole), type="fishing.zone", species=herring.p[[5]]$species, region=herring.p[[5]]$region, label=herring.p[[5]]$label, area=herring.p[[5]]$area),
      list(list(x=herring.p[[6]]$x, y=herring.p[[6]]$y, herring.p[[6]]$hole), type="fishing.zone", species=herring.p[[6]]$species, region=herring.p[[6]]$region, label=herring.p[[6]]$label, area=herring.p[[6]]$area),
      list(list(x=herring.p[[7]]$x, y=herring.p[[7]]$y, herring.p[[7]]$hole), type="fishing.zone", species=herring.p[[7]]$species, region=herring.p[[7]]$region, label=herring.p[[7]]$label, area=herring.p[[7]]$area)
   )

gulf.p <- c(crab.p, lobster.p2, herring.p2)



## create multipolygons for snow crab, lobster and herring
gulf.poly <-
   do.call(rbind,
           lapply(gulf.p, function(i){
              np <- length(i)-5 ## number of polygons defined for this fishing zone
              if(np==1){## we have only an exterior ring
                 my.poly <- st_sfc(st_multipolygon(list(list(cbind(i[[1]]$x,i[[1]]$y)))), crs=4326)
              } else {
                 ## create a list of lists where the first list is the exterior ring and the subsequent ones are the holes
                 ll <- list()
                 for(li in 1:np){
                    ll[[li]] <- list(cbind(i[[li]]$x,i[[li]]$y))
                 }
                 my.poly <- st_sfc(st_multipolygon(ll), crs=4326)
              }
              st_sf(
                 data.frame(
                    type="fishing zone polygon",
                    species.code=i$species,
                    region=i$region,
                    label=i$label,
                    my.poly
                 )
              )
           } ## end function
           ) ## end lapply
   )## end do.call


fz.gulf.sf <- gulf.poly ## this is the data frame that will be saved in the data folder of the package and that will also be writtenin shapefile and KML formats





##
## maps on the shared GIS drive are seemingly derived from the Atlantic Fisheries Regulations https://laws-lois.justice.gc.ca/eng/regulations/SOR-86-21/index.html
## in particular for lobster, Schedule XIII: https://laws-lois.justice.gc.ca/eng/regulations/SOR-86-21/page-25.html#h-892770
## polygons with points on land, to be used with a land overlay for mapping, and below for building polygons with coastline



## Quebec, zones 15, 16, 17, 18, 19, 20, 21, 22

zph.15 <- data.frame(
   x=-dms2deg(c(570645, 592330, 595440, 602000, 580000, 571000, 570645)),
   y=dms2deg(c(512500, 495100,501655, 505000, 515000, 515000, 512500))
)
poly.15 <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="15",
      st_sfc(st_multipolygon(list(list(cbind(zph.15$x,zph.15$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 15"

   )
)

zph.16 <- data.frame(
   x=-dms2deg(c(595440, 592330, 600000, 611600, 611600,611600, 600000, 595440, 595440)),
   y=dms2deg(c(501655, 495100, 492500, 495030, 501025, 502200, 502200, 502200, 501655))
)
poly.16 <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="16",
      st_sfc(st_multipolygon(list(list(cbind(zph.16$x,zph.16$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 16"
   )
)

zph.17 <- data.frame(
   x=-dms2deg(c(600000,600000,630000,650000,642030,611600,600000)),
   y=dms2deg(c(492500,475000,483000,495130,500630,495030,492500))
)
poly.17 <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="17",
      st_sfc(st_multipolygon(list(list(cbind(zph.17$x,zph.17$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 17"
   )
)

zph.17A <- data.frame(
   x=-dms2deg(c(642200,643730,640500,634600,632400,632400,640000,642200)),
   y=dms2deg(c(494842,493630,491330,490130,492100,492500,494842,494842))
)
poly.17A <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="17A",
      st_sfc(st_multipolygon(list(list(cbind(zph.17A$x,zph.17A$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 17A"
   )
)

zph.17B <- data.frame(
   x=-dms2deg(c(640000,642200,643730,650000,642030,611600,600000,600000,630000,634600,632400,640000)),
   y=dms2deg(c(494842,494842,493630,495130,500630,495030,492500,475000,483000,490130,492100,494842))
)
poly.17B <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="17B",
      st_sfc(st_multipolygon(list(list(cbind(zph.17B$x,zph.17B$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 17B"
   )
)

zph.18 <- data.frame(
   x=-dms2deg(c(694203,693530,690000,674009,662200,650000, 642030,611600,611600,640000, 660000,680000,694203,694203)),
   y=dms2deg(c(480806,480230,482700,485634,492405,495130, 500630,495030,501025,503000,503000,500000,483000, 480806))
)
poly.18 <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="18",
      st_sfc(st_multipolygon(list(list(cbind(zph.18$x,zph.18$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 18"
   )
)

zph.19A <- data.frame(
   x=-dms2deg(c(645500, 644400, 650000, 651633, 655239, 662200, 674009, 690000, 693530, 694203, 703000, 704840, 704411, 702442, 682132, 645500)),
   y=dms2deg(c(491200, 494030, 495130, 494605, 493404, 492405, 485634, 482700, 480230, 480806, 475000, 470257, 465606, 465904, 480154, 491200))
)

poly.19A <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="19A",
      st_sfc(st_multipolygon(list(list(cbind(zph.19A$x,zph.19A$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 19A"
   )
)

#x11();plot(zph.19A$x, zph.19A$y, pch=19)
#lines(zph.19A$x, zph.19A$y, pch=19)

zph.19B <- data.frame(
   x=-dms2deg(c(645500, 644400, 640500, 642400, 643000, 645500, 645500)),
   y=dms2deg(c(491200,  494030, 491330, 490032, 485500, 490400, 491200))
)
poly.19B <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="19B",
      st_sfc(st_multipolygon(list(list(cbind(zph.19B$x,zph.19B$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 19B"
   )
)

zph.19C <- data.frame(
   x=-dms2deg(c(642400,640500,630000,641330,640951,642247,642400)),
   y=dms2deg(c(490032,491330,483000,484300,484455,485354,490032))
)
poly.19C <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="19C",
      st_sfc(st_multipolygon(list(list(cbind(zph.19C$x,zph.19C$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 19C"
   )
)

zph.20A <- data.frame(
   x=-dms2deg(c(641330,630000,640210,643000,644136,641330)),
   y=dms2deg(c(484300,483000,481215,481215,481936,484300))
)
poly.20A <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="20A",
      st_sfc(st_multipolygon(list(list(cbind(zph.20A$x,zph.20A$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 20A"
   )
)
#x11();plot(poly.20A$geometry)

zph.20B <- data.frame(
   x=-dms2deg(c(644136,643000,650330,652910,652910,644136)),
   y=dms2deg(c(481936,481215,475745,475142,480200,481936))
)
poly.20B <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="20B",
      st_sfc(st_multipolygon(list(list(cbind(zph.20B$x,zph.20B$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 20B"
   )
)

zph.21A <- data.frame(
   x=-dms2deg(c(655410,655410,655000,652910,652910,652910,655410)),
   y=dms2deg(c(481236,480147,480130,475142,480200,480413,481236))
)
poly.21A <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="21A",
      st_sfc(st_multipolygon(list(list(cbind(zph.21A$x,zph.21A$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 21A"
   )
)

zph.21B <- data.frame(
   x=-dms2deg(c(655410,655410,661921,662130,662048,655410)),
   y=dms2deg(c(481236,480147,480316,480504,480557,481236))
)
poly.21B <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="21B",
      st_sfc(st_multipolygon(list(list(cbind(zph.21B$x,zph.21B$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 21B"
   )
)

zph.22 <- data.frame(
   x=-dms2deg(c(600000, 604500, 634730, 640210, 640210, 630000, 600000)),
   y=dms2deg(c(475000, 470218, 470758, 471825, 481215, 483000, 475000))
)
poly.22 <- st_sf(
   data.frame(
      type="fishing zone vertices",
      species.code=2550,
      region="quebec",
      label="22",
      st_sfc(st_multipolygon(list(list(cbind(zph.22$x,zph.22$y)))), crs=4326)
      # species.group="lobster",
      # name="LFA 22"
   )
)

quebec.lfas <- rbind(poly.15, poly.16, poly.17, poly.17A, poly.17B, poly.18, poly.19A, poly.19B, poly.19C, poly.20A, poly.20B, poly.21A, poly.21B, poly.22)
#library(ggplot2)
#g <- ggplot(data=quebec.lfas) +
#   geom_sf()
#g


## now create another set of polygons that are bounded by the coastline

## for each fishing zone, we will extract the coastline within its bounding box and apply a buffer to obtain all the require coastlines to perform a diference operation
bb <- st_bbox(st_buffer(poly.15,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.15.coast <- st_difference(poly.15, st_union(boundaries.temp$geometry))
poly.15.coast$type <- "fishing zone polygon"

bb <- st_bbox(st_buffer(poly.16,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.16.coast <- st_difference(poly.16, st_union(boundaries.temp$geometry))
poly.16.coast$type <- "fishing zone polygon"

bb <- st_bbox(st_buffer(poly.17,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.17.coast <- st_difference(poly.17, st_union(boundaries.temp$geometry))
poly.17.coast$type <- "fishing zone polygon"


bb <- st_bbox(st_buffer(poly.17A,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.17A.coast <- st_difference(poly.17A, st_union(boundaries.temp$geometry))
poly.17A.coast$type <- "fishing zone polygon"


bb <- st_bbox(st_buffer(poly.17B,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.17B.coast <- st_difference(poly.17B, st_union(boundaries.temp$geometry))
poly.17B.coast$type <- "fishing zone polygon"


bb <- st_bbox(st_buffer(poly.18,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.18.coast <- st_difference(poly.18, st_union(boundaries.temp$geometry))
poly.18.coast$type <- "fishing zone polygon"


bb <- st_bbox(st_buffer(poly.19A,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.19A.coast <- st_difference(poly.19A, st_union(boundaries.temp$geometry))
poly.19A.coast$type <- "fishing zone polygon"
# x11(); plot(poly.19A.coast$geometry)
# x11(); plot(boundaries.temp$geometry)

bb <- st_bbox(st_buffer(poly.19B,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.19B.coast <- st_difference(poly.19B, st_union(boundaries.temp$geometry))
poly.19B.coast$type <- "fishing zone polygon"


bb <- st_bbox(st_buffer(poly.19C,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.19C.coast <- st_difference(poly.19C, st_union(boundaries.temp$geometry))
poly.19C.coast$type <- "fishing zone polygon"

bb <- st_bbox(st_buffer(poly.20A,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.20A.coast <- st_difference(poly.20A, st_union(boundaries.temp$geometry))
poly.20A.coast$type <- "fishing zone polygon"

bb <- st_bbox(st_buffer(poly.20B,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.20B.coast <- st_difference(poly.20B, st_union(boundaries.temp$geometry))
poly.20B.coast$type <- "fishing zone polygon"

bb <- st_bbox(st_buffer(poly.21A,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.21A.coast <- st_difference(poly.21A, st_union(boundaries.temp$geometry))
poly.21A.coast$type <- "fishing zone polygon"

bb <- st_bbox(st_buffer(poly.21B,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.21B.coast <- st_difference(poly.21B, st_union(boundaries.temp$geometry))
poly.21B.coast$type <- "fishing zone polygon"

bb <- st_bbox(st_buffer(poly.22,0.1))
boundaries.temp <- st_crop(boundaries_simple, bb)
poly.22.coast <- st_difference(poly.22, st_union(boundaries.temp$geometry))
poly.22.coast$type <- "fishing zone polygon"

fz.quebec.sf <- rbind(quebec.lfas, poly.15.coast, poly.16.coast, poly.17.coast, poly.17A.coast, poly.17B.coast, poly.18.coast, poly.19A.coast, poly.19B.coast, poly.19C.coast, poly.20A.coast, poly.20B.coast, poly.21A.coast, poly.21B.coast, poly.22.coast)
## object fz.quebec.sf now contains all LFAs for Quebec, both as fishing zone vertices and fishing zone polygons


## Gulf, zones 23, 24, 25, 26A, 26B


#library(ggplot2)
#g <- ggplot(data=poly.19A.coast) +
#   geom_sf()
## read in the google earth kml file associated with My Places
kml.file <- file.path("C:/Users/RicardD/AppData/LocalLow/Google/GoogleEarth","myplaces.kml")
#st_layers(kml.file)
kml.in <- st_read(kml.file, layer="My Places")
pei.land.kml <- kml.in[kml.in$Name=="PEI-land-1",]
st_coordinates(pei.land.kml)



#################
## Newfoundland polygons with coastlines generated by Michael Elliot
nfld.shp <- read_sf("inst/extdata/shapefiles/NFLD_Lobster_v2.shp")
str(nfld.shp,3)

st_crs(nfld.shp)

fz.nfld.sf <- st_sf(
   data.frame(
      type="fishing zone",
      species.code=2550,
      region="newfoundland",
      label=gsub("LFA ", "", nfld.shp$Zone),
      st_transform(nfld.shp$geometry,4326)
   )
)

##using the same method as above, create Nfld fishing zones with coastlines



## shared GIS folder mess
## of all the folders and subfolder available on the shared drive, none of which has any metadata, one called "FishingZones.gdb" seems the most complete, so start from there
fgdb <- "D:/Base Maps/FishingAreas/FishingZones.gdb"

## lobster zones
lobster.gdb <- st_read(fgdb, "Zones_Homard") ## DOES NOT INCLUDE Newfoundland

## herring zones
herring.gdb <- st_read(fgdb, "Zones_Hareng")

## mackerel zones
mackerel.gdb <- st_read(fgdb, "Zones_Maquereau")

## snow crab zones
snowcrab.gdb <- st_read(fgdb, "Zones_Crabe_des_neiges")

## groundfish zones
groundfish.gdb <- st_read(fgdb, "Zones_Poisson_fond")

save(lobster.gdb, herring.gdb, mackerel.gdb, snowcrab.gdb, groundfish.gdb, file="from-gdb.Rda")
#plot(lobster.gdb$Shape)


## single data frame with all the fishing zone polygons
#st_crs(fz.nfld.sf)
#st_crs(fz.gulf.sf)
#st_crs(fz.quebec.sf)
vars <- c("type","species.code","region","label","geometry")
fz.all.for.rda <- rbind(fz.gulf.sf[,vars], fz.quebec.sf[,vars], fz.nfld.sf[,vars])


boundaries_simple <- boundaries %>%
   filter(
      POL_DIV %in% c(
         "Quebec",
         "New Brunswick",
         "Nova Scotia",
         "Prince Edward Island",
         "Newfoundland and Labrador"
      ),
      SELECTION == "sparse"
   ) %>%
   st_transform(4326)


boundaries_simple <- st_crop(boundaries_simple, st_bbox(fz.all.for.rda))

lobster <- fz.all.for.rda[fz.all.for.rda$species.code==2550 & fz.all.for.rda$type=="fishing zone polygon",]
lobster <- cbind(lobster, st_coordinates(st_centroid(lobster)))

g <- ggplot(data=boundaries_simple) +
   geom_sf(fill=grey(0.8), color=grey(0.3)) +
   geom_sf(data=lobster, color="red", fill="mistyrose") +
   geom_label(data=lobster, aes(X, Y, label=label))
ggsave(file="Gulf-of-St-Lawrence-lobster-areas.pdf", g, width = 30, height = 20, units = "cm")


g <- ggplot(data=boundaries_simple) +
   geom_sf(fill=grey(0.8), color=grey(0.3)) +
   geom_sf(data=lobster.gdb, color="red", fill="mistyrose")
ggsave(file="Gulf-of-St-Lawrence-lobster-areas-gdb.pdf", g, width = 30, height = 20, units = "cm")


## compare LFA 15 from my sf data frame and the one read from the geospatial database
st_area(lobster[lobster$label=="15",]$geometry)
st_area(lobster.gdb[lobster.gdb$Unité_gest=="15",])

tiff("LFA15-comparions.tiff", compression="lzw", width=2000, height=2000,res=300)
plot((lobster.gdb[lobster.gdb$Unité_gest=="15",]$Shape), border="red")
plot((lobster[lobster$label=="15",]$geometry), add=TRUE)
dev.off()


crab <- fz.all.for.rda[fz.all.for.rda$species.code==2526,]
g <- ggplot(data=boundaries_simple) +
   geom_sf(fill=grey(0.8), color=grey(0.3)) +
   geom_sf(data=crab, color="red", fill="mistyrose")
ggsave(file="Gulf-of-St-Lawrence-snow-crab-areas.pdf", g, width = 30, height = 20, units = "cm")


herring <- fz.all.for.rda[fz.all.for.rda$species.code==60,]
g <- ggplot(data=boundaries_simple) +
   geom_sf(fill=grey(0.8), color=grey(0.3)) +
   geom_sf(data=herring, color="red", fill="mistyrose")
ggsave(file="Gulf-of-St-Lawrence-herring-areas.pdf", g, width = 30, height = 20, units = "cm")


## we can now write the Rda file that will be included in gulf.spatial
save(fz.all.for.rda, file="./data/fishing.zone.polygons.rda")
#save(fz.vertices.all.for.rda, file="./data/fishing.zone.vertices.rda")


## also create 2 shapefiles:
## 1 - shapefile of polygon vertices without coastline (suitable to make a map with a coastline overlay)
## 2 - shapefile of polygons with coastlines

polygons <- fz.all.for.rda[fz.all.for.rda$type=="fishing zone polygon",]
vertices <- fz.all.for.rda[fz.all.for.rda$type=="fishing zone vertices",]


write_sf(vertices, file.path(here(), "inst/extdata/shapefiles/fishing.zone.vertices.shp")) ## silently overwrites shapefile
write_sf(polygons, file.path(here(), "inst/extdata/shapefiles/fishing.zone.polygons.shp")) ## silently overwrites shapefile

write_sf(vertices, file.path(here(), "inst/extdata/shapefiles/fishing.zone.vertices.kml")) ## silently overwrites shapefile
write_sf(polygons, file.path(here(), "inst/extdata/shapefiles/fishing.zone.polygons.kml")) ## silently overwrites shapefile

## get shapefiles from the shared GIS network drive
#fgdb <- "D:/Base Maps/FishingAreas/Groundfish"
#groundfish.fc <- sf::st_read(fgdb, layer = "Groundfish_Border_Lox")

#x11(); plot(st_geometry(groundfish.fc), col="black")



## files that are produced in this script:
## Gulf of St. Lawrence fishing zones lines and multilines
## Gulf of St. Lawrence fishing zones polygons without coastline
## - American lobster
## - Snow crab
## - Herring
## - Groundfish

