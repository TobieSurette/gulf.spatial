## coastline
## coastline file from the Atlas of Canada
## https://open.canada.ca/data/en/dataset/fec926ca-25ad-5c92-a9a7-6009c21d17b3
download.file(
    paste(
       "http://ftp.geogratis.gc.ca",
       "pub/nrcan_rncan/vector/framework_cadre/Atlas_of_Canada_1M",
       "boundary/AC_1M_BoundaryPolygons.shp.zip",
       sep="/"
    ),
    "AC_1M_BoundaryPolygons.shp.zip"
 )
unzip("AC_1M_BoundaryPolygons.shp.zip", exdir="AC")

boundaries <- read_sf("AC/AC_1M_BoundaryPolygons_shp/AC_1M_BoundaryPolygons.shp")

library(sf)
library(dplyr)
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

library(ggplot2)
g <- ggplot(data=boundaries_simple) +
   geom_sf(fill=grey(0.8), color=grey(0.3)) +
   xlim(-72,-48) + ylim(43,53)
