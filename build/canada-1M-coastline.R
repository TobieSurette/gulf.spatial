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
