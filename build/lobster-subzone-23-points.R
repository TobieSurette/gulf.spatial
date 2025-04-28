library(gulf.spatial)
lobster.23 <- read.table(file="./lobster-LFA-23-sub-zones-points.txt", header=TRUE, sep=" ", colClasses=c("numeric","numeric","numeric"))
lobster.23.land <- read.table(file="./lobster-LFA-23-sub-zones-land-points.txt", header=TRUE, sep=" ", colClasses=c("character","numeric","numeric","numeric"))


pdf("temp.pdf", width=6, height=5)
map.new(xlim=c(-66.7,-63.8), ylim=c(46.7,48.5), region="gulf")
coastline()
text(lobster.23$longitude, lobster.23$latitude, lobster.23$point.number)
text(lobster.23.land$longitude, lobster.23.land$latitude, lobster.23.land$point.number, col="red")
dev.off()

