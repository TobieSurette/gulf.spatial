library(gulf)

clg()

xlim <- c(-66.5, -60-1/6)
ylim <- c(45.5-1/6, 48.5+5/6)
ratio <- distance(xlim[1], ylim[1], xlim[1], ylim[2]) / distance(xlim[1], ylim[1], xlim[2], ylim[1])
width <- 9
windows(height = ratio * width, width = width);
plot(xlim, ylim, type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")

bathymetry(sea = TRUE)

# bathymetry(dem = FALSE, levels =  -c(10, 25, 50, 100, 150, 200, 300, 400))

# Draw lat-lon grid lines:
xx <- seq(-66.5, -60, by = 0.5)
yy <- seq(45, 49, by = 0.5)
for (i in 1:length(xx)) lines(c(xx[i], xx[i]), par("usr")[3:4], lty = "dotted", col = "lightgrey")
for (i in 1:length(yy)) lines(par("usr")[1:2], c(yy[i], yy[i]),  lty = "dotted", col = "lightgrey")  
      
xxx <- seq(-66.5 + 1/6, -60, by = 1/6)
yyy <- seq(45, 49, by = 1/6)
for (i in 1:length(xxx)) lines(c(xxx[i], xxx[i]), c(45, 49+1/6), col = "grey75")
for (i in 1:length(yyy)) lines(c(-66.5+1/6, -60), c(yyy[i], yyy[i]),  col = "grey75")
#for (i in 1:(length(xxx)-1)){
#   for (j in 1:(length(yyy)-1)){
#      mx <- (xxx[i]+xxx[i+1]) / 2
#      my <- (yyy[j]+yyy[j+1]) / 2
#      text(mx, my, DFOgrid.str(mx, my), cex = 0.5)
#   }
#}

# Draw fishing zone lines without drawing the shoreline:
data("fishing.zone.polygons")
p <- subset(fishing.zone.polygons, species = 2526)
for (i in 1:length(p)){
   for (j in 1:length(p[[i]])){
      if ("x" %in% names(p[[i]][[j]])){
         xx <- p[[i]][[j]]$x
         yy <- p[[i]][[j]]$y
         dd <- depth(xx, yy)
         index <- which(depth(xx, yy) < -30) # Threshold depth for crab zone lines.
         if (length(index) > 0){
            for (k in 1:length(index)){
               if (index[k] > 1) lines(xx[(index[k]-1):index[k]], yy[(index[k]-1):index[k]], col = "grey10", lwd = 1)
               if (index[k] < length(xx)) lines(xx[index[k]:(index[k]+1)], yy[index[k]:(index[k]+1)], col = "grey10", lwd = 1)
            }
         }
      }
   }
}
  

coastline(col = grey(0.9), border = grey(0.6))


map.place.names(sea = FALSE, language = "english")
box()

map.axis(side = c(1, 4))

tmp <- data.frame(longitude = seq(-66.5-1/12, -60, by = 1/6), latitude = 45+3/12)

# Erase corner:
map.grid("GQ22", col = "white", border = "white")

str <- paste0("GQ", 23:59)
map.grid(str, col = "papayawhip")
tmp <- grid.corners(str)
text(apply(tmp[, c(1,3)], 1, mean), apply(tmp[, c(2,4)], 1, mean), substr(rownames(tmp), 3, 4), cex = 0.60)

str <- paste0(c(paste0("G", LETTERS), paste0("H", LETTERS)), 22)
str <- str[which(substr(str, 1, 2) == "GR"):which(substr(str, 1, 2) == "HN")]
map.grid(str, col = "papayawhip")
tmp <- grid.corners(str)
text(apply(tmp[, c(1,3)], 1, mean), apply(tmp[, c(2,4)], 1, mean), substr(rownames(tmp), 1, 2), cex = 0.60)

rect(par("usr")[1] + 1/6, par("usr")[3], par("usr")[2], par("usr")[4]-1/6, border = "skyblue4", lwd = 2)

wind.rose(-60.75, 48.3+3/6, radius = 0.2)
scale.bar(-66, 45.4+1/6, len = 80, by = 20)   
   