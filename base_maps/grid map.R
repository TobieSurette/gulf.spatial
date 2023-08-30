library(gulf.spatial)
library(gulf.graphics)

# Draw map:
clg()
pdf(file = paste0("results/maps/grid map.pdf"), width = 8.5, height = 7)

map.new()
#rect(par("usr")[1] + 1/6, par("usr")[3], par("usr")[2], par("usr")[4]-1/6, border = "skyblue4", lwd = 2)
map("bathymetry")

str <- paste0(c(paste0("G", LETTERS), paste0("H", LETTERS)))
str <- str[which(substr(str, 1, 2) == "GR"):which(substr(str, 1, 2) == "HN")]
str <- expand.grid(str, 23:60)
str <- apply(str, 1, function(x) paste0(x, collapse = ""))
plot.grid(str, col = NA, border = "grey50")
xx <- grid2deg(str)
text((xx[,1]+xx[,3])/2, (xx[,2]+xx[,4])/2, str, cex = 0.3)

vline(-66:-60, col = "grey50", lty = "dashed")
hline(44:49, col = "grey50", lty = "dashed")

map("coast")

# Provinces and regions:
text(-63.45, 46.38, "Prince Edward Island", srt = -18, cex = 0.75, font = 2)
text(-60.9, 46.38, "Cape Breton", srt = 58, cex = 0.85, font = 2)
text(-63.5, 45.65, "Nova Scotia", srt = 0, cex = 1.0, font = 2)
text(-65.65, 46.75, "New Brunswick", srt = 0, cex = 1.0, font = 2)
text(-65.5, 48.65, "Québec", srt = 0, cex = 1.0, font = 2)

wind.rose(-60.5, 48.6)

# Fishing zones:
v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
v <- subset(v, label %in% c("12", "12E", "12F", "19"))
plot(v, add = TRUE, lwd = 0.5)

# Erase corner:
plot.grid("GQ22", col = "white", border = "white")

str <- paste0(c(paste0("G", LETTERS), paste0("H", LETTERS)), 22)
str <- str[which(substr(str, 1, 2) == "GR"):which(substr(str, 1, 2) == "HN")]
plot.grid(str, col = "papayawhip")
tmp <- grid2deg(str)
text(apply(tmp[, c(1,3)], 1, mean), apply(tmp[, c(2,4)], 1, mean), substr(str, 1, 2), cex = 0.60)

str <- paste0("GQ", 23:60)
plot.grid(str, col = "papayawhip")
tmp <- grid2deg(str)
text(apply(tmp[, c(1,3)], 1, mean), apply(tmp[, c(2,4)], 1, mean), substr(str, 3, 4), cex = 0.60)

map.axis(1:2)
box(col = "grey50")

dev.off()

