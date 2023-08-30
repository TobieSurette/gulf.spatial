library(gulf.spatial)
library(RColorBrewer)

s <- read.gulf.spatial("fishing polygons shp")
s <- s[which((s@data$species == 2550) & (s@data$region %in% c("gulf", "quebec")) & (as.numeric(substr(s@data$label, 1, 2)) >= 20)), ]
cols <- brewer.pal(8, "Set2")

clg()
png(file = paste0("lobster season by LFA.png"), units = "in", res = 500, height = 7, width = 8.5)

map.new()
plot(s[s@data$label == "25", ], add = TRUE, col = cols[4], border = "grey50")
plot(s[s@data$label %in% c("20A", "22", "24", "26A", "26B"), ], add = TRUE, col = cols[2], border = "grey50")
plot(s[s@data$label %in% c("21A", "21B", "23"), ], add = TRUE, col = cols[6], border = "grey50")
plot(s[s@data$label %in% c("20B"), ], add = TRUE, col = cols[7], border = "grey50") 
map("coast")
map.axis(1:2)
grid()

# LFA labels:
text(-64.5, 47.5, "23", font = 2, cex = 1.15)
text(-62.5, 47.7, "22", font = 2, cex = 1.15)
text(-62.8, 46.75, "24", font = 2, cex = 1.15)
text(-64.35, 46.45, "25", font = 2, cex = 1.15)
text(-61.5, 46.3, "26B", font = 2, cex = 1.15)
text(-62.25, 45.90, "26A", font = 2, cex = 1.15)
text(-64, 48.4, "20A", font = 2, cex = 1.15)
text(-65, 48.07, "20B", font = 2, cex = 0.85, srt = 35)
text(-65.70, 48.04, "21B", font = 2, cex = 0.85, srt = -35)
text(-66.05, 48.10, "21A", font = 2, cex = 0.85, srt = 0)

# Provinces and regions:
text(-63.45, 46.38, "Prince Edward Island", srt = -18, cex = 0.75, font = 2)
text(-60.9, 46.38, "Cape Breton", srt = 58, cex = 0.85, font = 2)
text(-63.5, 45.65, "Nova Scotia", srt = 0, cex = 1.0, font = 2)
text(-65.65, 46.75, "New Brunswick", srt = 0, cex = 1.0, font = 2)
text(-65.5, 48.65, "Québec", srt = 0, cex = 1.0, font = 2)

# Add legend:
legend("topright", 
       legend = c("May 1 to June 30", 
                  "May 8 to July 7",
                  "May 9 to July 9",
                  "August 10 to October 10"), 
       pch = 22,
       pt.cex = 3.0,
       pt.bg = cols[c(2,7,6,4)],
       col = "grey50",
       box.col = "grey"
       )

box(col = "grey60")
dev.off()
