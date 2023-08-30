library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

clg()
language <- language("fr")
height <- 7
width <- 8.5

if (language == "english"){
   chaleur.str     <- "Chaleur Bay"
   shediac.str     <- "Shediac Valley"
   magdalen.str    <- "Magdalen Channel"
   cape.breton.str <- "Cape Breton Corridor"
   american.str    <- "American\nBank"
   bradelle.str    <- "Bradelle\nBank"
   orphan.str      <- "Orphan\nBank"
   laurentian.str  <- "Laurentian   Channel"
   miscou.str      <- "Miscou\nBank"
   pieter.str      <- "Pieter\nBank"
   bennett.str     <- "Bennett\nBank"
   george.str      <- "St.\nGeorge's\nBay"
   eastern.bradelle.str <- "Eastern Bradelle Valley"
   western.bradelle.str <- "Western Bradelle Valley"

   pei.str  <- "Prince Edward Island"
   cb.str   <- "Cape Breton"
   ns.str   <- "Nova Scotia"
   nb.str   <- "New\nBrunswick"

   # Fishing zones:
   zone12.str <- "Area 12"
   zoneE.str  <- "Area 12E"
   zoneF.str  <- "Area 12F"
   zone19.str <- "Area 19"
}

if (language == "french"){
   chaleur.str     <- "Baie-des-Chaleurs"
   shediac.str     <- "Vallée de Shediac"
   magdalen.str    <- "Chenal Madelinot"
   cape.breton.str <- "Corridor du Cap-Breton"
   american.str    <- "Banc des\nAméricains"
   bradelle.str    <- "Banc\nBradelle"
   orphan.str      <- "Banc des\nOrphelins"
   laurentian.str  <- "Chenal   Laurentien"
   miscou.str      <- "Banc de\nMiscou"
   pieter.str      <- "Banc de\nPieter"
   bennett.str     <- "Banc\nBennett"
   george.str      <- "Baie St.\nGeorge"
   eastern.bradelle.str <- "Vallée Bradelle Est"
   western.bradelle.str <- "Vallée Bradelle Ouest"

   pei.str  <- "Ile-du-Prince-Edouard"
   cb.str   <- "Cap-Breton"
   ns.str   <- "Nouvelle-Ecosse"
   nb.str   <- "Nouveau\nBrunswick"

   # Fishing zones:
   zone12.str <- "Zone 12"
   zoneE.str  <- "Zone 12E"
   zoneF.str  <- "Zone 12F"
   zone19.str <- "Zone 19"
}

tiff(file = paste0("results/maps/sGSL geography SAR - ", language, ".tiff"), compression = "lzw", units = "in", res = 300, height = height, width = width)

map.new()
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "lightblue")

map("bathymetry")

x <- read.gulf.spatial("fishing zone vertices shp", species = 2526)
x <- x[x@data$label %in% c("12", "12E", "12F", "19"), ]
plot(x, add = TRUE, lwd = 0.8)

grid(col = "grey60")

map("coast")
map.axis(1:4)

# Locations:
text(-65, 47.97, chaleur.str, srt = 18, cex = 0.8, font = 3)
text(-64.25, 47.5, shediac.str, srt = 65, cex = 0.7, font = 3)
text(-62.35, 47.5, magdalen.str, srt = 65, cex = 0.65, font = 3)
text(-61.27, 47.05, cape.breton.str, srt = 54, cex = 0.65, font = 3)
text(-63.83, 48.55, american.str, srt = 0, cex = 0.7, font = 3)
text(-62.9, 47.4, bradelle.str, srt = 0, cex = 0.85, font = 3)
text(-63.5, 48.25, orphan.str, srt = 0, cex = 0.7, font = 3)
text(-61.6, 48.5, laurentian.str, srt = -24, cex = 1.2, font = 3)

#text(-62.45, 48.2, bennett.str, srt = 0, cex = 0.6, font = 3)
#text(-61.68, 45.8, george.str, srt = 0, cex = 0.5, font = 3)

#text(-62.38, 47.88, eastern.bradelle.str, srt = 63, cex = 0.55, font = 3)
#text(-62.85, 48.0, western.bradelle.str, srt = 75, cex = 0.55, font = 3)

# Provinces and regions:
text(-63.45, 46.38, pei.str, srt = -18, cex = 0.75, font = 2)
text(-60.9, 46.38, cb.str, srt = 58, cex = 0.85, font = 2)
text(-63.5, 45.65, ns.str, srt = 0, cex = 1.0, font = 2)
text(-65.65, 46.75, nb.str, srt = 0, cex = 1.0, font = 2)
text(-65.5, 48.65, "Québec", srt = 0, cex = 1.0, font = 2)

# Fishing zones:
text(-63.60, 47.5, zone12.str, srt = 0, cex = 1.0, font = 2)
text(-62.4, 48.48, zoneE.str, srt = -25, cex = 0.9, font = 2)
text(-60.75, 47.685, zoneF.str, srt = -30, cex = 0.9, font = 2)
text(-60.9, 47.0, zone19.str, srt = 60, cex = 0.9, font = 2)

#text(-60.5, 47.40, "B", srt = 0, cex = 0.9, font = 2)
#text(-61.33, 46.49, "C", srt = 0, cex = 0.9, font = 2)

box()
wind.rose()
scale.bar(length = 80)

dev.off()

