library(sp)

load("/Users/crustacean/Desktop/gulf.spatial/stratum.polygons.rda")

p <- list()
for (i in 1:length(strata)){
   x <- as.data.frame(strata[[i]][c("longitude", "latitude")])
   x$index <- cumsum(is.na(x$longitude))
   x <- x[!is.na(x$longitude), ]
   tmp <- list()
   for (j in 0:max(x$index)){
      tmp[[j+1]] <- Polygon(x[x$index == j, 1:2], hole = FALSE)
   }
   p[[i]] <- Polygons(tmp, i)
}
SP <- SpatialPolygons(p)

# Compile survey summary data:
vars <- unlist(lapply(strata, function(x) names(x)[unlist(lapply(x, function(x) length(x) <= 1))]))
vars <- unique(vars)
data <- NULL
for (i in 1:length(vars)){
   tmp <- lapply(strata, function(x) if (is.null(x[[vars[i]]])) return(NA) else return(x[[vars[i]]]))
   data <- cbind(data, unlist(tmp))
}
data <- as.data.frame(data)
names(data) <- vars
nvars <- c("area", "label.x", "label.y", "label.angle", "trawlable.units")
for (i in 1:length(nvars)) data[, nvars[i]] <- as.numeric(as.character(data[, nvars[i]]))
svars <- setdiff(names(data), nvars)
for (i in 1:length(svars)) data[, svars[i]] <- as.character(data[, svars[i]])

# Remove block number:
index <- is.na(data$stratum) & !is.na(data$block.number)
data$stratum[index] <- data$block.number[index]
data <- data[, -which(names(data) %in% c("block.number"))]

# Shorten field names:
str <- gsub("[.]", "_", names(data))
str <- gsub("trawlable_units", "t_units", str)
str <- gsub("label_angle", "label_an", str)
names(data) <- str

# Combine polygons and data:
y <- SpatialPolygonsDataFrame(SP, data)

writeOGR(y, layer = "survey.stratum.polygons",
         dsn = 'inst/extdata/shapefiles', driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# Read and plot shape files:
z <- readOGR(layer = "survey.stratum.polygons", dsn = "inst/extdata/shapefiles")
#plot(z, ID = "gulf")

plot(c(-66, -60), c(45, 49))
polygon(z@polygons[[3]]@Polygons[[1]]@coords)
title(main = z@data[3, 1])

dir(path = paste0(find.package("gulf.spatial"), "/extdata/"))






