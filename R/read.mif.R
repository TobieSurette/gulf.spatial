#' Read MapInfo MIF Files
#'
#' @description Reads MapInfo Interchange Format (MIF) files.
#'
#' @param file File name.
#' @param print Logical value specifying whether to print information about graphic objects being read.
#'

# READ.MIF - Read a MapInfo Interchange Format (MIF) file.
#' @export read.mif
read.mif <- function(file, print = FALSE){
   # Read file:
   x <- read.delim(file, header = FALSE, colClasses = "character")
   x <- toupper(gulf.utils::deblank(x[,1]))

   # Determine where graphics objects begins:
   start <- grep("COLUMNS", x) + as.numeric(strsplit(x[grep("COLUMNS", x)], " ")[[1]][2]) + 2

   # Create table of graphic object line and type identifiers:
   tab <- NULL
   index <- grep("POINT", x); index <- index[index >= start]
   if (length(index) > 0) tab <- rbind(tab, data.frame(index = index, type = "point", stringsAsFactors = FALSE))
   index <- setdiff(grep("LINE", x), grep("PLINE", x)); index <- index[index >= start]
   if (length(index) > 0) tab <- rbind(tab, data.frame(index = index, type = "line", stringsAsFactors = FALSE))
   index <- grep("PLINE", x); index <- index[index >= start]
   if (length(index) > 0) tab <- rbind(tab, data.frame(index = index, type = "pline", stringsAsFactors = FALSE))
   index <- grep("REGION", x); index <- index[index >= start]
   if (length(index) > 0) tab <- rbind(tab, data.frame(index = index, type = "region", stringsAsFactors = FALSE))

   if (any(dim(tab) == c(0, 0))) return(NULL)

   S <- vector("list", nrow(tab))

   # Extract region coordinates:
   for (i in 1:nrow(tab)){
      index <- tab[i, 1]

      # Parse "Point" object:
      if (tab[i, 2] == "point"){
         temp <- as.numeric(strsplit(x[index], " ")[[1]][2:3])
         S[[i]]$x <- temp[1]
         S[[i]]$y <- temp[2]

         # Add 'line' tag:
         S[[i]]$type <- "line"
      }

      # Parse "Line" object:
      if (tab[i, 2] == "line"){
         temp <- substr(x[index], 6, nchar(x[index]))
         temp <- as.numeric(unlist(strsplit(temp, " ")))
         S[[i]]$x <- temp[seq(1, length(temp), by = 2)]
         S[[i]]$y <- temp[seq(2, length(temp), by = 2)]

         # Add 'line' tag:
         S[[i]]$type <- "line"
      }

      # Parse "Pline" object:
      if (tab[i, 2] == "pline"){
         k <- as.numeric(substr(x[index], 7, nchar(x[index]))) # Number of coordinates in the polyline.
         m <- index + 1 # Index of first coordinate to be read.
         temp <- as.numeric(unlist(strsplit(x[m:(m+k-1)], " ")))
         S[[i]]$x <- temp[seq(1, length(temp), by = 2)]
         S[[i]]$y <- temp[seq(2, length(temp), by = 2)]
         # Add 'line' tag:
         S[[i]]$type <- "line"
      }

      # Parse "region" object:
      if (tab[i, 2] == "region"){
         k <- as.numeric(substr(x[index], 7, nchar(x[index]))) # Number of polygons in the region.
         m <- index + 2 # Index of first coordinate to be read.
         n <- as.numeric(x[m-1]) # Number of vertices to be read.
         # If polygon is simple, then create it, if multiple then add to it:
         for (j in 1:k){
            temp <- as.numeric(unlist(strsplit(x[m:(m+n-1)], " ")))
            xx <- temp[seq(1, length(temp), by = 2)]
            yy <- temp[seq(2, length(temp), by = 2)]
            if (j == 1){
               S[[i]]$x <- xx
               S[[i]]$y <- yy
            }else{
               S[[i]]$x <- c(S[[i]]$x, NA, xx)
               S[[i]]$y <- c(S[[i]]$y, NA, yy)
            }
            if ((k > 1) & (j != k)){
               m <- m + n + 1
               n <- as.numeric(x[m-1])
            }
         }
         # Add 'polygon' tag:
         S[[i]]$type <- "polygon"
      }
   }

   # Output which objects were loaded:
   if (print){
      if (any(tab[, 2] == "line")) cat(paste("Loaded", sum(tab[, 2] == "line"), "line objects.\n"))
      if (any(tab[, 2] == "pline")) cat(paste("Loaded", sum(tab[, 2] == "pline"), "polyline objects.\n"))
      if (any(tab[, 2] == "region")) cat(paste("Loaded", sum(tab[, 2] == "region"), "polygon objects.\n"))
   }

   # Read MID data file:
   midfile <- gsub("[Mm][Ii][Ff]$", "mid", file)
   if (file.exists(midfile)){
      # Get MID file names of data fields:
      index <- grep("COLUMNS", x)
      if (length(index) == 1){
         ncol <- as.numeric(strsplit(x[index], " ")[[1]][2])
         temp <- lapply(strsplit(x[index+(1:ncol)], " "), function(x) x[x != ""])
         names <- unlist(lapply(temp, function(x) x[[1]]))
      }

      # Get name of MID (data) file and read it:
      index <- grep("DELIMITER", x)
      sep <- strsplit(x[index], " ")[[1]][2]
      y <- read.delim(midfile, header = FALSE, sep = sep, stringsAsFactors = FALSE)
      names(y) <- tolower(names)

      for (i in 1:length(S)) S[[i]] <- c(S[[i]], y[i,,drop = FALSE])
   }

   return(S)
}
