#' @title Write Image Data as GeoTIFF
#'
#' @description Write Grid or Image to a GeoTIFF. GeoTIFF is a public domain metadata standard which allows georeferencing
#'              information to be embedded within a TIFF file. The potential additional information includes map projection,
#'              coordinate systems, ellipsoids, datums, and everything else necessary to establish the exact spatial reference
#'              for the file.
#'
#' @param z Two-dimensional matrix or array.
#' @param file.name File name.
#' @param longitude,latitude Vectors of regularly spaced coordinate that define the spatial extent of the image.
#'                           If \code{longitude}.

#' @export write.geotiff
write.geotiff <- function(z, file.name, longitude, latitude){

   # Initialize image ranges:
   xlim <- ylim <- NULL

   # Parse 'longitude' argument:
   if (!missing(longitude)){
      if (length(longitude) == 2){
         xlim <- sort(longitude)
      }else{
         if (length(longitude) == nrow(z)){
            xlim <- range(longitude)
         }else{
            stop("'longitude' length must be compatible with the dimensions of 'z'.")
         }
      }
   }

   # Parse 'latitude' argument:
   if (!missing(latitude)){
      if (length(latitude) == 2){
         ylim <- sort(latitude)
      }else{
         if (length(latitude) == nrow(z)){
            ylim <- range(latitude)
         }else{
            stop("'latitude' length must be compatible with the dimensions of 'z'.")
         }
      }
   }

   # Attempt to get image range from dimension labels:
   if (is.null(xlim)){
      if (!is.null(rownames(z))){
         tmp <- rownames(z)
         if (all(gsub("[-.0-9]", "", tmp) == "")){
            xlim <- range(as.numeric(tmp), na.rm = TRUE)
         }
      }
   }
   if (is.null(ylim)){
      if (!is.null(colnames(z))){
         tmp <- colnames(z)
         if (all(gsub("[-.0-9]", "", tmp) == "")){
            ylim <- range(as.numeric(tmp), na.rm = TRUE)
         }
      }
   }
   if (is.null(xlim) | is.null(ylim)) stop("Unable to define image coordinate ranges.")

   # Export to GeoTIFF format:
   r <- map.fishery(year[i], var = var, bathymetry = TRUE, legend = FALSE, language = language)
   rownames(r$density) <- lon
   colnames(r$density) <- lat
   r$density <- t(r$density)
   r$density <- r$density[rev(1:nrow(r$density)), ]
   lon <- r$longitude
   lat <- r$latitude
   r <- raster::raster(r$density)
   extent(r) <- c(min(lon), max(lon), min(lat), max(lat))
   crs(r) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")
   r@title <- paste0("Snow crab fishery ", var, " ", year[i])
   terra::writeRaster(r, paste0(r@title, ".tif"), overwrite = TRUE, options = c("COMPRESS=NONE", "TFW=YES"))
}

