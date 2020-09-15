#' Coordinates and Grids
#'
#' @description Functions to assign a grid label(s) given coordinates, and vice versa.
#'
#' @param x Horizontal coordinate vector (e.g. decimal longitude) or grid name.
#' @param y Vertical coordinate vector (e.g. decimal latitude).
#' @param xref A numerical value specifying the horizontal reference coordinate. Reference coordinates specify the location of the the lower-left corner of reference grid.
#' @param yref A numerical value specifying the vertical reference coordinate. Reference coordinates specify the location of the the lower-left corner of reference grid.
#' @param dx A numerical value specifying the horizontal width of each regular grid.
#' @param dy A numerical value specifying the vertical height of each regular grid.
#' @param reference A four-character string of the form XXYY, where XX are two letters and YY are two digits.
#' @param quarter.grid A logical value specifying whether the grid is to include northing and easting qualifiers.
#'
#' @examples
#' deg2grid(-64, 47) # Returns "HD37".
#' grid2deg("HD37")  # Return corner coordinates for "HD37".
#' grid2deg("HD37", vertices = TRUE)  # Return vertex coordinates for "HD37".
#'
#' # Generate 100 random coordinates:
#' lon <- -66 + 6 * runif(100)
#' lat <- 45 + 4 * runif(100)
#' deg2grid(lon, lat)
#'
#' @seealso \code{\link{plot.grid}}

#' @describeIn deg2grid Returns a grid name for a given coordinate(s).
#' @export deg2grid
deg2grid <- function(x, y,  quarter.grid = FALSE, xref = -66-1/3, yref = 45, dx = 1/6, dy = 1/6, reference = "HP23"){
   # Parse 'x':
   if (missing(y)) stop("'y' coordinates required.")
   x <- -abs(x)

   # Calculate 'x' index:
   xi <- as.numeric(substr(reference, 3, 4)) + floor(round((x - xref)/dx, 10))
   xstr <- gsub(" ", "0", formatC(xi, width = 2))

   # Calculate 'y' index:
   ystr <- num2abc(abc2num(reference) - floor(round((y-yref)/dy, 10)))
   str <- paste0(ystr, xstr)

   if (quarter.grid){
      ystr <- paste0(ystr, c("E", "W")[(((x/dx) - floor(x/dx)) >= 0.5)+1])
      xstr <- paste0(c("S", "N")[(((y/dy) - floor(y/dy)) >= 0.5)+1], xstr)
   }

   return(paste0(ystr, xstr))
}

# Internal functions:
abc2num <- function(x) return(26*(match(substr(toupper(x), 1, 1), LETTERS)-1) + match(substr(toupper(x), 2, 2), LETTERS)-1)
num2abc <- function(x) return(paste0(LETTERS[floor(x / 26) + 1], LETTERS[x %% 26 + 1]))

#' @describeIn deg2grid Returns the corner coordinates of a grid.
#' @export grid2deg
grid2deg <- function(x, xref = -66-1/3, yref = 45, dx = 1/6, dy = 1/6, reference = "HP23", vertices = FALSE){
   x <- toupper(x)

   # Calculate horizontal lower left coordinate:
   xr <- xref + dx * (as.numeric(substr(x, 3, 4)) - as.numeric(substr(reference, 3, 4)))

   # Calculate vertical lower left coordinate:
   yr <- yref - dy * (abc2num(substr(x, 1, 2)) - abc2num(reference))

   # Add corner coordinates:
   if (!vertices){
      v <- data.frame(left = xr, bottom = yr, right = xr + dx, top = yr + dy)
   }else{
      xx  <- cbind(xr, xr + dx, xr + dx, xr, NA)
      yy  <- cbind(yr, yr, yr + dy, yr + dy, NA)
      gg <- cbind(repvec(x, ncol = 4), "")
      v <- data.frame(grid = as.vector(t(gg)),
                      longitude = as.vector(t(xx)),
                      latitude = as.vector(t(yy)),
                      stringsAsFactors = FALSE)
   }

   return(v)
}

