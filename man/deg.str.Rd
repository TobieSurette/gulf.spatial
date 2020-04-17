\name{deg.str}

\alias{deg.str}

\title{Coordinate Conversion Functions}

\description{
   Convert a decimal-degree coordinate to a formatted character string for printing.
}

\usage{
   deg.str(deg = NULL, latitude = NULL, longitude = NULL, clip = TRUE,
           northing = TRUE, easting = TRUE, as.list = FALSE)
}

\arguments{
   \item{deg}{A numeric vector of coordinates in decimal degree format.}

   \item{latitude}{A numeric vector of coordinates the same format as \code{deg}.
         If \code{latitude} is specified and \code{northing} is \code{TRUE}, then
         a Northing character (i.e. "N" or "S") is included in the output character
         string. The default is \code{TRUE}.}

   \item{longitude}{A numeric vector of coordinates the same format as \code{deg}.
         If \code{longitude} is specified and \code{easting} is \code{TRUE}, then
         an Easting character (i.e. "E" or "W") is included in the output character
         string. The default is \code{TRUE}.}

   \item{clip}{A logical value. Whether to strip away coordinate seconds or minutes
         and seconds when they have zero values. The resulting output strings are
         of an abridged form. The default is \code{TRUE}.}
         
   \item{northing}{A logical value. Whether to include a Northing character (i.e.
         "N" or "S")in the output string. The coordinates must be passed through
         \code{latitude} for this option to be used. The default is \code{TRUE}.}
         
   \item{easting}{A logical value. Whether to include an Easting character (i.e.
         "E" or "W")in the output string. The coordinates must be passed through
         \code{longitude} for this option to be used. The default is \code{TRUE}.}

   \item{as.list}{A logical value. Whether to return the output as a list object.
         The default is \code{FALSE}.}
}

\value{
   If a single argument is provided and as.list is FALSE, then the returned
   output is a character string vector. Otherwise, the output is returned
   as a list with fields \code{long} and \code{lat}.
}

\details{
   This function belongs to a suite of \code{\link[gulf]{Coordinate Conversion Functions}}.
}

\seealso{
   \code{\link[gulf]{dms2deg}},
   \code{\link[gulf]{dmm2deg}},
   \code{\link[gulf]{deg2dmm}},
   \code{\link[gulf]{dmm2deg}},
   \code{\link[gulf]{deg2km}},
   \code{\link[gulf]{km2deg}},
   \code{\link[gulf]{loran2deg}}
}

\examples{
   # Convert generic coordinate:
   deg.str(c(45, 47.5))

   # Convert latitude coordinate, which adds the northing character to the output:
   deg.str(lat = c(45, 47.5))

   # Convert longitude coordinate, which adds the easting character to the output:
   deg.str(long = c(45, 47.5))

   # Convert generic coordinate without clipping the output:
   deg.str(c(45, 47.5), clip = FALSE)
   
   # Convert generic coordinate without clipping the output:
   deg.str(c(45, 47.5), clip = FALSE)

   # Convert vectors of lat-long coordinates, which are returned as a list:
   deg.str(latitude = 45:47, longitude = -64:-66, clip = FALSE)
}

\author{Tobie Surette \email{Tobie.Surette@dfo-mpo.gc.ca} \cr
        Pablo Vergara \email{Pablo.Vergara@dfo-mpo.gc.ca}
}