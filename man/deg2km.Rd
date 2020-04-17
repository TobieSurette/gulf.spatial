\name{deg2km}

\alias{deg2km}

\title{Coordinate Conversion Functions}

\description{
   Convert geographical coordinates from decimal degree format to kilometer format.
}

\usage{
   deg2km(x, y, long.ref = NULL, lat.ref = NULL, method = "utm")
}

\arguments{
   \item{x}{A numeric vector of longitude coordinates. Coordinates are assumed
            to be in decimal degree format.}

   \item{y}{A numeric vector of latitude coordinates. Coordinates are assumed
            to be in decimal degree format.}

   \item{long.ref}{A longitude reference coordinate from which the coordinate
                   distances will be calculated. Assumed to be in decimal degree
                   format.}

   \item{lat.ref}{A latitude reference coordinate from which the coordinate
                  distances will be calculated. Assumed to be in decimal degree
                  format.}
               
   \item{method}{A character string specifying the method to be used for calculating
                 the distances between coordinate points. The options are \sQuote{utm}
                 which first converts the coordinates into UTM NAD83 coordinates, or
                 \sQuote{ellipsoid} which models the Earth as an ellipsoid with major
                 and minor radii of 6378.16 and 6356.775 kilomters, respectively.
                 The default is \sQuote{utm}.}
}

\value{
   A list with fields \code{x} and \code{y} in kilometers whose dimension follows
   the dimension of the original input arguments \code{x} and \code{y}.
}

\details{
   This function belongs to a suite of \code{\link[gulf]{Coordinate Conversion Functions}}.
}

\seealso{
   \code{\link[gulf]{dms2deg}},
   \code{\link[gulf]{dmm2deg}},
   \code{\link[gulf]{deg2dmm}},
   \code{\link[gulf]{deg2km}},
   \code{\link[gulf]{km2deg}},
   \code{\link[gulf]{deg.str}},
   \code{\link[gulf]{loran2deg}},
   \code{\link[gulf]{deg.str}}
}

\examples{
   # Convert a decimal degree coordinate to kilometers format:
   deg2km(-63.123, 47.56)

   # Convert a vector of decimal degree coordinates to kilometer format:
   deg2km(seq(-65, -63, len = 21), seq(46, 48, len = 21))
}

\author{
   Tobie Surette \email{Tobie.Surette@dfo-mpo.gc.ca} \cr
   Pablo Vergara \email{Pablo.Vergara@dfo-mpo.gc.ca}
}