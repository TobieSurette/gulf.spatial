\name{km2deg}

\alias{km2deg}

\title{Coordinate Conversion Functions}

\description{
   Convert geographical coordinates from kilometer format to decimal degree format.
}

\usage{
   km2deg(x, y, long.ref = NULL, lat.ref = NULL, method = "utm")
}

\arguments{
   \item{x}{A numeric vector of x coordinates. They are the relative distance in
            kilometers with respect to the reference coordinate (\code{long.ref}, 
            \code{lat.ref}).}

   \item{y}{A numeric vector of y coordinates. They are the relative distance in
            kilometers with respect to the reference coordinate (\code{long.ref}, 
            \code{lat.ref}).}

   \item{long.ref}{Longitude reference coordinate from which the \code{x} and \code{y}
                   kilometer values are referenced.}

   \item{lat.ref}{Latitude reference coordinate from which the \code{x} and \code{y}
                  kilometer values are referenced.}

   \item{method}{A character string specifying the method to be used for calculating
                 the distances between coordinate points. The options are \sQuote{utm}
                 which first converts the coordinates into UTM NAD83 coordinates, or
                 \sQuote{ellipsoid} which models the Earth as an ellipsoid with major
                 and minor radii of 6378.16 and 6356.775 kilomters, respectively.
                 The default is \sQuote{utm}.}
}

\value{
   A list with fields \code{longitude} and \code{latitude} in decimal degree format whose 
   dimensions is identical to those of the input arguments \code{x} and \code{y}.
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
   \code{\link[gulf]{deg.str}},
   \code{\link[gulf]{loran2deg}},
   \code{\link[gulf]{deg.str}}
}

\examples{
   # Convert a coordinate in kilometer format to decimal degree format:
   km2deg(-227.3639, 111.1331) 
   
   # Return the value of the reference coordinate:
   km2deg(0, 0)

   # Convert a vector of kilometer-format coordinates to decimal degree format:
   km2deg(seq(0, -200, len = 21), seq(0, 100, len = 21))
}

\author{
   Tobie Surette \email{Tobie.Surette@dfo-mpo.gc.ca} \cr
   Pablo Vergara \email{Pablo.Vergara@dfo-mpo.gc.ca}
}
