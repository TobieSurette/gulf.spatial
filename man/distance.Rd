\name{distance}

\alias{distance}
\alias{distance.default}

\title{Calculate Distances Between Points}

\description{
   Calculate distances between sets of points given their coordinates.
}

\usage{
   # Generic 'distance' function:
   distance(x, y, ...)

   # Default 'distance' method:
   distance(x, y, x0 = NULL, y0 = NULL,
            coordinate.units = "latlon", distance.units = "km",
            pairwise = TRUE, method = "utm")
}


\arguments{
   \item{x}{Vector of 'x' coordinate values. Must be the same size as \code{y}}

   \item{y}{Vector of 'y' coordinate values. Must be the same size as \code{x}}

   \item{x0}{Optional second vector of 'x' coordinate values. Must be the same size as \code{y0}}

   \item{y0}{Optional second vector of 'y' coordinate values. Must be the same size as \code{x0}}

   \item{coordinate.units}{A character string. Specifies the format of the input coordinates.
                           It may be \code{coordinate.units} = \dQuote{latlon} or \dQuote{km}.}

   \item{distance.units}{A character string. Specifies the units of the output distances.
                         They may be returned in kilometers (\code{distance.units} = \dQuote{km})
                         or in meters (= \dQuote{m}).}

   \item{pairwise}{A logical value. Whether to calculate all the pairwise distances
                   between sets of points or the one-to-one distances between the
                   (x, y) and (x0, y0) vectors. The default is \code{FALSE}.}

   \item{method}{A character string specifying the method to be used for calculating
                 the distances between coordinate points. The options are \sQuote{utm}
                 which first converts the coordinates into UTM NAD83 coordinates, or
                 \sQuote{ellipsoid} which models the Earth as an ellipsoid with major
                 and minor radii of 6378.16 and 6356.775 kilomters, respectively.
                 The default is \sQuote{utm}.}
}

\details{
   Input coordinates may be in latitude-longitude (decimal degrees) and the
   appropriate conversions are performed if \code{coordinate.units} is set to
   \dQuote{latlon}. Note that the order that the coordinates must be entered
   is longitude and then latitude. For other coordinate units the Euclidean
   distance is directly applied.

   If only the \code{x} and \code{y} arguments are given, then it is assumed that
   pairwise distances between each point are desired. They are returned as a square
   distance matrix.
   
   When \code{x}, \code{y}, \code{x0} and \code{y0} arguments are all provided
   there are two types of distance calculations which may be performed, depending
   on the \code{pairwise} flag. When \code{pairwise} is \code{FALSE} and the
   dimensions of \code{x}, \code{y}, \code{x0} and \code{y0} are identical, then
   an element by element distance calculation between the (x, y) and (x0, y0) point
   sets is performed. When \code{pairwise} is \code{TRUE}, then a (n x k) matrix
   of all pairwise distances between (x, y) and (x0, y0) is returned, where 'n' is
   the length of \code{x} and \code{y}, and 'k' is the length of \code{x0} and
   \code{y0}.
   
   When the \code{method} argument is set to \sQuote{ellipsoid}, reference
   coordinates (i.e. \sQuote{long.ref} and \sQuote{lat.ref}) must be specified.
   These are then passed along to the \code{\link[gulf]{deg2km}} function.
   
}

\value{
   A numeric matrix or vector of distances. The distance units depend on the units
   of the input coordinates and the \code{distance.units} argument.
}

\keyword{IO}

\examples{
   # Calculate the distance between a pair of lat-lon coordinates.
   d <- distance(-66.5, 46.25, -65.75, 47.00)
   
   # Return the pairwise distances for a single vector of points:
   lon <- c(-66, -66, -64.25)
   lat <- c(46, 45.5, 46)
   d <- distance(lon, lat)
   
   # Return the pairwise distances for two coordinate sets:
   lon2 <- c(-65, -63)
   lat2 <- c(45.46, 47.00)
   d <- distance(lon, lat, lon2, lat2, pairwise = TRUE)
   print(d)
}

\seealso{
   \code{\link[gulf]{Gulf Mapping Functions}},
   \code{\link[gulf]{Coordinate Conversion Functions}},
   \code{\link[gulf]{distance}}
}

\author{
   Tobie Surette \email{Tobie.Surette@dfo-mpo.gc.ca} \cr
   Pablo Vergara \email{Pablo.Vergara@dfo-mpo.gc.ca}
}
