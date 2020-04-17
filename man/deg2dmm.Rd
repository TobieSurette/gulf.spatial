\name{deg2dmm}

\alias{deg2dmm}

\title{Coordinate Conversion Functions}

\description{
   Convert geographical coordinates from decimal degree format to degree decimal-minute (DMM) format.
}

\usage{
   deg2dmm(x, as.list = FALSE, digits = NULL)
}

\arguments{
   \item{x}{A numeric vector or matrix of geographical coordinates. Coordinates are
            assumed to be in decimal degree format.}

   \item{as.list}{A logical value stating whether to return the converted coordinates as
                  a list with fields \code{deg} and \code{min}, representing the
                  coordinate degrees and minutes, respectively. The default format
                  is numeric with a four-digit integer portion plus a decimal minute fraction,
                  whose precision is controlled by \code{digits}. The default is \code{FALSE}.}

   \item{digits}{The precision of the converted coordinates to be returned. It specifies
                 the number of digits in the minutes part of the converted coordinates.
                 The default is \code{digits = NULL}, which returns full precision.}
}

\value{
   A matrix or list whose dimension follows the dimension of input argument \code{x}.
}

\details{
   This function belongs to a suite of \code{\link[gulf]{Coordinate Conversion Functions}}.
}

\seealso{
   \code{\link[gulf]{dms2deg}},
   \code{\link[gulf]{deg2dms}},
   \code{\link[gulf]{dmm2deg}},
   \code{\link[gulf]{deg2km}},
   \code{\link[gulf]{km2deg}},
   \code{\link[gulf]{deg.str}},
   \code{\link[gulf]{loran2deg}}
}

\keyword{IO}

\examples{
   # Convert a decimal degree coordinate to degree-minute-second format:
   deg2dmm(45.25)

   # Convert a vector of decimal degree coordinates to degree-minute-second format:
   deg2dmm(seq(45, 46, by = 0.05), as.list = FALSE)

   # Perform conversion and return as a list:
   deg2dmm(seq(45, 46, by = 0.05), as.list = TRUE)
}

\author{Tobie Surette \email{Tobie.Surette@dfo-mpo.gc.ca} \cr
        Pablo Vergara \email{Pablo.Vergara@dfo-mpo.gc.ca}}