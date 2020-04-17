\name{dmm2deg}

\alias{dmm2deg}

\title{Coordinate Conversion Functions}

\description{
   Convert geographical coordinates from degree decimal-minute (DMM) format to decimal degree format.
}

\usage{
   dmm2deg(x = NULL, degrees = NULL, minutes = NULL, list = NULL)
}

\arguments{
   \item{x}{Vector of degree-minute-second (DMS) values. May be either integers or character strings. May also
            be a list with the same structure as the \code{list} argument.}

   \item{degrees}{Vector of coordinate degrees. To be used with \code{minutes} argument in
                  specifying coordinates to be converted.}

   \item{minutes}{Vector of coordinate minutes. To be used with \code{degrees} argument in
                  specifying coordinates to be converted.}

   \item{list}{A list or data frame object with \code{deg}, \code{min} and \code{sec} attributes.}
}

\value{
   A vector or matrix whose dimension follows the dimension of the input coordinates.
}

\details{
   There are three ways to specify the input coordinates. They may be entered as a single vector or matrix
   via the \code{x} argument, or they may be entered as separate components jointly via the \code{degrees} and
   \code{minutes} arguments, or entered as a list with \code{deg} and \code{min} fields.
   
   This function belongs to a suite of \code{\link[gulf]{Coordinate Conversion Functions}}.
}

\seealso{
   \code{\link[gulf]{deg2dms}},
   \code{\link[gulf]{dms2deg}},
   \code{\link[gulf]{deg2dmm}},
   \code{\link[gulf]{deg2km}},
   \code{\link[gulf]{km2deg}},
   \code{\link[gulf]{deg.str}},
   \code{\link[gulf]{loran2deg}}
}

\keyword{IO}

\examples{
   # Convert coordinate from DMM format to decimal degree:
   dmm2deg(4530.852)

   # Convert coordinate from DMM format to decimal degree by passing a character string:
   dmm2deg("4530.852")

   # Alternate way of converting coordinate from DMM format to decimal degree:
   dmm2deg(deg = 45, min = 30.852)

   # Alternate way of converting coordinate from DMM format to decimal degree:
   y <- list(deg = 45, min = 30.852)
   dmm2deg(list = y)

   # Equivalently, we may pass 'y' directly:
   dmm2deg(y)

   # Convert a vector of DMM values to decimal degree format:
   dmm2deg(c(4530.852, 4600.30, 6545.00))
}

\author{Tobie Surette \email{Tobie.Surette@dfo-mpo.gc.ca} \cr
        Pablo Vergara \email{Pablo.Vergara@dfo-mpo.gc.ca}}