\name{dms2deg}

\alias{dms2deg}

\title{Coordinate Conversion Functions}

\description{
   Convert geographical coordinates from degree-minute-second (DMS) format to decimal degree format.
}

\usage{
   dms2deg(x = NULL, degrees = NULL, minutes = NULL, seconds = NULL, list = NULL)
}

\arguments{
   \item{x}{Vector of degree-minute-second (DMS) values. May be either integers or character strings. May also
            be a list with the same structure as the \code{list} argument.}

   \item{degrees}{Vector of coordinate degrees. To be used with \code{minutes} and \code{seconds} arguments in
                  specifying coordinates to be converted.}
   
   \item{minutes}{Vector of coordinate minutes. To be used with \code{degrees} and \code{seconds} arguments in
                  specifying coordinates to be converted.}
   
   \item{seconds}{Vector of coordinate degrees. To be used with \code{degrees} and \code{minutes} arguments in
                  specifying coordinates to be converted.}
   
   \item{list}{A list or data frame object with \code{deg}, \code{min} and \code{sec} attributes.}
}

\value{
   A vector or matrix whose dimension follows the dimension of the input coordinates.
}

\details{
   There are three ways to specify the input coordinates. They may be entered as a single vector or matrix
   via the \code{x} argument, or they may be entered as separate components jointly via the \code{degrees},
   \code{minutes} and \code{seconds} arguments, or entered as a list with \code{deg}, \code{min} and \code{sec}
   fields.
   
   This function belongs to a suite of \code{\link[gulf]{Coordinate Conversion Functions}}.
}

\seealso{
   \code{\link[gulf]{deg2dms}},
   \code{\link[gulf]{dmm2deg}},
   \code{\link[gulf]{deg2dmm}},
   \code{\link[gulf]{deg2km}},
   \code{\link[gulf]{km2deg}},
   \code{\link[gulf]{deg.str}},
   \code{\link[gulf]{loran2deg}}
}

\keyword{IO}

\examples{
   # Convert coordinate from DMS format to decimal degree:
   dms2deg(453030.5)

   # Convert coordinate from DMS format to decimal degree by passing a character string:
   dms2deg("453030.5")
   
   # Alternate way of converting coordinate from DMS format to decimal degree:
   dms2deg(deg = 45, min = 30, sec = 30.5)

   # Alternate way of converting coordinate from DMS format to decimal degree:
   y <- list(deg = 45, min = 30, sec = 30.5)
   dms2deg(list = y)

   # Equivalently, we may pass 'y' directly:
   dms2deg(y)
   
   # Convert a vector of DMS values to decimal degree format:
   dms2deg(c(453500, 460030, 654500))
}

\author{Tobie Surette \email{Tobie.Surette@dfo-mpo.gc.ca} \cr
        Pablo Vergara \email{Pablo.Vergara@dfo-mpo.gc.ca}}