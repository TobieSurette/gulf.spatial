\name{longitude}

\alias{latitude}
\alias{longitude}
\alias{latitude.gulf.set}
\alias{longitude.gulf.set}
\alias{latitude.scset}
\alias{longitude.scset}

\title{
   Latitude-Longitude Methods
}

\description{
   Returns the latitude or longitude associated with an object.
}

\usage{
   'generic' method:
   latitude(x)

   'generic' method:
   longitude(x)

   'gulf.set' method:
   latitude.gulf.set(x)

   'gulf.set' method:
   longitude.gulf.set(x)
   
   'scset' method:
   latitude.scset(x)

   'scset' method:
   longitude.scset(x)   
}

\arguments{
   \item{x}{A \sQuote{gulf.set} object.}
}

\details{
    For \sQuote{gulf.set} objects, the mid-point of the longitude or latitude
    is returned.
}

\value{
    A numeric vector containing the mid-longitude or mid-latitude is returned.
    \code{NA} values are ignored. The number of elements in the vector matches
    the number of records in the \sQuote{gulf.set} object.
}

\author{
   Tobie Surette \email{Tobie.Surette@dfo-mpo.gc.ca} \cr
   Pablo Vergara \email{Pablo.Vergara@dfo-mpo.gc.ca}
}

\seealso{
   \code{\link[gulf]{ascii Class Methods}},
   \code{\link[gulf]{convert.vector}},
   \code{\link[base]{format}}
}

\examples{
   # Read set card:
   x <- read.card(year = 2008, experiment = 1)
   
   # Plot mid-point of tows:
   gulf.map()    # Display base map.
   map.strata()  # Display survey strata.
   points(longitude(x), latitude(x), pch = 19, col = "red", cex = 0.5)
}
