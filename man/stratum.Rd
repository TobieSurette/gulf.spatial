\name{stratum}

\alias{stratum}
\alias{stratum.default}
\alias{stratum.gulf.set}

\title{
   RV Spatial Strata
}

\description{
   Returns the observed or determines the RV survey spatial strata.
}

\usage{
   # Generic 'stratum' function:
   stratum(x, ...)
   
   # Default 'stratum' function:
   stratum(longitude, latitude, region = "gulf", survey = "rv", ...)
   
   # 'gulf.set' 'stratum' function:
   stratum(x, method = c("observed", "latlong"), ...)
}

\arguments{
   \item{longitude}{Numerical vector of longitudes in decimal degree format.}
   
   \item{latitude}{Numerical vector of latitudes in decimal degree format.}
   
   \item{region}{Character value specifying the geographic region. This argument 
                 is passed onto the \code{\link[gulf]{stratum.info}} function.}
                 
   \item{survey}{Character value specifying the research survey. This argument 
                 is passed onto the \code{\link[gulf]{stratum.info}} function.}
                 
   \item{x}{A set card object.}
   
   \item{method}{A character string specifying the method to be used for 
                 generating the stratum identifiers. The default value is
                 \sQuote{observed}.}       
                 
   \item{\ldots}{Further arguments (not used).}                           
}

\details{
   
}

\value{
   Returns a numerical vector the same size as \code{latitude} and \code{longitude}
   containing the RV stratum number for each given coordinate.
}

\author{
   Tobie Surette \email{Tobie.Surette@dfo-mpo.gc.ca} \cr  
   Pablo Vergara \email{Pablo.Vergara@dfo-mpo.gc.ca}
}
        
\seealso{
   \code{\link[gulf]{block.number}},
   \code{\link[gulf]{stratum.polygons}},
   \code{\link[gulf]{in.polygon}},
   \code{\link[gulf]{which.polygon}}
}

\examples{   
   # Find the stratum at a single point:
   stratum(-63.8, 47.05) # returns 429
   
   # Find strata values for multiple points:
   lat <- c(48, 47, 46.5)
   long <- c(-64, -61.5, -62)
   stratum(long, lat) # returns: c(422, 434, 401) 
   
   # Read a set card:
   x <- read.card(year = 2010)
   # Determine strata:
   S <- stratum(x, method = "latlon")  
}
