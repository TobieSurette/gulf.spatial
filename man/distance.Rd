% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distance.R
\name{distance}
\alias{distance}
\alias{distance.default}
\title{Calculate Distance}
\usage{
distance(x, ...)

\method{distance}{default}(
  x,
  y,
  x0 = NULL,
  y0 = NULL,
  coordinate.units = "latlon",
  distance.units = "km",
  pairwise = TRUE,
  method = "utm",
  ...
)
}
\arguments{
\item{x}{Horizontal coordinates.}

\item{y}{Vertical coordinates.}

\item{x0}{Second set of horizontal coordinates.}

\item{y0}{Second set of vertical coordinates.}

\item{coordinate.units}{Units of the input coordinates.}

\item{distance.units}{Units distances to be returned.}

\item{pairwise}{Logical value specifying whether to calculate all possible pairwise combinations of coordinates.}

\item{method}{Method used to calculate distance.}
}
\description{
Functions to retrieve or calculate distance.
}
\section{Methods (by class)}{
\itemize{
\item \code{distance(default)}: Default distance method.

}}
\seealso{
deg2km
}
