% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wind.rose.R
\name{wind.rose}
\alias{wind.rose}
\alias{wind}
\alias{rose}
\title{Draw Wind Rose}
\usage{
wind.rose(
  longitude,
  latitude,
  radius = 0.3,
  pos = 3,
  bearing = 0,
  units = "inches",
  margin = 0.04,
  convert = TRUE,
  cex = 0.6,
  ...
)
}
\arguments{
\item{radius}{Numerical value specifying the size (as a radius) of the wind rose. The distance units are specified by \code{units}.}

\item{pos}{Numerical value specifying the position of the wind rose on the graph. It may take the following values: 
1 = Lower-left corner, 2 = Lower-right corner, 3 = Upper-right corner and 4 = Upper-left corner.  The 
default is \code{3}.}

\item{bearing}{Numerical value specifying the degrees in radians that the wind rose is to be rotated counter-clockwise.}

\item{units}{Character string specifying the length units to be used when interpreting the \sQuote{radius} argument. 
It may take the following values: \sQuote{user} (user coordinate system), \sQuote{in} or \sQuote{inches} 
(in inches) or \sQuote{km} or \sQuote{kilometers} (in kilometers). The default is \sQuote{inches}.}

\item{convert}{Whether or not to convert radius units to degree scale when units = \sQuote{km} or \sQuote{kilometers}.}

\item{cex}{Character expansion value for the wind rose labels.}

\item{longitude.latitude}{Coordinates specifying the center location of the wind rose is to be drawn.}
}
\description{
Display a wind rose on a map.
}
\examples{
# Gulf of Saint-Lawrence base map:
map()
wind.rose()
}
