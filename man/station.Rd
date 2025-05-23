% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/station.R
\name{station.default}
\alias{station.default}
\title{Group Sampling Stations by Location}
\usage{
\method{station}{default}(longitude, latitude, time, distance.tolerance = 1, time.tolerance, ...)
}
\arguments{
\item{longitude, latitude}{Numeric vector in decimal degrees.}

\item{time}{Numeric vector or time (\sQuote{POSIX}) objects.}

\item{distance.tolerance}{Distance (in kilometers) below which a pair of points is considered to belong to the same sampling station.}

\item{time.tolerance}{Numeric value specifying the time-difference below which a pair of points is considered to have occured at the same time.}

\item{...}{Other arguments (not used).}
}
\description{
Group sampling stations by spatial proximity.
}
\section{Methods (by class)}{
\itemize{
\item \code{station(default)}: Assign a spatial location identifier based on spatio-temporal proximity.

}}
