% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{summary.hexlattice}
\alias{summary.hexlattice}
\title{Print a brief description of a hexagon lattice object}
\usage{
\method{summary}{hexlattice}(h)
}
\arguments{
\item{h}{Lattice of hexagons; an object of class \code{hexlattice}
as produced by \code{\link{make_hexagons}}.}
}
\value{
Invisibly returns a list with elements: num.hexagons,
  width, side (side length), area, xbnds, ybnds, epsg (integer EPSG code
  for coordinate reference system).
}
\description{
Print a brief description of a hexagon lattice object
}
