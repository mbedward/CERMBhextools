% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{tail.hexlattice}
\alias{tail.hexlattice}
\title{Return the last rows of a hexlattice data frame}
\usage{
\method{tail}{hexlattice}(x, n = 6L, geometry = FALSE, ...)
}
\arguments{
\item{x}{A \code{hexlattice} object.}

\item{n}{Number of rows to return or, if negative, the number of
initial rows to omit.}

\item{geometry}{If TRUE, include the geometry column if present. If FALSE
(default) omit the geometry column.}

\item{...}{Arguments to be passed to other methods (presently unused).}
}
\value{
A data frame. If the lattice has geometries this will be an
  \code{sf} data frame.
}
\description{
Return the last rows of a hexlattice data frame
}
