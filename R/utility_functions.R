#' Quick plot of hexagon edges using ggplot
#'
#' This is simply a short cut for using ggplot to draw the geometries in the
#' \code{shapes} element (an \code{sf} spatial data frame) of a
#' \code{hexlattice} object. If the lattice is very large it will be slow to
#' draw and the hexagons too small to see properly. In such cases you can use
#' the \code{xbnds} and \code{ybnds} arguments to draw just a portion of the
#' lattice. If the lattice does not yet have geometries a warning message
#' is issued and the function invisibly returns NULL.
#'
#' @param h Lattice of hexagons; an object of class \code{hexlattice}
#'   as produced by \code{\link{make_hexagons}}.
#'
#' @param xbnds A vector of the minimum and maximum X ordinates of
#'   hexagons to include. The range is applied to hexagon centroids.
#'   If \code{NULL} (default) the full X range is included in the plot.
#'
#' @param ybnds A vector of the minimum and maximum Y ordinates of
#'   hexagons to include. The range is applied to hexagon centroids.
#'   If \code{NULL} (default) the full Y range is included in the plot.
#'
#' @importFrom ggplot2 ggplot geom_sf
#'
#' @return A ggplot object.
#'
#' @export
#'
plot.hexlattice <- function(h, xbnds = NULL, ybnds = NULL) {
  stopifnot(inherits(h, "hexlattice"))

  if (!has_geometries(h)) {
    warning("Nothing to plot. Hexagon geometries not created for yet")
    return( invisible(NULL) )
  }

  if (is.null(xbnds) && is.null(ybnds)) {
    dat <- h$shapes

  } else {
    if (is.null(xbnds)) xbnds <- h$xbnds
    if (is.null(ybnds)) ybnds <- h$ybnds

    dat <- dplyr::filter(h$shapes,
                         xc >= xbnds[1], xc <= xbnds[2],
                         yc >= ybnds[1], yc <= ybnds[2])
  }

  ggplot(data = dat) +
    geom_sf(colour = "black", fill = NA)
}


#' Print a brief description of a hexagon lattice object
#'
#' @param h Lattice of hexagons; an object of class \code{hexlattice}
#'   as produced by \code{\link{make_hexagons}}.
#'
#' @return Invisibly returns a list with elements: num.hexagons,
#'   width, side (side length), area, xbnds, ybnds, epsg (integer EPSG code
#'   for coordinate reference system).
#'
#' @export
#'
summary.hexlattice <- function(h) {
  stopifnot(inherits(h, "hexlattice"))

  cat("Lattice of", nrow(h$shapes), "hexagons\n",

      "Geometries created:", has_geometries(h), "\n",

      "Hexagon width:", h$width, "\n",
      "Hexagon side length:", h$side, "\n",
      "Hexagon area:", h$area, "\n",

      "Bounds in X direction:", h$xbnds, "\n",
      "Bounds in Y direction:", h$ybnds, "\n")

  epsg <- sf::st_crs(h$shapes)$epsg

  cat(" Coordinate reference system (EPSG code):", epsg, "\n")

  invisible(list(num.hexagons = nrow(h$shapes),
                 width = h$width,
                 side = h$side,
                 area = h$area,
                 xbnds = h$xbnds,
                 ybnds = h$ybnds,
                 epsg = epsg)
            )
}


#' Check if a hexlattice object has geometries
#'
#' @param h Lattice of hexagons; an object of class \code{hexlattice}
#'   as produced by \code{\link{make_hexagons}}.
#'
#' @return TRUE if geometries are present, FALSE otherwise.
#'
#' @export
#'
has_geometries <- function(h) {
  stopifnot(inherits(h, "hexlattice"))

  inherits(h$shapes, "sf")
}
