#' Create a lattice of hexagons
#'
#' @param w Hexagon width.
#'
#' @param xbnds Vector of two values for the minimum (left) and maximum (right)
#'   X ordinates of the bounding rectangle.
#'
#' @param ybnds Vector of two values for the minimum (bottom) and maximum (top)
#'   Y ordinates of the bounding rectangle.
#'
#' @param lazy If TRUE, hexagon geometries (\code{sf} polygons) are not
#'   created immediately to save time and space. The lattice data frame
#'   (element 'shapes' in the returned \code{hexlattice} object) will just
#'   contain hexagon IDs, centroid (X,Y) coordinates and axial (Q-R)
#'   coordinates. If FALSE, geometries will be created for the returned
#'   lattice.
#'
#' @param quiet If TRUE, suppress console ouput about the details of the
#'   lattice being created.
#'
#' @return A list (class \code{hexlattice}) with the following elements:
#'   \describe{
#'     \item{width}{Hexagon width (X dimension).}
#'     \item{side}{Hexagon side length.}
#'     \item{area}{Hexagon area.}
#'     \item{xbnds}{X bounds specified when creating the lattice.}
#'     \item{ybnds}{Y bounds specified when creating the lattice.}
#'     \item{shapes}{A spatial data frame (class \code{sf}) with hexagon
#'     edges as the geometry column, and columns for id (integer: 1-N
#'     starting at lower left and proceeding row-wise), centroid X (xc),
#'     centroid Y (yc), hexagon lattice column (q), hexagon lattice row
#'     (r).}
#'   }
#'
#' @importFrom dplyr %>% mutate
#'
#' @export
#'
make_hexagons <- function(w, xbnds, ybnds, lazy = TRUE, quiet = FALSE) {
  centroids <- make_centroids(w, xbnds, ybnds)

  if (!quiet) cat("Creating lattice of", nrow(centroids), "hexagons\n")

  sidelen <- hex_width2side(w)

  qr <- .do_xy2hex(xy = centroids,
                   origin = c(xbnds[1], ybnds[1]),
                   sidelen = sidelen)

  shapes <- data.frame(
    id = 1:nrow(centroids),
    xc = centroids[, "x"],
    yc = centroids[, "y"],
    q = qr[, "q"],
    r = qr[, "r"]
  )

  if (!lazy) {
    geometry <- .make_hexagons_from_centroids(centroids, w)
    shapes <- sf::st_sf(shapes, geometry)
  }

  lattice <- list(
    side = sidelen,
    width = w,
    area = hex_width2area(w),
    xbnds = xbnds,
    ybnds = ybnds,

    shapes = shapes
  )

  class(lattice) <- c("hexlattice", "list")

  lattice
}


#' Create centroids for a hexagon lattice
#'
#' Adapted from code by Tim Keitt posted at:
#' https://stat.ethz.ch/pipermail/r-sig-geo/2007-March/001791.html
#'
#' @param w Hexagon width.
#'
#' @param xbnds Vector of two values for the minimum (left) and maximum (right)
#'   X ordinates of the bounding rectangle.
#'
#' @param ybnds Vector of two values for the minimum (bottom) and maximum (top)
#'   Y ordinates of the bounding rectangle.
#'
#' @return A two column matrix of centroid coordinates.
#'
#' @examples
#' # Create 1 square kilometre hexagons over a given area
#' width <- hex_area2width(1000000)
#' centroids <- make_hexagons(width, xbnds = c(700000, 750000), ybnds = c(6500000, 6600000))
#'
#' @export
#'
make_centroids <- function(w, xbnds, ybnds) {
  # Vertical distance between hexagon centres in lattice
  dy <- ROOT3 * w / 2

  x <- seq(xbnds[1], xbnds[2], w)
  y <- seq(ybnds[1], ybnds[2], dy)

  y <- rep(y, each = length(x))
  x <- rep(c(x, x + w / 2), length.out = length(y))

  cbind(x = x, y = y)
}


#' Add geometries to a hexagonal lattice
#'
#' This function checks if the lattice already has geometries and, if not,
#' creates them.
#'
#' @param h Lattice of hexagons; an object of class \code{hexlattice}
#'   as produced by \code{\link{make_hexagons}}.
#'
#' @param force If TRUE, create new geometries even when there are existing
#'   ones present. Default is FALSE.
#'
#' @return A copy of the input lattice with geometries added.
#'
#' @export
#'
create_geometries <- function(h, force = FALSE) {
  if (has_geometries(h) && !force) {
    message("Lattice already has geometries")

  } else {
    geoms <- .make_hexagons_from_centroids(h$shapes[, c("xc", "yc")], h$width)

    h$shapes <- sf::st_sf(shapes, geometry = sf::st_sfc(geoms))
  }

  h
}


# Makes a list (a plain list, not ans `sfc` object) of hexagon geometries
# given a valid grid of centroids
#
# Adapted from code by Tim Keitt posted at:
# https://stat.ethz.ch/pipermail/r-sig-geo/2007-March/001791.html
#
.make_hexagons_from_centroids <- function(centroids, w = NULL) {
  if (!is.matrix(centroids)) {
    if (is.vector(centroids)) {
      if (length(centroids) != 2) stop("If centroids is a vector it must have two elements (x, y)")
      else centroids <- matrix(centroids, nrow = 1)

    } else if (is.data.frame(centroids)) {
      if (ncol(centroids) != 2) stop("centroids data frame should have two columns")
      else centroids <- as.matrix(centroids)

    } else if (is.matrix(centroids)) {
      if (ncol(centroids) != 2) stop("centroids matrix should have two columns")

    } else {
      stop("centroids must be a two-column matrix or data frame or a two-element vector")
    }
  }

  xs <- sort(unique(centroids[,1]))
  if (length(xs) > 1) {
    w <- 2 * (xs[2] - xs[1])
  } else if (is.null(w)) {
    stop("Only one distinct centroid X value - width must be supplied")
  }

  dy <- w / sqrt(3)

  x.offsets <- c(-w/2, 0, w/2, w/2, 0, -w/2, -w/2)
  y.offsets <- c(dy/2, dy, dy/2, -dy/2, -dy, -dy/2, dy/2)

  f <- function(i) list(x = hexGrid$x[i] + x.offset,
                        y = hexGrid$y[i] + y.offset)

  polys <- lapply(1:nrow(centroids), function(i) {
    x <- centroids[i,1] + x.offsets
    y <- centroids[i,2] + y.offsets
    sf::st_polygon(list(cbind(x, y)))
  })

  polys
}


