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
#' @return A list (class \code{hexlattice}) with the following elements:
#'   \code{hwidth} - hexagon width (X dimension);
#'   \code{centroids} - a two-column matrix of hexagon centroids;
#'   \code{edges} - a spatial data frame of hexagon boundaries;
#'   \code{xbnds} - X bounds;
#'   \code{ybnds} - Y bounds;
#'
#' @export
#'
make_hexagons <- function(w, xbnds, ybnds, quiet = FALSE) {
  centroids <- make_centroids(w, xbnds, ybnds)

  if (!quiet) cat("Creating lattice of", nrow(centroids), "hexagons\n")

  h <- .make_hexagons_from_centroids(centroids, w)

  lattice <- list(
    hwidth = w,
    centroids = centroids,
    edges = h,
    xbnds = xbnds,
    ybnds = ybnds
  )

  class(lattice) <- c("hexlattice", "list")

  lattice
}


# Make a lattice of hexagons given a valid grid of centroids
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

  sf::st_sf(id = 1:length(polys), geometry = sf::st_sfc(polys))
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
#' hexagons <- make_hexagons(width, xbnds = c(700000, 750000), ybnds = c(6500000, 6600000))
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
