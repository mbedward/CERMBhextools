#' Calculate hexagon width given side length
#'
#' In this package, hexagons are always positioned to have two sides vertical.
#' Width is the distance between these two sides, and is equal to
#' \code{sqrt(3) * s} (side length).
#'
#' @param s Side length values for one or more hexagons.
#'
#' @return Width (X-dimension) value(s).
#'
#' @seealso \code{\link{hex_width2side}} for the reverse operation.
#'
#' @export
#'
hex_side2width <- function(s) {
  ifelse(s < 0, NA, ROOT3 * s)
}


#' Calculate hexagon side length given width
#'
#' In this package, hexagons are always positioned to have two sides vertical.
#' Width is the distance between these two sides, and is equal to
#' \code{sqrt(3) * s} (side length).
#'
#' @param w Width values for one or more hexagons.
#'
#' @return Side length value(s).
#'
#' @seealso \code{\link{hex_side2width}} for the reverse operation.
#'
#' @export
#'
hex_width2side <- function(w) {
  w / ROOT3
}


#' Calculate hexagon height given side length
#'
#' In this package, hexagons are always positioned to have two sides vertical.
#' Height is thus the distance between the two vertices on the vertical
#' mid-line, and is equal to \code{2 * s} (side length).
#'
#' @param s Side length values for one or more hexagons.
#'
#' @return Height (Y-dimension) values.
#'
#' @seealso \code{\link{hex_height2side}} for the reverse operation.
#'
#' @export
#'
hex_side2height <- function(s) {
  2*s
}


#' Calculate hexagon side length given height
#'
#' In this package, hexagons are always positioned to have two sides vertical.
#' Height is thus the distance between the two vertices on the vertical
#' mid-line, and is equal to \code{2 * s} (side length).
#'
#' @param h Height values for one or more hexagons.
#'
#' @return Side length values.
#'
#' @seealso \code{\link{hex_side2height}} for the reverse operation.
#'
#' @export
hex_height2side <- function(h) {
  h/2
}


#' Calculate hexagon side length given area
#'
#' A hexagon with area A has side length \code{s = sqrt[ 2*A / (3*sqrt(3)) ]}
#'
#' @param A Area values for one or more hexagons.
#'
#' @return Side length values.
#'
#' @seealso \code{\link{hex_side2area}} for the reverse operation.
#'
#' @export
#'
hex_area2side <- function(A) {
  sqrt( 2*A / X3ROOT3 )
}


#' Calculate hexagon area given side length
#'
#' A hexagon with side length \code{s} has area \code{A = 3 * sqrt(3) * s*s / 2}.
#'
#' @param s Side length values for one or more hexagons.
#'
#' @return Hexagon areas.
#'
#' @seealso \code{\link{hex_area2side}} for the reverse operation.
#'
#' @export
#'
hex_side2area <- function(s) {
  X3ROOT3 * s*s / 2
}


#' Calculate hexagon width for a given area
#'
#' In this package, hexagons are always positioned to have two sides vertical.
#' Width is the distance between these two sides.
#'
#' @param A Area values for one or more hexagons.
#'
#' @return Width (X-dimension) values.
#'
#' @seealso \code{\link{hex_width2area}} for the reverse operation.
#'
#' @export
#'
hex_area2width <- function(A) {
  sqrt(2*A) * INV_4THROOT3
}


#' Calculate hexagon area for a given width
#'
#' In this package, hexagons are always positioned to have two sides vertical.
#' Width is the distance between these two sides.
#'
#' @param w Width (X-dimension) values for one or more hexagons.
#'
#' @return Hexagon areas.
#'
#' @seealso \code{\link{hex_area2width}} for the reverse operation.
#'
#' @export
#'
hex_width2area <- function(w) {
  ROOT3 * w^2 / 2
}


