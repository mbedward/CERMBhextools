#' Convert cartesian coordinates to hexagonal coordinates
#'
#' @param xy A matrix or data frame of point locations. X and Y ordinates are
#'   assumed to be in the first two columns.
#'
#' @param h The reference lattice: an object of class \code{hexlattice}.
#'
#' @return A matrix of hexagon (axial) coordinates.
#'
#' @export
#'
xy2hex <- function(xy, h) {
  stopifnot(inherits(h, "hexlattice"))

  origin <- c( min(h$shapes$xc), min(h$shapes$yc) )
  .do_xy2hex(xy, origin = origin, sidelen = h$side)
}


.do_xy2hex <- function(xy, origin, sidelen) {
  xy[,1] <- xy[,1] - origin[1]
  xy[,2] <- xy[,2] - origin[2]

  q <- xy[,1] * ROOT3DIV3 - xy[,2] / 3
  r <- 2 * xy[,2] / 3

  out <- round(
    cbind(q = q / sidelen,
          r = r / sidelen))

  rownames(out) <- NULL
  out
}


#' Convert hexagonal coordinates to cartesian coordinates
#'
#' @param xy A matrix or data frame of hexagon locations. Q (hex column) and
#'   R (hex row) ordinates are assumed to be in the first two columns.
#'
#' @param h The reference lattice: an object of class \code{hexlattice}.
#'
#' @return A matrix of cartesian coordinates.
#'
#' @export
#'
hex2xy <- function(qr, h) {
  stopifnot(inherits(h, "hexlattice"))

  origin <- c( min(h$shapes$xc), min(h$shapes$yc) )
  .do_hex2xy(qr, origin, h$side)
}


.do_hex2xy <- function(qr, origin, sidelen) {

  # X and Y offsets relative to lower left hexagon centroid
  # (ie. hex coords q=0, r=0)
  x <- ROOT3 * (qr[,1] + qr[,2] / 2.0)
  y <- 3 * qr[,2] / 2

  out <- cbind(x = origin[1] + x * sidelen,
               y = origin[2] + y * sidelen)

  rownames(out) <- NULL
  out
}
