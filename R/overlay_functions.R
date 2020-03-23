#' Count points within each hexagon in a lattice
#'
#' Given a lattice of hexagons and a set of points, this function counts how
#' many points lie within each hexagon. The lattice should be a spatial data
#' frame (\code{sf} object) while the point locations can be a spatial data
#' frame or a two-column matrix or plain data frame. If points are provided as a
#' spatial data frame, the function checks whether the coordinate reference
#' system matches that of the hexagons. If either layer does not have a
#' reference system defined this check is skipped, ie. point and hexagon
#' coordinates are assumed to be comparable.
#'
#' @param h Lattice of hexagons; an object of class \code{hexlattice}
#'   as produced by \code{\link{make_hexagons}}.
#'
#' @param pts Point locations. This can either be a spatial data frame
#'   (\code{sf} object) or a two-column matrix or data frame of point
#'   coordinates.
#'
#' @param outcol The name of the column of point counts to add to the
#'   \code{hexlattice} object's data frame. If there is an existing column in the data
#'   frame with this name, the name of the new column will be adjusted using
#'   \code{\link[base]{make.names}}.
#'
#' @return A copy of the input \code{hexlattice} object with point counts added
#'   to its data frame.
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
count_points <- function(h, pts, outcol = "npoints") {
  if (!inherits(h, "hexlattice")) {
    stop("Argument h should be an object of class 'hexlattice' ")
  }

  if (inherits(pts, "sf")) {
    if (has_geometries(h)) {
      hproj <- sf::st_crs(h$shapes)
      pproj <- sf::st_crs(pts)
      na.proj <- (sf:::is.na.crs(hproj) || sf:::is.na.crs(pproj))

      ok <- na.proj | (hproj == pproj)
      if (!ok) stop("CRS for points differs from that for hexagons")
    }

    pts <- as.matrix(sf::st_coordinates(pts)[, 1:2])

  } else {
    pts <- as.matrix(pts[, 1:2])
  }


  outcol.requested <- outcol
  n <- ncol(h$shapes)
  outcol <- make.names(c(colnames(h$shapes), outcol.requested), unique = TRUE)[n+1]
  if (outcol != outcol.requested) {
    warning("Adjusted column name to ", outcol)
  }

  dat <- xy2hex(pts, h) %>%
    as.data.frame() %>%
    dplyr::group_by(q, r) %>%
    dplyr::summarize(NPOINTS__ = n()) %>%
    dplyr::ungroup()


  h$shapes <- h$shapes %>%
    dplyr::left_join(dat, by = c("q", "r")) %>%
    dplyr::mutate(NPOINTS__ = ifelse(is.na(NPOINTS__), 0, NPOINTS__))

  i <- which(colnames(h$shapes) == "NPOINTS__")
  colnames(h$shapes)[i] <- outcol

  h
}
