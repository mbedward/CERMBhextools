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


#' Sample values from a raster at hexagon centroid locations
#'
#' Given a lattice of hexagons and a raster layer, this function samples the
#' raster value at each hexagon centroid. A check is done that the hexagon
#' lattice and the raster have the same coordinate reference system. If either
#' does not have a reference system defined this check is skipped, i.e. point
#' and hexagon coordinates are assumed to be comparable.
#'
#' @param h Lattice of hexagons; an object of class \code{hexlattice}
#'   as produced by \code{\link{make_hexagons}}.
#'
#' @param r A raster object, either a single layer (\code{RasterLayer}) or
#'   a multi-layer raster (\code{RasterStack}, \code{RasterBrick}).
#'
#' @param interp If \code{TRUE} (default), raster values are sampled using
#'   bilinear interpolation; if \code{FALSE} no interpolation is performed.
#'
#' @param outcol The name of the column(s) of raster values to add to the
#'   \code{hexlattice} object's data frame. For multi-layer rasters, this is the
#'   root name, and columns will be named \code{outcol1, outcol2, ...}. The
#'   default (\code{NULL}) means to use raster layer names as column names if
#'   these are defined, otherwise use the name 'layer'. If any names clash with
#'   existing columns in the data frame, the name of the new columns will be
#'   adjusted using \code{\link[base]{make.names}}.
#'
#' @return A copy of the input \code{hexlattice} object with raster values added
#'   to its data frame.
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
sample_raster <- function(h, r, interp = TRUE, outcol = NULL) {
  if (!inherits(h, "hexlattice")) {
    stop("Argument h should be an object of class 'hexlattice' ")
  }

  if (!inherits(r, "Raster")) {
    stop("Argument r should be a Raster object")
  }

  if (has_geometries(h)) {
    hcrs <- sf::st_crs(h)
    rcrs <- raster::crs(r)

    if (!is.na(hcrs) && !is.na(rcrs)) {
      ok <- raster::compareCRS(hcrs$proj4string, rcrs)
      if (!ok) {
        stop("Lattice and raster appear to have different coordinate reference systems")
      }
    }
  }

  xy <- h$shapes %>%
    as.data.frame() %>%
    dplyr::select(xc, yc) %>%
    as.matrix()

  method <- ifelse(interp, "bilinear", "simple")
  vals <- raster::extract(r, xy, method)

  layer.names <- names(r)
  if (!is.matrix(vals)) {
    # r is a single layer raster
    vals <- matrix(vals, ncol = 1)
  }

  if (is.null(outcol)) {
    colnames(vals) <- names(r)
  } else {
    colnames(vals) <- paste0(outcol[1], 1:ncol(vals))
  }

  n <- ncol(h$shapes)
  outcols <- make.names(c(colnames(h$shapes), colnames(vals)), unique = TRUE)[-(1:n)]
  colnames(vals) <- outcols

  h$shapes <- cbind(h$shapes, vals)

  h
}

