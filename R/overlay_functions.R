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
#' @return A two-column data frame with integer hexagon ID (\code{'id'}) and
#'   count of points (\code{'npoints'}).
#'
#' @importFrom dplyr %>%
#' @importFrom sf st_bbox st_coordinates st_crs
#'
#' @export
#'
count_points <- function(h, pts) {
  if (!inherits(h, "hexlattice")) {
    stop("Argument h should be an object of class 'hexlattice' ")
  }

  if (inherits(pts, "sf")) {
    hproj <- st_crs(h$edges)
    pproj <- st_crs(pts)
    na.proj <- (sf:::is.na.crs(hproj) || sf:::is.na.crs(cproj))

    ok <- na.proj | (hproj == pproj)
    if (!ok) stop("CRS for points differs from that for hexagons")

    pts <- as.matrix(st_coordinates(pts)[, 1:2])

  } else {
    pts <- as.matrix(pts[, 1:2])
  }

  xdim <- h$xbnds[2] - h$xbnds[1]
  ydim <- h$ybnds[2] - h$ybnds[1]

  binned <- hexbin::hexbin(pts,
                           xbins = xdim / h$hwidth,
                           shape = ydim / xdim,
                           xbnds = h$xbnds,
                           ybnds = h$ybnds)

  binned <- data.frame(id = binned@cell, npoints = binned@count)

  out <- h$edges %>%
    as.data.frame() %>%
    dplyr::select(id) %>%
    dplyr::left_join(binned, by = "id") %>%
    dplyr::mutate(npoints = ifelse(is.na(npoints), 0, npoints))

  out
}
