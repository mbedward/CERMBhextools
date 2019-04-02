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
#' @param output One of 'shapes', 'simple' or 'clone' (may be abbreviated). If
#'   'shapes' (the default), a spatial data frame is returned that is a copy of
#'   that in the input \code{hexlattice} object with an 'npoints' column added.
#'   If 'simple', a plain data frame is returned with columns for hexagon ID
#'   (integer) and number of points. If 'clone' a copy of the input
#'   \code{hexlattice} object is returned, with an 'npoints' column added to its
#'   'shapes' data frame.
#'
#' @return Depending on the 'output' argument: either a spatial data frame
#'   (output = 'shapes'), a plain data frame of hexagon IDs and point counts
#'   (output = 'simple'), or a copy of the input \code{hexlattice} object with
#'   point counts added to its 'shapes' spatial data frame.
#'
#' @importFrom dplyr %>% group_by left_join mutate summarize ungroup
#' @importFrom sf st_bbox st_coordinates st_crs
#'
#' @export
#'
count_points <- function(h, pts, output = c("shapes", "simple", "clone")) {
  if (!inherits(h, "hexlattice")) {
    stop("Argument h should be an object of class 'hexlattice' ")
  }

  output <- match.arg(output)

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

  dat <- xy2hex(pts, h) %>%
    as.data.frame() %>%
    group_by(q, r) %>%
    summarize(npoints = n()) %>%
    ungroup()

  if (output == "simple") {
    out <- h$shapes %>%
      as.data.frame() %>%
      dplyr::select(id, q, r) %>%
      left_join(dat, by = c("q", "r")) %>%
      dplyr::select(id, npoints) %>%
      mutate(npoints = ifelse(is.na(npoints), 0, npoints))

  } else { # shapes or clone
    out <- h$shapes
    if ("npoints" %in% colnames(out)) out$npoints <- NULL
    out <- left_join(out, dat, by = c("q", "r")) %>%
      mutate(npoints = ifelse(is.na(npoints), 0, npoints))
  }

  if (output == "clone") {
    h$shapes <- out
    h
  } else {
    out
  }
}
