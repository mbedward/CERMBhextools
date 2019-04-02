#' Quick plot of hexagon edges using ggplot
#'
#' @param h Lattice of hexagons; an object of class \code{hexlattice}
#'   as produced by \code{\link{make_hexagons}}.
#'
#' @importFrom ggplot2 ggplot geom_sf
#'
#' @return A ggplot object.
#'
#' @export
#'
plot.hexlattice <- function(h) {
  ggplot(data = h$shapes) +
    geom_sf(colour = "black", fill = NA)
}
