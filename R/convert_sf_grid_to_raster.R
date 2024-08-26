#' Convert an \code{sf} to a \code{SpatRaster}
#' @description
#' Convert an \code{sf} object to a \code{SpatRaster} object. Returns a
#' \code{SpatRatser} with one layer for each time step in \code{x}.
#' @param x an `sf` object
#' @param varname variable to rasterize
#' @param nx number of cells in the x direction
#' @param ny number of cells in the y direction
#' @return a `SpatRaster` object
#' @importFrom stars st_as_stars st_rasterize
#' @importFrom sf st_bbox
#' @importFrom terra rast
#' @author Eva Marques
#' @export
sf_as_spatraster <- function(x, varname, nx, ny) {
  stopifnot("varname missing or mispelled" = varname %in% colnames(x))
  grid <- stars::st_as_stars(sf::st_bbox(x),
                             nx = nx,
                             ny = ny,
                             values = NA_real_)
  newrast <- stars::st_rasterize(x[, varname],
                                 template = grid) |>
    terra::rast()
  return(newrast)
}
