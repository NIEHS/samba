#' Convert an \code{sftime} to a \code{SpatRaster}
#' @description
#' Convert an \code{sftime} object to a \code{SpatRaster} object. Returns a
#' \code{SpatRatser} with one layer for each time step in \code{x}.
#' @note
#' Running \code{sftime_as_spatraster} can take a long time if \code{x} is not
#' spatially structured.
#' @param x an `sftime` object
#' @param varname variable to rasterize
#' @return a `SpatRaster` object
#' @importFrom sftime st_time
#' @importFrom stars st_rasterize
#' @importFrom terra rast
#' @author Eva Marques
#' @export
sftime_as_spatraster <- function(x, varname) {
  stopifnot("varname missing or mispelled" = varname %in% colnames(x))
  dates <- unique(sftime::st_time(x))
  layers <- list()
  for (d in dates) {
    newrast <- stars::st_rasterize(x[which(sftime::st_time(x) == d),
                                     varname]) |>
      terra::rast()
    layers[[d]] <- newrast
  }
  return(terra::rast(layers))
}


#' Convert an \code{sftime} to a \code{SpatRaster}
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
