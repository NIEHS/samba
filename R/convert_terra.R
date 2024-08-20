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
#' @import sftime
#' @import stars
#' @seealso [terra::rast]
#' @author Eva Marques
#' @keywords spacetime
#' @export
sftime_as_spatraster <- function(x, varname) {
  stopifnot("varname missing or mispelled" = varname %in% colnames(x))
  dates <- unique(sftime::st_time(x))
  layers <- list()
  for (d in dates) {
    newrast <- stars::st_rasterize(x[which(st_time(x) == d), varname]) |>
      terra::rast()
    layers[[d]] <- newrast
  }
  return(terra::rast(layers))
}


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
#' @import sftime
#' @import stars
#' @seealso [terra::rast]
#' @author Eva Marques
#' @keywords spacetime
#' @export
sf_as_spatraster <- function(x, varname, nx, ny) {
  stopifnot("varname missing or mispelled" = varname %in% colnames(x))
  layers <- list()
  grid <- stars::st_as_stars(sf::st_bbox(x),
                             nx = nx,
                             ny = ny,
                             values = NA_real_)
  newrast <- stars::st_rasterize(x[, varname],
                                 template = grid) |>
      terra::rast()
  return(newrast)
}
