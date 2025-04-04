#' @title Convert an \code{sf} to a \code{SpatRaster}
#' @description
#' Convert an \code{sf} object to a \code{SpatRaster} object. Returns a
#' \code{SpatRatser} with one layer for each time step in \code{x}.
#' @param x `st::sf`.
#' @param varname character. Variable to rasterize.
#' @param nx integer. Number of cells in the x direction.
#' @param ny integer. Number of cells in the y direction.
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
    values = NA_real_
  )
  newrast <- stars::st_rasterize(x[, varname],
    template = grid
  ) |>
    terra::rast()
  newrast
}

#' @title Convert prediction data frame to `terra::SpatRaster`
#' @description
#' Convert prediction data frame to `terra::SpatRaster`
#' @param pred data.frame. Output of SAMBA model.
#' @param varname character. Variable to rasterize
#' @param existing_raster `terra::SpatRaster`. Used as a template for
#' the layout.
#' @importFrom lubridate tz
#' @importFrom sf st_as_sf
#' @importFrom terra rast time
#' @return a `SpatRaster` object
#' @author Eva Marques
#' @export
rasterize_pred <- function(
  pred,
  varname = "pred_mean",
  existing_raster = NULL
) {
  stopifnot("varname missing or mispelled" = varname %in% colnames(pred))
  tz <- lubridate::tz(pred$time)
  nx <- length(unique(as.numeric(sprintf("%.3f", pred$lon))))
  ny <- length(unique(as.numeric(sprintf("%.3f", pred$lat))))
  period <- seq(min(pred$time), max(pred$time), by = "1 hour")
  predictions <- list()
  missing <- c()
  for (p in period) {
    p_str <- strftime(p, format = "%Y-%m-%d %H:%M:%S", tz = tz) |>
      as.POSIXct(tz = tz)
    cat(p_str, "\n")
    i <- which(period == p_str)
    sample <- pred[which(pred$time == p_str), ]
    if (nrow(sample) == 0) {
      message("no predictions at time ", p_str)
      missing <- c(missing, p_str)
    } else {
      sample <- sample |>
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
        sf_as_spatraster(varname, nx = nx, ny = ny)
      predictions[[i]] <- sample[[1]]
    }
  }
  predictions <- terra::rast(predictions)
  terra::time(predictions) <- setdiff(period, missing)
  if (is.null(existing_raster)) {
    predictions
  } else {
    terra::add(existing_raster) <- predictions
    existing_raster
  }
}
