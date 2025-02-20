#' Inflate the prediction grid raster to a spatiotemporal SpatVector
#' @description Inflate the prediction grid raster to a spatiotemporal SpatVector
#' with datetime in "time" column
#' @param s a terra::SpatVector of points (eg: points of a 100km * 100km grid)
#' @param ts starting time
#' @param te ending time
#' @return a spatVector object with a time column and duplicated spatial covariates
#' along the time dimension
#' @importFrom sf st_as_sf
inflate <- function(s, ts, te) {
  dates <- seq(from = ts, to = te, by = "hour")
  dates <- lubridate::with_tz(tz = "UTC")
  # create a spatiotemporal datatable
  st <- sf::st_as_sf(s)
  n <- nrow(st)
  st <- st[rep(seq_len(nrow(st)), times = length(dates)), ]
  st$time <- rep(dates, each = n)
  st <- st |>
    terra::vect()
  return(st)
}