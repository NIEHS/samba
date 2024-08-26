#' Inflate the prediction grid raster to a spatiotemporal datatable
#' @description Inflate the prediction grid raster to a spatiotemporal datatable
#' with dates
#' @param s a terra::SpatVector of points (eg: points of a 100km * 100km grid)
#' @param yyyy a numeric year
#' @param mm a numeric month
#' @return a sf object with a time column and duplicated spatial covariates
#' along the time dimension
#' @importFrom sf st_as_sf
inflate <- function(s, yyyy, mm) {
  # create a vector of hourly timestamps for the given mm and yyyy
  d_start <- as.POSIXct(
    paste0(as.character(yyyy), "-", as.character(mm), "-01 00:00:00"),
    tz = "UTC"
  )
  dates <- seq(
    from = d_start,
    to = d_start + months(1) - lubridate::hours(1),
    by = "hour"
  )
  # create a spatiotemporal datatable
  st <- sf::st_as_sf(s)
  n <- nrow(st)
  st <- st[rep(seq_len(nrow(st)), times = length(dates)), ]
  st$time <- rep(dates, each = n)
  return(st)
}
