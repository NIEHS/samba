open_azmet <- function() {
  config <- list(
    ts = as.POSIXct("2023-07-01 07:00:00", tz = "UTC"),
    te = as.POSIXct("2023-08-01 06:00:00", tz = "UTC")
  )

  # install.packages('azmetr',
  # repos = c('https://cct-datascience.r-universe.dev',
  # 'https://cloud.r-project.org'))
  library(azmetr)

  # Select stations in phoenix shapefile
  data <- open_phoenix()
  az <- force(station_info) |>
    terra::vect(
      geom = c("longitude", "latitude"),
      crs = "epsg:4326",
      keepgeom = TRUE
    )
  area_rect_p <- terra::project(data$area_rect, az)
  az_phoe <- terra::intersect(area_rect_p, az)
  terra::plot(az)
  terra::plot(area_rect_p, col = "blue", add = TRUE)
  terra::plot(az_phoe, col = "lightblue", add = TRUE)

  az_hourly <- az_hourly(
    station_id = az_phoe$meta_station_id,
    start_date_time = with_tz(config$ts, "MST") - lubridate::second(1),
    end_date_time = with_tz(config$te, "MST")
  ) |>
    as.data.frame()
  head(az_hourly)
  ggplot(az_hourly) +
    geom_line(aes(
      x = date_datetime,
      y = temp_airC,
      group = meta_station_id,
      color = meta_station_id
    ))

  # Changing timestamp 23:59:59 to 00:00:00
  az_hourly[which(lubridate::hour(az_hourly$date_datetime) == 23), ]
  add_sec <- function(x) {
    idx <- which(lubridate::minute(x) == 59)
    lubridate::hour(x[idx]) <- 0
    lubridate::minute(x[idx]) <- 0
    lubridate::second(x[idx]) <- 0
    lubridate::day(x[idx]) <- lubridate::day(x[idx]) + 1
    return(x)
  }
  az_hourly$time <- add_sec(az_hourly$date_datetime)

  # Add lat lon
  az_hourly <- merge(az_hourly,
    az[, c(
      "meta_station_id",
      "latitude",
      "longitude",
      "elev_m"
    )],
    by = "meta_station_id"
  )

  # Format AZMET
  # Note from <https://azmet.arizona.edu/sites/default/files/2024-02/
  # programmatic-access-to-hourly-and-daily-azmet-data-with-a-web-api.pdf>:
  # "Each hourly summary of measured and derived variables represents
  # conditions over the ***previous 60 minutes***."

  azmet <- az_hourly[, c(
    "meta_station_name",
    "longitude",
    "latitude",
    "time",
    "temp_airC",
    "precip_total",
    "relative_humidity",
    "sol_rad_total",
    "wind_spd_max_mph",
    "wind_vector_dir"
  )]
  colnames(azmet) <- c(
    "site_id", "lon", "lat", "time", "temp",
    "rain", "rh", "sw", "wind_s", "wind_d"
  )
  azmet <- sf::st_as_sf(azmet,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

  return(azmet)
}
