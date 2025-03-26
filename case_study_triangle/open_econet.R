#' @title Open Econet data
#' @description Open Econet data from the Triangle area
#' @param f_path character path to the Econet data
#' @return a sf object with the Econet data
#' @import data.table
open_econet <- function(f_path) {
  # REED Latitude 35.80715°N Longitude -78.74408°E
  # LAKE Latitude 35.72816°N Longitude -78.67981°E
  # DURH Latitude 36.02896°N Longitude -78.85851°E
  # BAHA Latitude 36.17493°N Longitude -78.80865°E
  # DKFN7 Latitude 35.971011°N Longitude -79.093333°E
  # CHAP Latitude 35.934°N Longitude -79.064°E
  # CLA2 Latitude 35.59159°N Longitude -78.45892°E
  # CLAY Latitude 35.66974°N Longitude -78.4926°E
  # KRDU Latitude 35.892°N Longitude -78.782°E
  # create a list of the stations with latitude, longitude and file path
  stations <- list(
    REED = c(35.80715, -78.74408, "REED"),
    LAKE = c(35.72816, -78.67981, "LAKE"),
    DURH = c(36.02896, -78.85851, "DURH"),
    BAHA = c(36.17493, -78.80865, "BAHA"),
    DKFN7 = c(35.971011, -79.093333, "DKFN7"),
    CHAP = c(35.934, -79.064, "CHAP"),
    CLA2 = c(35.59159, -78.45892, "CLA2"),
    CLAY = c(35.66974, -78.4926, "CLAY")
  )

  eco_sw <- data.table::data.table()
  for (i in seq_along(stations)) {
    file_i <- list.files(path = f_path,
                         pattern = paste0(stations[[i]][3], ".csv"),
                         full.names = TRUE)
    length_file <- length(readLines(file_i))
    eco_i <- data.table::fread(file = file_i,
                               sep = ",",
                               fill = TRUE,
                               nrows = length_file - 4)
    #colnames(eco_i) <- c("time", "rh", "temp", "rain", "wind_s", "wind_d")
    colnames(eco_i) <- c("time", "temp", "rain", "rh", "sw")
    eco_i$station <- stations[[i]][3]
    eco_i$lat <- stations[[i]][1]
    eco_i$lon <- stations[[i]][2]
    eco_sw <- rbind(eco_sw, eco_i)
  }
  # change to all char variable to numeric in eco except for station column
  num_cols <- c("temp", "rain", "rh", "sw", "lat", "lon")
  eco_sw[ , (num_cols) := lapply(.SD, "as.numeric"), .SDcols = num_cols]
  eco_sw <- na.omit(eco_sw)

  # create a list of the stations with latitude, longitude and file path
  stations <- list(
    REED = c(35.80715, -78.74408, "REED"),
    LAKE = c(35.72816, -78.67981, "LAKE"),
    DURH = c(36.02896, -78.85851, "DURH"),
    CHAP = c(35.934, -79.064, "CHAP"),
    CLA2 = c(35.59159, -78.45892, "CLA2"),
    CLAY = c(35.66974, -78.4926, "CLAY"),
    KRDU = c(35.892, -78.782, "KRDU")
  )
  eco_wind <- data.table::data.table()
  for (i in seq_along(stations)) {
    file_i <- list.files(path = f_path,
                         pattern = paste0(stations[[i]][3], "_wind.csv"),
                         full.names = TRUE)
    length_file <- length(readLines(file_i))
    eco_i <- data.table::fread(file = file_i,
                               sep = ",",
                               fill = TRUE,
                               nrows = length_file - 4)
    colnames(eco_i) <- c("time", "rh", "temp", "rain", "wind_s", "wind_d")
    eco_i$station <- stations[[i]][3]
    eco_i$lat <- stations[[i]][1]
    eco_i$lon <- stations[[i]][2]
    eco_wind <- rbind(eco_wind, eco_i)
  }
  # change to all char variable to numeric in eco except for station column
  num_cols <- c("temp", "rain", "rh", "wind_s", "wind_d", "lat", "lon")
  eco_wind[ , (num_cols) := lapply(.SD, "as.numeric"), .SDcols = num_cols]
  eco_wind <- na.omit(eco_wind)
  eco <- merge(eco_sw,
               eco_wind,
               c("station", "time", "rh", "temp", "rain", "lat", "lon"),
               all = TRUE) 

  # create a list of the stations with latitude, longitude and file path
  stations <- list(
    REED = c(35.80715, -78.74408, "REED"),
    LAKE = c(35.72816, -78.67981, "LAKE"),
    DURH = c(36.02896, -78.85851, "DURH"),
    CHAP = c(35.934, -79.064, "CHAP"),
    CLA2 = c(35.59159, -78.45892, "CLA2"),
    CLAY = c(35.66974, -78.4926, "CLAY"),
    BAHA = c(36.17493, -78.80865, "BAHA")
  )
  eco_soilm <- data.table::data.table()
  for (i in seq_along(stations)) {
    file_i <- list.files(path = f_path,
                         pattern = paste0(stations[[i]][3], "_soilm.csv"),
                         full.names = TRUE)
    length_file <- length(readLines(file_i))
    eco_i <- data.table::fread(file = file_i,
                               sep = ",",
                               fill = TRUE,
                               nrows = length_file - 4)
    colnames(eco_i) <- c("time", "soilm", "sw")
    eco_i$station <- stations[[i]][3]
    eco_i$lat <- stations[[i]][1]
    eco_i$lon <- stations[[i]][2]
    eco_soilm <- rbind(eco_soilm, eco_i)
  }
  # change to all char variable to numeric in eco except for station column
  num_cols <- c("soilm", "sw", "lat", "lon")
  eco_soilm[, (num_cols) := lapply(.SD, "as.numeric"), .SDcols = num_cols]
  eco_soilm$sw <- NULL 
  eco_soilm <- na.omit(eco_soilm)
  eco <- merge(eco,
               eco_soilm,
               c("station", "time", "lat", "lon"),
               all = TRUE) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
    dplyr::rename(site_id = station)

  lubridate::tz(eco$time) <- "America/New_York"
  eco$time <- lubridate::with_tz(eco$time, tzone = "UTC")
  return(eco)
}

