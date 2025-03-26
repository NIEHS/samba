
test_that("check_input function works correctly", {
  # Create mock data for input list
  cws_raw <- terra::vect(
    data.frame(
      lon = c(1, 2),
      lat = c(3, 4),
      time = Sys.time(),
      temp = c(20, 25),
      site_id = c("A", "B")
    ),
    geom = c("lon", "lat"),
    keepgeom = TRUE,
    crs = "EPSG:4326"
  )
  elev <- terra::rast(nrows = 10, ncols = 10, vals = runif(100))
  fch <- terra::rast(nrows = 10, ncols = 10, vals = runif(100))
  imp <- terra::rast(nrows = 10, ncols = 10, vals = runif(100))
  era5_instant <- terra::rast(nrows = 10, ncols = 10, vals = runif(100))
  era5_accum <- terra::rast(nrows = 10, ncols = 10, vals = runif(100))
  area_shp <- terra::vect(
    data.frame(
      x = c(1, 2),
      y = c(3, 4)
    ),
    geom = c("x", "y"),
    crs = "EPSG:4326"
  )
  ts <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
  te <- as.POSIXct("2023-01-02 00:00:00", tz = "UTC")
  # Create input list
  input <- list(
    cws_raw = cws_raw,
    elev = elev,
    fch = fch,
    imp = imp,
    era5_instant = era5_instant,
    era5_accum = era5_accum,
    area_shp = area_shp,
    ts = ts,
    te = te
  )
  # Test valid input
  expect_no_error(check_input(input))
  # Test missing element in input list
  input_missing <- input
  input_missing$cws_raw <- NULL
  expect_error(check_input(input_missing), "missing data in input")
  # Test invalid time range
  input_invalid_time <- input
  input_invalid_time$ts <- as.POSIXct("2023-01-02 00:00:00", tz = "UTC")
  input_invalid_time$te <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
  expect_error(
    check_input(input_invalid_time),
    "ts is more recent than te"
  )
  # Test invalid cws_raw format
  input_invalid_cws_raw <- input
  input_invalid_cws_raw$cws_raw <- data.frame(
    lon = c(1, 2),
    lat = c(3, 4)
  )
  expect_error(
    check_input(input_invalid_cws_raw)
  )
})