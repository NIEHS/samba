
test_that("map_uhi_obs function works correctly", {
  # Create mock data for predictions and observations
  pred_stat <- data.frame(
    lon = c(1, 2),
    lat = c(3, 4),
    time = as.POSIXct(
      c("2023-01-01 00:00:00", "2023-01-01 00:00:00"),
      tz = "UTC"
    ),
    pred_mean = c(25, 30)
  )
  obs <- data.frame(
    lon = c(1.5, 2.5),
    lat = c(3.5, 4.5),
    time = as.POSIXct(
      c("2023-01-01 00:00:00", "2023-01-01 00:00:00"),
      tz = "UTC"
    ),
    temp = c(26, 29),
    network = c("WU", "WU")
  )
  borders <- vect(
    data.frame(x = c(0, 0, 5, 5), y = c(0, 5, 5, 0)),
    geom = c("x", "y"),
    crs = "EPSG:4326"
  )
  time <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
  # Test map_uhi_obs
  expect_silent({
    p <- map_uhi_obs(pred_stat, obs, borders, time)
  })
  expect_true(inherits(p, "ggplot"))
})
test_that("map_uhi function works correctly", {
  # Create mock data for UHI raster and borders
  uhi <- rast(
    nrows = 10,
    ncols = 10,
    vals = runif(100, -5, 5),
    crs = "EPSG:4326"
  )
  terra::time(uhi) <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
  borders <- vect(
    data.frame(x = c(0, 0, 5, 5), y = c(0, 5, 5, 0)),
    geom = c("x", "y"),
    crs = "EPSG:4326"
  )
  ts <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
  # Test map_uhi
  expect_silent({
    p <- map_uhi(uhi, borders, ts)
  })
  expect_true(inherits(p, "ggplot"))
})
