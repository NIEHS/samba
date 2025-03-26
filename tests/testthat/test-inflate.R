
test_that("inflate function works correctly", {
  # Create a sample SpatVector
  coords <- data.frame(x = c(1, 2), y = c(3, 4))
  s <- terra::vect(coords, geom = c("x", "y"), crs = "EPSG:4326")
  # Define start and end times
  ts <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
  te <- as.POSIXct("2023-01-01 03:00:00", tz = "UTC")
  # Call the inflate function
  result <- inflate(s, ts, te)
  # Check the number of rows in the result
  expect_equal(nrow(result), 8) # 2 points * 4 time steps
  # Check if the time column exists and is correct
  expect_true("time" %in% names(result))
  expected_times <- rep(seq(from = ts, to = te, by = "hour"), each = 2)
  expect_equal(result$time, expected_times)
})