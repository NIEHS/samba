test_that("map_pred_mean works as expected", {
  # Mock data
  pred <- data.frame(
    lon = rep(seq(-80, -79.5, by = 0.1), each = 6),
    lat = rep(seq(35, 35.5, by = 0.1), times = 6),
    time = lubridate::ymd_hms("2023-01-01 00:00:00"),
    pred_mean = runif(36, min = 0, max = 10)
  )
  # Mock borders
  borders <- terra::vect(
    matrix(c(-80, 35, -79.5, 35, -79.5, 35.5, -80, 35.5, -80, 35), 
           ncol = 2, byrow = TRUE),
    type = "polygons", crs = "EPSG:4326"
  )
  # Call the function
  plots <- map_pred_mean(pred, borders, tz = "America/New_York")
  # Assertions
  expect_type(plots, "list")
  expect_true(all(sapply(plots, inherits, "ggplot")))
  expect_equal(length(plots), 1) # One plot per time period
})
