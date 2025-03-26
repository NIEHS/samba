
test_that("plot_windrose function works correctly", {
  # Create mock data
  data <- data.frame(
    spd = c(2, 5, 8, 12, 15, 18),
    dir = c(30, 60, 90, 120, 150, 180)
  )
  # Test with default parameters
  expect_silent({
    p <- plot_windrose(data = data, spd = data$spd, dir = data$dir)
  })
  expect_true(inherits(p, "ggplot"))
  # Test with custom parameters
  expect_silent({
    p <- plot_windrose(
      data = data,
      spd = data$spd,
      dir = data$dir,
      spdres = 5,
      dirres = 45,
      spdmin = 0,
      spdmax = 20,
      palette = "Reds"
    )
  })
  expect_true(inherits(p, "ggplot"))
  # Test with missing wind speed or direction
  data_missing <- data
  data_missing$spd[1] <- NA
  expect_silent({
    p <- plot_windrose(
      data = data_missing,
      spd = data_missing$spd,
      dir = data_missing$dir
    )
  })
  expect_true(inherits(p, "ggplot"))
})
