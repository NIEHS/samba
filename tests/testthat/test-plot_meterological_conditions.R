test_that("mytile creates a ggplot object", {
  df <- data.frame(
    time = seq(
      lubridate::ymd_h("2023-01-01 00"),
      lubridate::ymd_h("2023-01-31 23"),
      by = "hour"
    ),
    temperature = runif(744, min = -10, max = 30)
  )
  plot <- mytile(df, "temperature")
  expect_s3_class(plot, "ggplot")
  expect_true(
    "GeomTile" %in% sapply(
      plot$layers,
      function(layer) class(layer$geom)[1]
    )
  )
})

test_that("mytile_rain creates a ggplot object", {
  df <- data.frame(
    time = seq(
      lubridate::ymd_h("2023-01-01 00"),
      lubridate::ymd_h("2023-01-31 23"),
      by = "hour"
    ),
    rain = sample(c(0, 1), 744, replace = TRUE)
  )
  plot <- mytile_rain(df)
  expect_s3_class(plot, "ggplot")
  expect_true(
    "GeomTile" %in% sapply(
      plot$layers,
      function(layer) class(layer$geom)[1]
    )
  )
})
