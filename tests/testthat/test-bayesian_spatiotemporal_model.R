test_that("inference function works as expected", {
  data <- "../testdata/cws_fake.csv" |>
    testthat::test_path() |>
    read.csv()
  pred <- "../testdata/pred_fake.csv" |>
    testthat::test_path() |>
    read.csv()
  data$time <- as.POSIXct(data$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  pred$time <- as.POSIXct(pred$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  polygon <- "../testdata/area_rect.shp" |>
    testthat::test_path() |>
    terra::vect()
  # Run inference
  result <- inference(
    data = data,
    pred = pred,
    polygon = polygon,
    ts = lubridate::ymd_hms("1000-07-28 00:00:00"),
    te = lubridate::ymd_hms("1000-07-28 02:00:00"),
    verbose = TRUE,
    debug = FALSE
  )
  # Assertions
  expect_type(result, "list")
  expect_named(result, c("pred", "mod", "info"))
  expect_s3_class(result$pred, "data.frame")
  expect_true(
    all(
      c(
        "pred_mean",
        "pred_ll",
        "pred_ul",
        "pred_sd"
      ) %in% colnames(result$pred)
    )
  )
  expect_s3_class(result$mod, "inla")
  expect_s3_class(result$info, "data.frame")
  expect_true(
    all(
      c(
        "mesh_max_edge",
        "mesh_cutoff",
        "pcrange_thresh",
        "pcrange_proba",
        "pcsigma_thresh",
        "pcsigma_proba",
        "pccor1_thresh",
        "pccor1_proba",
        "s2n_data_prior",
        "s2x_data_prior",
        "prior_a2",
        "prior_b2",
        "int_mean",
        "era5_t2m_mean",
        "era5_rh_mean",
        "era5_tp_mean",
        "era5_u10_mean",
        "era5_v10_mean",
        "era5_tcc_mean",
        "local_hour19:elev_mean",
        "local_hour20:elev_mean",
        "local_hour21:elev_mean",
        "local_hour19:fch_mean",
        "local_hour20:fch_mean",
        "local_hour21:fch_mean",
        "local_hour19:imp_mean",
        "local_hour20:imp_mean",
        "local_hour21:imp_mean",
        "int_sd",
        "era5_t2m_sd",
        "era5_rh_sd",
        "era5_tp_sd",
        "era5_u10_sd",
        "era5_v10_sd",
        "era5_tcc_sd",
        "local_hour19:elev_sd",
        "local_hour20:elev_sd",
        "local_hour21:elev_sd",
        "local_hour19:fch_sd",
        "local_hour20:fch_sd",
        "local_hour21:fch_sd",
        "local_hour19:imp_sd",
        "local_hour20:imp_sd",
        "local_hour21:imp_sd",
        "prec_gaussian_obs_mean",
        "range_gaussian_obs_mean",
        "stdev_s_mean",
        "grouprho_s_mean",
        "prec_gaussian_obs_sd",
        "range_gaussian_obs_sd",
        "stdev_s_sd",
        "grouprho_s_sd"
      ) %in% colnames(result$info)
    )
  )
})
