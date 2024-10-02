run_triangle_cv <- function(model,
                            in_path,
                            out_path,
                            t_start,
                            t_end,
                            cv_type) {
  stopifnot("model should be 1, 2 or 3" = model %in% c(1, 2, 3))
  stopifnot("cv_type should be sp or imp" = cv_type %in% c("sp", "imp"))
  # Create an output repo for this casestudy
  if (!dir.exists(paste0(out_path))) {
    dir.create(paste0(out_path))
  }
  # Area of interest
  cities_shp <- paste0(in_path, "500Cities_City_11082016/CityBoundaries.shp")
  cities <- terra::vect(cities_shp)
  cities <- cities[which(cities$NAME %in% c("Raleigh", "Durham", "Cary")), ]
  triangle <- terra::ext(cities) |>
    terra::as.polygons(crs = terra::crs(cities)) |>
    terra::buffer(10000, joinstyle = "mitre")
  data <- readRDS(paste0(
    in_path,
    "cws/cws_covar_nc_2021-07.rds"
  ))
  # Select observations in the Triangle area
  polygon <- terra::ext(triangle) |>
    terra::as.polygons(crs = terra::crs(triangle)) |>
    terra::buffer(10000, joinstyle = "mitre")
  data_samp <- terra::vect(data,
    geom = c("lon", "lat"),
    crs = "epsg:4326"
  ) |>
    terra::project(polygon) |>
    terra::intersect(polygon) |>
    terra::project("epsg:4326") |>
    as.data.frame(geom = "xy") |>
    dplyr::rename(lon = "x", lat = "y")
  # Cross validation
  data_cv <- unique(data_samp[, c("lon", "lat", "imp")]) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  if (cv_type == "sp") {
    folds <- spatialsample::spatial_clustering_cv(data_cv, v = 10)
  } else if (cv_type == "imp") {
    # create classes of imp
    cut_quant <- function(x, n_quant = 10) {
      cut(x,
        breaks = c(quantile(x, probs = seq(0, 1, length.out = n_quant + 1))),
        labels = 1:n_quant, include.lowest = TRUE
      )
    }
    data_cv$imp_class <- cut_quant(data_cv$imp, n_quant = 5)
    folds <- spatialsample::spatial_leave_location_out_cv(data_cv,
      group = imp_class,
      v = 5
    )
  }
  for (k in 1:nrow(folds)) {
    train <- spatialsample::analysis(folds$splits[[k]])
    train <- cbind(train, sf::st_coordinates(train)) |>
      dplyr::rename(lon = X, lat = Y)
    train$geometry <- NULL
    test <- spatialsample::assessment(folds$splits[[k]])
    test <- cbind(test, st_coordinates(test)) |>
      dplyr::rename(lon = X, lat = Y)
    test$geometry <- NULL
    data_samp_k <- merge(train[, c("lon", "lat")],
      data_samp,
      all.y = FALSE,
      by = c("lon", "lat")
    )
    pred_samp_k <- merge(test[, c("lon", "lat")],
      data_samp,
      all.y = FALSE,
      by = c("lon", "lat")
    )
    # Inference
    start_time <- Sys.time()
    period <- seq(t_start, t_end, by = "6 days")
    for (i in 1:length(period)) {
      ts_str <- strftime(period[[i]],
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ) |>
        as.POSIXct(tz = "UTC")
      te_str <- strftime(period[[i]] + lubridate::days(6),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ) |>
        as.POSIXct(tz = "UTC")
      if (model == 1) {
        out <- inference_mod1(data_samp_k,
          pred_samp_k,
          polygon = triangle,
          ts_str,
          te_str,
          verbose = FALSE
        )
        saveRDS(
          object = out,
          file = paste0(
            out_path,
            cv_type,
            "_cv_fold",
            k,
            "_model1_inference_",
            format(min(out$pred$time), "%Y%m%d%H"),
            "_",
            format(max(out$pred$time), "%Y%m%d%H"),
            ".rds"
          )
        )
      } else if (model == 2) {
        out <- inference_mod2(data_samp_k,
          pred_samp_k,
          polygon = triangle,
          ts_str,
          te_str,
          verbose = FALSE
        )
        saveRDS(
          object = out,
          file = paste0(
            out_path,
            cv_type,
            "_cv_fold",
            k,
            "_model2_inference_",
            format(min(out$pred$time), "%Y%m%d%H"),
            "_",
            format(max(out$pred$time), "%Y%m%d%H"),
            ".rds"
          )
        )
      } else if (model == 3) {
        out <- inference_mod3(data_samp_k,
          pred_samp_k,
          polygon = triangle,
          ts_str,
          te_str,
          verbose = FALSE
        )
        saveRDS(
          object = out,
          file = paste0(
            out_path,
            cv_type,
            "_cv_fold",
            k,
            "_model3_inference_",
            format(min(out$pred$time), "%Y%m%d%H"),
            "_",
            format(max(out$pred$time), "%Y%m%d%H"),
            ".rds"
          )
        )
      }
    }
  }
  end_time <- Sys.time()
  return(list(exec_time = end_time - start_time))
}

plot_error_oob <- function(pred) {
  p <- ggplot(pred) +
    geom_point(aes(x = lubridate::hour(pred$time), y = pred_mean - temp),
               alpha = .5) +
    xlab("UTC (EST + 4)") +
    ylab("pred - obs") +
    theme(
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
  return(p)
}
