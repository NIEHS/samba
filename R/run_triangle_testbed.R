run_triangle <- function(model, in_path, out_path, t_start, t_end) {
  stopifnot("model should be 1, 2 or 3" = model %in% c(1, 2, 3))
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
  terra::plot(triangle)
  terra::plot(cities, col = "red", add = TRUE)
  # Open prediction grids covering this polygon
  # find_cells(triangle)
  # pred1 <- readRDS("../input/us_grids_covar/202107/grid_800.rds")
  # pred1 <- sf::st_transform(pred1, crs = 4326)
  # pred1$lon <- sf::st_coordinates(pred1)[, 1]
  # pred1$lat <- sf::st_coordinates(pred1)[, 2]
  # pred1 <- as.data.frame(pred1, xy = T)
  #
  # pred2 <- readRDS("../input/us_grids_covar/202107/grid_801.rds")
  # pred2 <- sf::st_transform(pred2, crs = 4326)
  # pred2$lon <- sf::st_coordinates(pred2)[, 1]
  # pred2$lat <- sf::st_coordinates(pred2)[, 2]
  # pred2 <- as.data.frame(pred2, xy = T)
  #
  # pred1$frac_5_500m <- 0
  # pred <- rbind(pred1, pred2)
  # saveRDS(pred, "../output/case_study_triangle/pred_grid.rds")
  pred <- readRDS(paste0(
    in_path,
    "pred_grid_800_801.rds"
  ))
  data <- readRDS(paste0(
    in_path,
    "cws/cws_covar_nc_2021-07.rds"
  ))
  # Select observations in the Triangle area
  polygon <- terra::ext(triangle) |>
    terra::as.polygons(crs = terra::crs(triangle)) |>
    terra::buffer(10000, joinstyle = "mitre")
  data_poly <- terra::vect(data,
    geom = c("lon", "lat"),
    crs = "epsg:4326"
  ) |>
    terra::project(polygon) |>
    terra::intersect(polygon) |>
    terra::project("epsg:4326") |>
    as.data.frame(geom = "xy") |>
    dplyr::rename(lon = "x", lat = "y")
  # Inference
  start_time <- Sys.time()
  period <- seq(t_start, t_end, by = "6 days")
  for (i in 1:(length(period))) {
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
      out <- inference_mod1(data_poly,
        pred,
        polygon = triangle,
        ts_str,
        te_str,
        verbose = FALSE
      )
      saveRDS(
        object = out$mod,
        file = paste0(
          out_path,
          "model1_inference_",
          format(min(out$pred$time), "%Y%m%d%H"),
          "_",
          format(max(out$pred$time), "%Y%m%d%H"),
          ".rds"
        )
      )
      data.table::fwrite(
        object = out$pred,
        file = paste0(
          out_path,
          "model1_inference_pred_",
          format(min(out$pred$time), "%Y%m%d%H"),
          "_",
          format(max(out$pred$time), "%Y%m%d%H"),
          ".csv"
        )
      )
    } else if (model == 2) {
      out <- inference_mod2(data_poly,
        pred,
        polygon = triangle,
        ts_str,
        te_str,
        verbose = FALSE
      )
      saveRDS(
        object = out$mod,
        file = paste0(
          out_path,
          "model2_inference_",
          format(min(out$pred$time), "%Y%m%d%H"),
          "_",
          format(max(out$pred$time), "%Y%m%d%H"),
          ".rds"
        )
      )
      data.table::fwrite(
        object = out$pred,
        file = paste0(
          out_path,
          "model2_inference_pred_",
          format(min(out$pred$time), "%Y%m%d%H"),
          "_",
          format(max(out$pred$time), "%Y%m%d%H"),
          ".csv"
        )
      )
    } else if (model == 3) {
      out <- inference_mod3(data_poly,
        pred,
        polygon = triangle,
        ts_str,
        te_str,
        verbose = FALSE
      )
      saveRDS(
        object = out$mod,
        file = paste0(
          out_path,
          "model3_inference_",
          format(min(out$pred$time), "%Y%m%d%H"),
          "_",
          format(max(out$pred$time), "%Y%m%d%H"),
          ".rds"
        )
      )
      data.table::fwrite(
        object = out$pred,
        file = paste0(
          out_path,
          "model3_inference_pred_",
          format(min(out$pred$time), "%Y%m%d%H"),
          "_",
          format(max(out$pred$time), "%Y%m%d%H"),
          ".csv"
        )
      )
    }
  }
  end_time <- Sys.time()
  return(list(exec_time = end_time - start_time))
}
