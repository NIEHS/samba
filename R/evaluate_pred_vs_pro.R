#' Add prediction mean to professional network dataset
#' @description
#' Add prediction mean to professional network dataset.
#' This function is needed to run model evaluation.
#' @param pred prediction dataset with lat, lon, pred_mean and pred_sd columns
#' @param pro professional weather station dataset
#' @return the professional dataset with added columns pred_mean and pred_sd
#' @importFrom sf st_as_sf st_nearest_feature
#' @importFrom dplyr rename
#' @importFrom data.table as.data.table
#' @author Eva Marques
#' @export
add_pred_to_pro <- function(pred, pro) {
  lon <- lat <- NULL
  stopifnot(
    "missing variables in pred" =
      c("lat", "lon", "pred_mean", "pred_sd") %in% names(pred)
  )
  pred_pts <- unique(pred[, c("lon", "lat")]) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  pro_stations <- pro[, c("lon", "lat", "site_id")]
  pro_stations$geometry <- NULL
  pro_stations <- pro_stations |>
    unique() |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  pred_pts_pro <- pred_pts[
    sf::st_nearest_feature(pro_stations, pred_pts),
    c("lon", "lat")
  ] |>
    dplyr::rename(lon_pred = lon, lat_pred = lat)
  pro_stations <- cbind(pro_stations, pred_pts_pro)
  pro_stations$geometry <- NULL
  pro_stations$geometry.1 <- NULL
  pro_pred <- merge(pro,
    pro_stations,
    by = c("lon", "lat", "site_id")
  )
  pro_pred$time <- lubridate::with_tz(pro_pred$time, tzone = "UTC")
  pred_test <- pred |>
    dplyr::rename(lon_pred = lon, lat_pred = lat)
  pro_pred <- merge(pro_pred,
    pred_test[, c("time", "lon_pred", "lat_pred", "pred_mean", "pred_sd")],
    by = c("time", "lon_pred", "lat_pred")
  ) |>
    data.table::as.data.table()
  # residuals for each pro site_id
  pro_pred$res <- pro_pred$pred_mean - pro_pred$temp
  pro_pred
}

#' Plot temperature timeseries
#' @description
#' Plot predicted and observed (by professional network) temperature timeseries
#' @param pred prediction dataset
#' @param pro professional weather station dataset
#' @param ts POSIXct of timeserie starting date
#' @param te POSIXct of timeserie ending date
#' @return a plot of the timeseries
#' @import ggplot2
#' @importFrom dplyr between
#' @author Eva Marques
#' @export
timeseries_temp <- function(pred, pro, ts, te) {
  temp <- site_id <- pred_mean <- NULL
  p <- pred[which(dplyr::between(pro$time, ts, te)), ] |>
    dplyr::group_by(time) |>
    dplyr::summarise(pred_mean = mean(pred_mean, na.rm = TRUE)) |>
    ggplot2::ggplot(ggplot2::aes(x = time, y = pred_mean)) +
    ggplot2::geom_line(ggplot2::aes(color = "pred avg"), linewidth = 2) +
    ggplot2::theme_minimal() +
    ggplot2::geom_line(
      data = pro[which(dplyr::between(pro$time, ts, te)), ],
      ggplot2::aes(x = time, y = temp, color = site_id)
    ) +
    ggplot2::scale_x_datetime(date_labels = "%d", date_breaks = "1 days") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  p
}

#' Plot residual timeseries
#' @description
#' Plot residual timeseries
#' @param pro professional weather station dataset
#' @return a plot of the residual timeseries
#' @import ggplot2
#' @author Eva Marques
#' @export
timeseries_res <- function(pro) {
  res <- site_id <- NULL
  stopifnot("res is not in colnames(pro)" = "res" %in% colnames(pro))
  p <- ggplot2::ggplot(pro) +
    ggplot2::geom_point(ggplot2::aes(x = time, y = res, color = site_id)) +
    ggplot2::scale_x_datetime(date_labels = "%d", date_breaks = "1 days") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    )
  p
}

#' Calculate and plot RMSE map
#' This function calculates the root mean square error (RMSE)
#' for each station in the \code{pro} dataset and plots it on a map
#' with imperviousness background
#' @param pro The dataset of professional weather stations used as reference
#' @param imp Imperviousness raster
#' @return A ggplot object displaying the RMSE values on a map.
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @import ggplot2
#' @importFrom data.table .SD
#' @export
map_rmse <- function(pro, imp) {
  rmse <- lat <- lon <- site_id <- NULL
  stopifnot("res is not in colnames(pro)" = "res" %in% colnames(pro))
  rmse_station <- pro[,
    .(rmse = sqrt(mean(.SD$res^2, na.rm = TRUE))), #nolint
    by = .(site_id, lat, lon), #nolint
    .SDcols = "res"] |> #nolint
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    terra::vect() |>
    terra::project(imp)
  pro <- pro |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(crs = terra::crs(imp))
  pro$lat <- sf::st_coordinates(pro)[, 2]
  pro$lon <- sf::st_coordinates(pro)[, 1]
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = imp) +
    tidyterra::geom_spatvector(
      data = rmse_station,
      ggplot2::aes(color = rmse),
      shape = 16,
      size = 4,
      linewidth = .1
    ) +
    ggplot2::geom_text(
      data = pro,
      ggplot2::aes(x = lon, y = lat + 3000, label = site_id),
      size = 3
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c(
        "#F7F7F7",
        "lightgrey",
        "black"
      ),
      na.value = NA
    ) +
    ggplot2::labs(fill = "imp (%)", color = "RMSE (C)") +
    ggplot2::scale_color_stepsn(
      colours = load_palette("reds"),
      na.value = NA
    ) +
    ggspatial::annotation_scale(
      location = "bl", pad_x = ggplot2::unit(1, "cm"),
      pad_y = ggplot2::unit(1, "cm"),
      height = ggplot2::unit(0.30, "cm"),
      text_cex = 1
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      which_north = "true",
      pad_x = ggplot2::unit(0.2, "cm"),
      pad_y = ggplot2::unit(0.2, "cm")
    ) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  p
}

#' Calculate and plot RMSE map with NLCD background
#' This function calculates the root mean square error (RMSE)
#' for each station in the \code{pro} dataset and plots it on a map
#' with National Land Cover Dataset background
#' @param pro The dataset of professional weather stations used as reference
#' @param nlcd National Land Cover Dataset raster
#' @return A ggplot object displaying the RMSE values on a map.
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @import ggplot2
#' @importFrom terra vect project crs
#' @importFrom tidyterra geom_spatraster geom_spatvector
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom data.table .SD
#' @export
map_rmse_nlcd <- function(pro, nlcd) {
  lon <- lat <- rmse <- site_id <-  NULL
  stopifnot("res is not in colnames(pro)" = "res" %in% colnames(pro))
  rmse_station <- pro[,
    .(rmse = sqrt(mean(.SD$res^2, na.rm = TRUE))), #nolint
    by = .(site_id, lat, lon), #nolint
    .SDcols = "res"] |> #nolint
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    terra::vect() |>
    terra::project(nlcd)
  pro <- pro |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(crs = terra::crs(nlcd))
  pro$lat <- sf::st_coordinates(pro)[, 2]
  pro$lon <- sf::st_coordinates(pro)[, 1]
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = nlcd) +
    tidyterra::geom_spatvector(
      data = rmse_station,
      ggplot2::aes(color = rmse),
      shape = 16,
      size = 4,
      linewidth = .1
    ) +
    ggplot2::geom_text(
      data = pro,
      ggplot2::aes(x = lon, y = lat + 3000, label = site_id),
      size = 3
    ) +
    ggplot2::scale_fill_manual(
      values = load_palette("nlcd")$color,
      breaks = load_palette("nlcd")$description,
      na.value = NA
    ) +
    ggplot2::labs(fill = "NLCD", color = "RMSE (C)") +
    ggplot2::scale_color_stepsn(
      colours = c("white", "yellow", "orange"),
      na.value = NA
    ) +
    ggspatial::annotation_scale(
      location = "bl", pad_x = ggplot2::unit(1, "cm"),
      pad_y = ggplot2::unit(1, "cm"),
      height = ggplot2::unit(0.30, "cm"),
      text_cex = 1
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      which_north = "true",
      pad_x = ggplot2::unit(0.2, "cm"),
      pad_y = ggplot2::unit(0.2, "cm")
    ) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(angle = 90, hjust = .5),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  p
}

#' Plot residuals boxplot per station from reference network
#' @param pro The dataset of professional weather stations used as reference
#' @return A ggplot object displaying the residual boxplots
#' @import ggplot2
#' @importFrom lubridate hour
#' @export
boxplot_per_station <- function(pro) {
  pred_mean <- temp <- NULL
  stopifnot("res is not in colnames(pro)" = "res" %in% colnames(pro))
  p <- ggplot2::ggplot(pro) +
    ggplot2::geom_boxplot(
      ggplot2::aes(
        x = lubridate::hour(time),
        y = pred_mean - temp,
        group = lubridate::hour(time)
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = -1, linetype = "dotted", color = "red") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "red") +
    ggplot2::facet_wrap(~site_id, ncol = 3) +
    ggplot2::scale_y_continuous(breaks = seq(-5, 5, by = 1)) +
    ggplot2::xlab("Hour") +
    ggplot2::ylab("pred - obs") +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  p
}

#' Plot residuals boxplot per hour from reference network
#' @param pro The dataset of professional weather stations used as reference
#' @return A ggplot object displaying the residual boxplots
#' @import ggplot2
#' @importFrom lubridate hour
#' @export
boxplot_per_hour <- function(pro) {
  temp <- pred_mean <- NULL
  stopifnot("res is not in colnames(pro)" = "res" %in% colnames(pro))
  p <- ggplot2::ggplot(pro) +
    ggplot2::geom_boxplot(
      ggplot2::aes(
        x = lubridate::hour(time),
        y = pred_mean - temp,
        group = lubridate::hour(time)
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = -1, linetype = "dotted", color = "red") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "red") +
    ggplot2::xlab("Hour (UTC)") +
    ggplot2::ylab("pred - obs") +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.title = ggplot2::element_text(size = 18),
      axis.text.x = ggplot2::element_text(size = 18),
      axis.text.y = ggplot2::element_text(
        size = 18,
        angle = 90,
        hjust = .5
      ),
      plot.caption = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 18),
      legend.title = ggplot2::element_text(size = 18),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  p
}

#' Plot median residuals per hour
#' @param pro The dataset of professional weather stations used as reference
#' @return A ggplot object displaying the residual median evolution through time
#' @import ggplot2
#' @importFrom sf st_as_sf
#' @importFrom lubridate hour
#' @importFrom stats median
#' @export
median_bias_per_hour <- function(pro) {
  hour <- bias <- site_id <- pred_mean <- temp <- lat <- lon <- . <- NULL
  stopifnot("res is not in colnames(pro)" = "res" %in% colnames(pro))
  # compute median error per station per hour
  bias_station_hour <- pro[,
    .(bias = stats::median(pred_mean - temp, na.rm = TRUE)), #nolint
    by = .(site_id, lat, lon, hour = lubridate::hour(time)) #nolint
  ] |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  # plot curve of bias by station and hour
  p <- ggplot2::ggplot(bias_station_hour) +
    ggplot2::geom_line(
      ggplot2::aes(x = hour, y = bias, color = site_id)
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "red",
      linewidth = 1
    ) +
    ggplot2::xlab("Hour") +
    ggplot2::ylab("q0.5(pred - obs)") +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 16),
      axis.text.x = ggplot2::element_text(size = 16),
      axis.text.y = ggplot2::element_text(
        size = 18,
        angle = 90,
        hjust = .5
      ),
      plot.caption = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 16),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  p
}

#' Map prediction mean with professional measurements for a given time
#' @param pred prediction dataset with pred_mean column
#' @param pro dataset of professional weather stations used as reference
#' @param ts time stamp for the map
#' @param borders borders shapefile (as a landmark on the map)
#' @param tz string of the timezone chosen to be printed with the timestamp
#' in the title
#' @return A ggplot object displaying the residual median evolution through time
#' @import ggplot2
#' @importFrom sf st_as_sf
#' @importFrom terra vect project values
#' @importFrom tidyterra geom_spatraster geom_spatvector
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom lubridate hour
#' @export
map_pred_with_pro <- function(pred, pro, ts, borders, tz) {
  temp <- NULL
  nx <- length(unique(as.numeric(sprintf("%.3f", pred$lon))))
  ny <- length(unique(as.numeric(sprintf("%.3f", pred$lat))))
  pred_sample <- pred[which(pred$time == ts), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf_as_spatraster("pred_mean", nx = nx, ny = ny)
  # turn pro in a SpatVector
  pro <- pro |>
    terra::vect(crs = "epsg:4326")
  pro_sample <- pro[which(pro$time == ts), ]
  borders <- terra::project(borders, "epsg:4326")
  # ggplot of the predictions with pro_sample
  tn <- floor(
    c(
      min(
        min(pro_sample$temp, na.rm = TRUE),
        min(terra::values(pred_sample), na.rm = TRUE)
      )
    )
  )
  tx <- ceiling(
    c(
      max(
        max(pro_sample$temp, na.rm = TRUE),
        max(terra::values(pred_sample), na.rm = TRUE)
      )
    )
  )
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = pred_sample) +
    tidyterra::geom_spatvector(
      data = pro_sample,
      ggplot2::aes(fill = temp),
      color = "black",
      shape = 21,
      size = 2,
      linewidth = .1
    ) +
    tidyterra::geom_spatvector(
      data = borders,
      fill = NA,
      size = 2,
      linewidth = .1
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c(
        "#F7F7F7",
        "#FDDBC7",
        "#F4A582",
        "#D6604D",
        "#B2182B",
        "#67001F"
      ),
      na.value = NA,
      limits = c(tn, tx),
      breaks = seq(tn, tx, by = 1)
    ) +
    ggplot2::ggtitle(strftime(ts, format = "%Y-%m-%d %H:%M:%S", tz = tz)) +
    ggspatial::annotation_scale(
      location = "bl", pad_x = ggplot2::unit(1, "cm"),
      pad_y = ggplot2::unit(1, "cm"),
      height = ggplot2::unit(0.30, "cm"),
      text_cex = 1
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      which_north = "true",
      pad_x = ggplot2::unit(0.2, "cm"),
      pad_y = ggplot2::unit(0.2, "cm")
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  p
}
