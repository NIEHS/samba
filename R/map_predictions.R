#' Map urban heat island with point observations
#' @description Map urban heat island infered with the Bayesian model
#' with observations from weather stations
#' @param pred_stat a dataframe of predictions
#' @param obs a dataframe of point observations
#' @param borders vector with borders shapefile (as a landmark on the map)
#' @param time POSIXct time of the map
#' @return a ggplot2 of a map
#' @importFrom lubridate hours seconds
#' @importFrom sf st_as_sf
#' @importFrom dplyr between
#' @import ggplot2
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @author Eva Marques
#' @export
map_uhi_obs <- function(pred_stat, obs, borders, time) {
  lon <- lat <- pred_mean <- temp <- network <- NULL
  te <- time + lubridate::hours(1) - lubridate::seconds(1)
  pred_plot <- pred_stat[which(pred_stat$time == time), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  obs <- obs[which(dplyr::between(obs$time, time, te)), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  range <- range(pred_plot$pred_mean, na.rm = TRUE)
  ndeg <- ceiling(range[2] - range[1])
  tn <- floor(mean(pred_plot$pred_mean, na.rm = TRUE) - ndeg / 2)
  tx <- ceiling(mean(pred_plot$pred_mean, na.rm = TRUE) + ndeg / 2)
  pal_ipcc <- load_palette("temp_ipcc")
  shape <- c("WU" = 21)
  labels <- c("WU" = "Weather Underground")
  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = pred_plot,
      ggplot2::aes(
        x = lon,
        y = lat,
        fill = pred_mean
      ),
      width = 0.01,
      height = 0.01
    ) +
    ggplot2::geom_point(
      data = obs,
      ggplot2::aes(x = lon, y = lat, fill = temp, shape = network),
      size = 3
    ) +
    ggplot2::geom_sf(data = borders, fill = NA, size = 0.05) +
    ggplot2::coord_sf(crs = 4326) +
    ggplot2::scale_fill_gradientn(
      colours = pal_ipcc,
      na.value = NA,
      breaks = seq(tn, tx, 1),
      limits = c(tn, tx)
    ) +
    ggplot2::labs(fill = "T (°C)") +
    ggplot2::ggtitle(strftime(time, format = "%Y-%m-%d %H:%M:%S EDT")) +
    ggplot2::scale_shape_manual("", values = shape, labels = labels) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 23, barheight = 1.5)
    ) +
    ggspatial::annotation_scale(
      location = "tr", text_cex = 1.5,
      pad_x = ggplot2::unit(0.5, "cm"),
      pad_y = ggplot2::unit(0.5, "cm"),
      height = ggplot2::unit(0.30, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "br", which_north = "true",
      pad_x = ggplot2::unit(0.5, "cm"), pad_y = ggplot2::unit(0.5, "cm")
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.title = ggplot2::element_blank(),
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

#' Map urban heat island
#' @description Map urban heat island infered with the Bayesian model
#' @param uhi urban heat island SpatRaster
#' @param borders vector with borders shapefile (as a landmark on the map)
#' @param ts POSIXct time of the map
#' @return a ggplot2 of a map
#' @import ggplot2
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom terra time
#' @importFrom tidyterra geom_spatraster geom_spatvector
#' @author Eva Marques
#' @export
map_uhi <- function(uhi, borders, ts) {
  tz <- NULL
  uhi_ts <- uhi[[which(terra::time(uhi) == ts)]]
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = uhi_ts) +
    tidyterra::geom_spatvector(data = borders,
                               fill = NA,
                               size = 2,
                               linewidth = .1) +
    ggplot2::scale_fill_gradientn(
      colours = load_palette("temp_ipcc"),
      na.value = NA,
      limits = c(-5, 5),
      breaks = seq(-5, 5, by = 1)
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
