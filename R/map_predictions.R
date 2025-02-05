
map_uhi_obs <- function(pred_stat, obs, borders, time) {
  te <- time + lubridate::hours(1) - lubridate::seconds(1)
  pred_plot <- pred_stat[which(pred_stat$time == time), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  obs <- obs[which(dplyr::between(obs$time, time, te)), ] |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)
  range <- range(pred_plot$pred_mean, na.rm = TRUE)
  ndeg <- ceiling(range[2] - range[1])
  tn <- floor(mean(pred_plot$pred_mean, na.rm = TRUE) - ndeg / 2)
  tx <- ceiling(mean(pred_plot$pred_mean, na.rm = TRUE) + ndeg / 2)
  pal_ipcc <- list(c(103, 0, 31),
                   c(178, 24, 43),
                   c(214, 96, 77),
                   c(244, 165, 130),
                   c(253, 219, 199),
                   c(247, 247, 247),
                   c(209, 229, 240),
                   c(146, 197, 222),
                   c(67, 147, 195),
                   c(33, 102, 172),
                   c(5, 48, 97)
  ) |>
    lapply(function(x) rgb(x[1], x[2], x[3], maxColorValue = 255)) |>
    rev()
  shape <- c("WU" = 21)
  labels <- c("WU" = "Weather Underground")
  p <- ggplot() +
    geom_tile(
      data = pred_plot, aes(x = lon,
                            y = lat,
                            fill = pred_mean),
      width = 0.01, height = 0.01
    ) +
    geom_point(
      data = obs,
      aes(x = lon, y = lat, fill = temp, shape = network),
      size = 3
    ) +
    geom_sf(data = borders, fill = NA, size = 0.05) +
    coord_sf(crs = 4326) +
    scale_fill_gradientn(
      colours = pal_ipcc,
      na.value = NA,
      breaks = seq(tn, tx, 1),
      limits = c(tn, tx)
    ) +
    labs(fill = "T (°C)") +
    ggtitle(strftime(time, format = "%Y-%m-%d %H:%M:%S EDT")) +
    scale_shape_manual("", values = shape, labels = labels) +
    guides(fill = guide_colourbar(barwidth = 23, barheight = 1.5)) +
    ggspatial::annotation_scale(
      location = "tr", text_cex = 1.5,
      pad_x = unit(0.5, "cm"),
      pad_y = unit(0.5, "cm"),
      height = unit(0.30, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "br", which_north = "true",
      pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.title = element_blank(),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(
        size = 18,
        angle = 90,
        hjust = .5
      ),
      plot.caption = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
  return(p)
}

map_uhi <- function(uhi, borders, ts) {
  uhi_ts <- uhi[[which(terra::time(uhi) == ts)]]
  p <- ggplot() +
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
  return(p)
}
