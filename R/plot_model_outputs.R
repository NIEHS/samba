#' @title Plot model outputs
#' @description Plot model prediction mean
#' @param pred a data.frame with prediction mean, lat, lon, time
#' @param borders a terra::SpatVector with territory borders
#' @param tz timezone character
#' @import ggplot2
#' @import sf
#' @importFrom ggspatial annotation_scale annotation_north_arrow coord_sf
#' @importFrom tidyterra geom_spatraster geom_spatvector
#' @importFrom lubridate with_tz
#' @return a list of ggplot2 objects
map_pred_mean <- function(pred, borders, tz = "America/New_York") {
  stopifnot("lon missing" = "lon" %in% colnames(pred),
            "lat missing" = "lat" %in% colnames(pred),
            "time missing" = "time" %in% colnames(pred),
            "pred_mean missing" = "pred_mean" %in% colnames(pred))
  pred$time <- lubridate::with_tz(pred$time, tz = tz)
  period <- seq(min(pred$time), max(pred$time), by = "1 hour")
  tn <- floor(min(pred$pred_mean, na.rm = TRUE))
  tx <- ceiling(max(pred$pred_mean, na.rm = TRUE))
  plots <- list()
  for (p in period) {
    p_str <- strftime(p, format = "%Y-%m-%d %H:%M:%S") |>
      as.POSIXct(tz = tz)
    nx <- length(unique(as.numeric(sprintf("%.3f", pred$lon))))
    ny <- length(unique(as.numeric(sprintf("%.3f", pred$lat))))
    pred_plot <- pred[which(pred$time == p_str), ] |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
      sf_as_spatraster("pred_mean", nx = nx, ny = ny)
    plots[[which(period == p)]] <- ggplot2::ggplot() +
      tidyterra::geom_spatraster(data = pred_plot) +
      tidyterra::geom_spatvector(data = borders,
                                 fill = "transparent",
                                 color = "black",
                                 linewidth = .1) +
      ggplot2::scale_fill_gradientn(
        colours = c("#F7F7F7",
                    "#FDDBC7",
                    "#F4A582",
                    "#D6604D",
                    "#B2182B",
                    "#67001F"),
        limits = c(tn, tx),
        breaks = seq(tn, tx, by = 1)
      ) +
      ggplot2::ggtitle(strftime(p, format = "%Y-%m-%d %H:%M:%S EDT")) +
      ggspatial::coord_sf(xlim = c(min(pred$lon), max(pred$lon) - 0.01),
                          ylim = c(min(pred$lat) + 0.01, max(pred$lat)),
                          expand = FALSE) +
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
  }
  return(plots)
}
