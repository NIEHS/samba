#' Generic function to create meteorological tile plot on one month
#' @description Generic function to create meteorological tile plot on one month
#' @param df dataframe. Contains the covariate to be plotted by the tile plot
#' and time column
#' @param fill character. Name of the covariate to be plotted.
#' @return a ggplot2 tileplot
#' @import ggplot2
#' @importFrom lubridate date hour days
#' @author Eva Marques
#' @export
mytile <- function(df, fill) {
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = lubridate::hour(time),
      y = as.Date(time),
      fill = .data[[fill]] #nolint
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 6, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 12, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 18, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 24, linetype = "dashed", size = 0.2) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 6, 12, 18, 24),
      labels = as.character(c(0, 6, 12, 18, 24))
    ) +
    ggplot2::scale_y_date(
      date_labels = "%d/%m", date_breaks = "3 days",
      date_minor_breaks = "1 day",
      limits = c(
        lubridate::date(min(df$time)),
        lubridate::date(max(df$time) + lubridate::days(1))
      )
    ) +
    ggplot2::labs(
      y = "",
      x = "UTC"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 1.5, barheight = 22),
      linetype = ggplot2::guide_legend(nrow = 2)
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 16),
      axis.title.x = ggplot2::element_text(size = 22),
      axis.title.y = ggplot2::element_text(size = 22),
      legend.key.width = ggplot2::unit(1, "cm"),
      panel.grid.major = ggplot2::element_line(
        color = "grey",
        linewidth = 0.2
      ),
      legend.text = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 18),
      legend.text.align = 0,
      legend.box.spacing = ggplot2::unit(0, "pt")
    )
  plot
}

#' Rain tile plot on one month
#' @description Rain tile plot on one month (rain is categorical)
#' @param df dataframe. Contains `rain` and `time` columns.
#' @return a ggplot2 tileplot
#' @import ggplot2
#' @importFrom lubridate date hour days
#' @author Eva Marques
#' @export
mytile_rain <- function(df) {
  rain <- NULL
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = lubridate::hour(time),
      y = as.Date(time),
      fill = as.factor(ifelse(rain != 0, 1, 0))
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 6, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 12, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 18, linetype = "dashed", size = 0.2) +
    ggplot2::geom_vline(xintercept = 24, linetype = "dashed", size = 0.2) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 6, 12, 18, 24),
      labels = as.character(c(0, 6, 12, 18, 24))
    ) +
    ggplot2::scale_y_date(
      date_labels = "%d/%m", date_breaks = "3 days",
      date_minor_breaks = "1 day",
      limits = c(
        lubridate::date(min(df$time)),
        lubridate::date(max(df$time) + lubridate::days(1))
      )
    ) +
    ggplot2::labs(
      y = "",
      x = "UTC"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(barwidth = 1.5, barheight = 22),
      linetype = ggplot2::guide_legend(nrow = 2)
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 16),
      axis.title.x = ggplot2::element_text(size = 22),
      axis.title.y = ggplot2::element_text(size = 22),
      legend.key.width = ggplot2::unit(1, "cm"),
      panel.grid.major = ggplot2::element_line(color = "grey", linewidth = 0.2),
      legend.text = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 18),
      legend.text.align = 0,
      legend.box.spacing = ggplot2::unit(0, "pt")
    )
  plot
}
