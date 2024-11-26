mytile <- function(df, fill) {
  plot <- ggplot(df,
                 aes(x = lubridate::hour(time),
                     y = as.Date(time),
                     fill = .data[[fill]])) +
    geom_tile() +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 6, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 12, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 18, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 24, linetype = "dashed", size = 0.2) +
    # geom_hline(
    #   yintercept = as.Date("2021-07-04", tz = "UTC"),
    #   linetype = "dashed", size = 0.2
    # ) +
    # geom_hline(
    #   yintercept = as.Date("2021-07-11", tz = "UTC"),
    #   linetype = "dashed", size = 0.2
    # ) +
    # geom_hline(
    #   yintercept = as.Date("2021-07-18", tz = "UTC"),
    #   linetype = "dashed", size = 0.2
    # ) +
    # geom_hline(
    #   yintercept = as.Date("2021-07-25", tz = "UTC"),
    #   linetype = "dashed", size = 0.2
    # ) +
    scale_x_continuous(breaks = c(0, 6, 12, 18, 24),
                       labels = as.character(c(0, 6, 12, 18, 24))) +
    scale_y_date(
      date_labels = "%d/%m", date_breaks = "3 days",
      date_minor_breaks = "1 day",
      limits = c(
        lubridate::date(min(df$time)),
        lubridate::date(max(df$time) + lubridate::days(1))
      )
    ) +
    labs(
      y = "",
      x = "UTC"
    ) +
    guides(fill = guide_colourbar(barwidth = 1.5, barheight = 22)) +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.key.width = unit(1, "cm"),
      panel.grid.major = element_line(color = "grey", linewidth = 0.2),
      legend.text = element_text(size = 16),
      plot.caption = element_text(size = 14),
      legend.title = element_text(size = 18),
      legend.text.align = 0,
      legend.box.spacing = unit(0, "pt")
    ) +
    guides(linetype = guide_legend(nrow = 2))

  return(plot)
}


mytile_rain <- function(df) {
  plot <- ggplot(df,
                 aes(x = lubridate::hour(time),
                     y = as.Date(time),
                     fill = as.factor(ifelse(rain != 0, 1, 0)))) +
    geom_tile() +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 6, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 12, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 18, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 24, linetype = "dashed", size = 0.2) +
    # geom_hline(
    #   yintercept = as.Date("2021-07-04", tz = "UTC"),
    #   linetype = "dashed", size = 0.2
    # ) +
    # geom_hline(
    #   yintercept = as.Date("2021-07-11", tz = "UTC"),
    #   linetype = "dashed", size = 0.2
    # ) +
    # geom_hline(
    #   yintercept = as.Date("2021-07-18", tz = "UTC"),
    #   linetype = "dashed", size = 0.2
    # ) +
    # geom_hline(
    #   yintercept = as.Date("2021-07-25", tz = "UTC"),
    #   linetype = "dashed", size = 0.2
    # ) +
    scale_x_continuous(breaks = c(0, 6, 12, 18, 24),
                       labels = as.character(c(0, 6, 12, 18, 24))) +
    scale_y_date(
      date_labels = "%d/%m", date_breaks = "3 days",
      date_minor_breaks = "1 day",
      limits = c(
        lubridate::date(min(df$time)),
        lubridate::date(max(df$time) + lubridate::days(1))
      )
    ) +
    labs(
      y = "",
      x = "UTC"
    ) +
    guides(fill = guide_colourbar(barwidth = 1.5, barheight = 22)) +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.key.width = unit(1, "cm"),
      panel.grid.major = element_line(color = "grey", linewidth = 0.2),
      legend.text = element_text(size = 16),
      plot.caption = element_text(size = 14),
      legend.title = element_text(size = 18),
      legend.text.align = 0,
      legend.box.spacing = unit(0, "pt")
    ) +
    guides(linetype = guide_legend(nrow = 2))

  return(plot)
}
