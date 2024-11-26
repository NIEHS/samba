density_beta_covar <- function(info) {
  linetype <- c("prior" = "dashed", "post" = "solid")
  color <- c(
    "int" = "orange",
    "era5" = "maroon2",
    "tcc" = "green",
    "fch" = "darkgreen",
    "bf" = "blue"
  )
  p <- ggplot(data = data.frame(x = c(-5, 5)), aes(x)) +
    stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$dem_mean,
        sd = info$dem_sd
      ),
      linewidth = 1, aes(color = "dem", linetype = "post")
    ) +
    stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$build_d_mean,
        sd = info$build_d_sd
      ),
      linewidth = 1, aes(color = "building density", linetype = "post")
    ) +
    stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$build_h_mean,
        sd = info$build_d_sd
      ),
      linewidth = 1, aes(color = "building height", linetype = "post")
    ) +
    stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$mu_covar,
        sd = sqrt(1 / info$prec_covar)
      ),
      color = "black",
      linewidth = 1,
      aes(linetype = "prior")
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = seq(-2, 2, 0.5), limits = c(-2, 2)) +
    scale_color_manual("", values = color) +
    scale_linetype_manual("",
                          values = linetype,
                          labels = c(
                            "prior" = latex2exp::TeX("uninf. prior: $\\beta_k \\sim N(0, 10^3)$"),
                            "post" = latex2exp::TeX("post")
                          )
    ) +
    ylab("") +
    xlab("") +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.title = element_blank(),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.caption = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      legend.margin=margin(0,0,0,0),
      legend.box.spacing = unit(0, "pt"),
      legend.text.align = 0,
      legend.key.width = unit(1, 'cm'),
      panel.grid.major.x = element_line(color="grey", size = 0.2),
      panel.background = element_rect(fill = "white")
    ) +
    guides(linetype = guide_legend(nrow = 2), color = guide_legend(nrow = 3))
  return(p)
}
