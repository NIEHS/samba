#' Plot density for spatial covariate coefficients from the BHM
#' @description
#' Plot density for spatial covariate coefficients from the BHM
#' @param info the dataframe from BHM output with inference informations
#' @return a ggplot object
#' @import ggplot2
#' @author Eva Marques
#' @export
density_beta_covar <- function(info) {
  x <- NULL
  linetype <- c("prior" = "dashed", "post" = "solid")
  color <- c(
    "int" = "orange",
    "era5" = "maroon2",
    "tcc" = "green",
    "fch" = "darkgreen",
    "bf" = "blue"
  )
  p <- ggplot2::ggplot(data = data.frame(x = c(-5, 5)), ggplot2::aes(x)) +
    ggplot2::stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$dem_mean,
        sd = info$dem_sd
      ),
      linewidth = 1, ggplot2::aes(color = "dem", linetype = "post")
    ) +
    ggplot2::stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$build_d_mean,
        sd = info$build_d_sd
      ),
      linewidth = 1, ggplot2::aes(color = "building density", linetype = "post")
    ) +
    ggplot2::stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$build_h_mean,
        sd = info$build_d_sd
      ),
      linewidth = 1, ggplot2::aes(color = "building height", linetype = "post")
    ) +
    ggplot2::stat_function(
      fun = dnorm, n = 100,
      args = list(
        mean = info$mu_covar,
        sd = sqrt(1 / info$prec_covar)
      ),
      color = "black",
      linewidth = 1,
      ggplot2::aes(linetype = "prior")
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::scale_x_continuous(breaks = seq(-2, 2, 0.5), limits = c(-2, 2)) +
    ggplot2::scale_color_manual("", values = color) +
    ggplot2::scale_linetype_manual(
      "",
      values = linetype,
      labels = c(
        "prior" = latex2exp::TeX("uninf. prior: $\\beta_k \\sim N(0, 10^3)$"),
        "post" = latex2exp::TeX("post")
      )
    ) +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 18),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 18),
      legend.title = ggplot2::element_text(size = 18),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = ggplot2::unit(0, "pt"),
      legend.text.align = 0,
      legend.key.width = ggplot2::unit(1, "cm"),
      panel.grid.major.x = ggplot2::element_line(color = "grey", size = 0.2),
      panel.background = ggplot2::element_rect(fill = "white")
    ) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(nrow = 2),
      color = ggplot2::guide_legend(nrow = 3)
    )
  p
}
