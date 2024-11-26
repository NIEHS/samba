#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
# args[1] = model number, eg: "5"
# args[2] = ts in UTC, eg: "2021-07-01 00:00:00"
# args[3] = te in UTC, eg: "2021-07-31 23:00:00"
# args[4] = boolean for model saving as rds
# args[5] = boolean for saving all maps + analysis graphs
# args[6] = boolean for evaluation

# test if there are 5 arguments: if not, return an error
if (length(args) != 6) {
  stop("Six arguments must be supplied.", call. = FALSE)
}

save_mod <- args[4]
save_graphs <- args[5]
save_eval <- args[6]

devtools::load_all()

# create all output and graphs repositories if they do not exist
if (!dir.exists(paste0("./output/model", args[1]))) {
  dir.create(paste0("./output/model", args[1]))
}
if (!dir.exists(paste0("./graphs/model", args[1]))) {
  dir.create(paste0("./graphs/model", args[1]))
}
if (!dir.exists(paste0("./graphs/model", args[1], "/evaluation"))) {
  dir.create(paste0("./graphs/model", args[1], "/evaluation"))
}
if (!dir.exists(paste0("./graphs/model", args[1], "/interpretation"))) {
  dir.create(paste0("./graphs/model", args[1], "/interpretation"))
}
if (!dir.exists(paste0("./graphs/model", args[1], "/maps"))) {
  dir.create(paste0("./graphs/model", args[1], "/maps"))
}

config <- list(
  model = args[1],
  t_start = as.POSIXct(args[2], tz = "UTC"),
  t_end = as.POSIXct(args[3], tz = "UTC"),
  in_path = "./input/",
  out_path = paste0("./output/model", args[1], "/"),
  graphs_path = paste0("./graphs/model", args[1], "/")
)

# store console output to a file
run_file <- paste0(
  config$out_path,
  "console_output_rundate_",
  format(Sys.time(), "%Y%m%d%H%M%S"),
  ".txt"
)
cat(run_file, "\n")
sink(file = run_file)


cat("Arguments: ", args, "\n")

# open data
source("open_triangle.R")
input <- open_triangle(cws = TRUE, pred = TRUE, ref_network = TRUE)
cws <- input$cws
n_cws <- length(unique(cws$site_id))
n_time <- length(unique(cws$time))

# print information about stations and missing data
cat("Number of stations: ", n_cws, "\n")
cat("Number of time points: ", n_time, "\n")
cat("Expected number of observations: ", n_cws * n_time, "\n")
cat("Actual number of observations: ", nrow(cws), "\n")
cat(
  "Number of missing observations referenced in the dataset: ",
  sum(is.na(cws$temp)), "\n"
)
cat(
  "Number of missing observations not referenced in the dataset: ",
  n_cws * n_time - nrow(cws), "\n"
)
cat(
  "Percentage of missing observations: ",
  (n_cws * n_time - nrow(cws)) / (n_cws * n_time) * 100, "%\n"
)

cat(config$t_start, " ", class(config$t_start), "\n")
cat(config$t_end, " ", class(config$t_end), "\n")

start_time <- Sys.time()
# remove NaNs
# data <- input$cws |>
#  dplyr::filter(!is.na(temp)) |>
#  dplyr::filter(!is.na(era5))

# INLA::inla.binary.install()

inference <- get(paste0("inference_mod", config$model))
out <- inference(
  input$cws,
  input$pred,
  polygon = input$area_rect,
  config$t_start,
  config$t_end,
  verbose = TRUE,
  debug = TRUE
)
end_time <- Sys.time()
cat("Running time: ", end_time - start_time, "\n")

if (save_mod) {
  cat("Save model... \n")
  saveRDS(
    object = out$mod,
    file = paste0(
      config$out_path,
      "model",
      config$model,
      "_inference_mod_",
      format(min(out$pred$time), "%Y%m%d%H"),
      "_",
      format(max(out$pred$time), "%Y%m%d%H"),
      ".csv"
    )
  )
  cat("...Done \n")
}
cat("Save info and predictions... \n")
data.table::fwrite(
  x = out$info,
  file = paste0(
    config$out_path,
    "model",
    config$model,
    "_inference_info_",
    format(min(out$pred$time), "%Y%m%d%H"),
    "_",
    format(max(out$pred$time), "%Y%m%d%H"),
    ".csv"
  )
)
data.table::fwrite(
  x = out$pred,
  file = paste0(
    config$out_path,
    "model",
    config$model,
    "_inference_pred_",
    format(min(out$pred$time), "%Y%m%d%H"),
    "_",
    format(max(out$pred$time), "%Y%m%d%H"),
    ".csv"
  )
)
cat("...Done \n")
cat("inference done from ", config$t_start, " to ", config$t_end, "\n")

if (save_graphs == TRUE || save_eval == TRUE) {
  cat("rasterize output predictions...\n")
  pred_mean <- rasterize_pred(out$pred, varname = "pred_mean")
  pred_sd <- rasterize_pred(out$pred, varname = "pred_sd")
  cat("...Done \n")
}


if (save_graphs) {
  cat("Save maps... \n")
  # compute urban heat island (UHI)
  pred_mean_avg <- terra::global(pred_mean, "mean", na.rm = TRUE)
  pred_mean_avg$time <- terra::time(pred_mean)
  uhi <- pred_mean - terra::global(pred_mean, "mean", na.rm = TRUE)$mean
  # save all uhi maps
  period <- seq(min(out$pred$time), max(out$pred$time), by = "1 hour")
  tz <- "UTC"
  for (p in period) {
    p_str <- strftime(p, format = "%Y-%m-%d %H:%M:%S", tz = tz) |>
      as.POSIXct(tz = tz)
    p <- map_uhi(uhi, input$plot_shp, p_str)
    ggsave(p,
      filename = paste0(
        "./graphs/model",
        config$model,
        "/maps/map_uhi_",
        format(p_str, "%Y%m%d%H"),
        "UTC.png"
      ),
      width = 10,
      height = 10
    )
  }
  # save average uhi on the period
  p <- ggplot() +
    tidyterra::geom_spatraster(data = terra::mean(uhi)) +
    tidyterra::geom_spatvector(
      data = input$plot_shp,
      fill = NA,
      size = 2,
      linewidth = .1
    ) +
    ggplot2::scale_fill_gradientn(
      colours = load_palette("temp_ipcc"),
      na.value = NA,
      limits = c(-1.5, 1.5),
      breaks = seq(-1.5, 1.5, by = .5)
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
      axis.text = ggplot2::element_text(size = 12),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  p
  ggsave(p,
    filename = paste0(
      "./graphs/model",
      config$model,
      "/map_uhiavg_from_",
      format(min(out$pred$time), "%Y%m%d%H"),
      "_to_",
      format(max(out$pred$time), "%Y%m%d%H"),
      ".png"
    ),
    width = 10,
    height = 10
  )
  cat("...Done \n")
  cat("Posterior marginal analysis... \n")

  if (config$model == "6") {
    p10 <- ggplot(data = data.frame(x = c(-.5, .5)), aes(x)) +
      stat_function(
        fun = dnorm, n = 1000,
        args = list(
          mean = out$info$hour10.elev_mean[1],
          sd = out$info$hour10.elev_sd[1]
        ), aes(color = "elev")
      ) +
      stat_function(
        fun = dnorm, n = 1000,
        args = list(
          mean = out$info$hour10.imp_mean[1],
          sd = out$info$hour10.imp_sd[1]
        ), aes(color = "imp")
      ) +
      stat_function(
        fun = dnorm, n = 1000,
        args = list(
          mean = out$info$hour10.fch_mean[1],
          sd = out$info$hour10.fch_sd[1]
        ), aes(color = "fch")
      ) +
      stat_function(
        fun = dnorm, n = 1000,
        args = list(
          mean = out$info$hour10.bf_mean[1],
          sd = out$info$hour10.bf_sd[1]
        ), aes(color = "bf")
      ) +
      stat_function(
        fun = dnorm, n = 1000,
        args = list(
          mean = out$info$hour10.et_mean[1],
          sd = out$info$hour10.et_sd[1]
        ), aes(color = "et")
      ) +
      scale_color_manual("",
        values = load_palette("covariates")
      ) +
      annotate("text",
        x = 0.3,
        y = 10,
        label = "1OUTC",
        size = 9
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

    p19 <- ggplot(data = data.frame(x = c(-.5, .5)), aes(x)) +
      stat_function(
        fun = dnorm, n = 1000,
        args = list(
          mean = out$info$hour20.elev_mean[1],
          sd = out$info$hour20.elev_sd[1]
        ), aes(color = "elev")
      ) +
      stat_function(
        fun = dnorm, n = 1000,
        args = list(
          mean = out$info$hour20.imp_mean[1],
          sd = out$info$hour20.imp_sd[1]
        ), aes(color = "imp")
      ) +
      stat_function(
        fun = dnorm, n = 1000,
        args = list(
          mean = out$info$hour20.fch_mean[1],
          sd = out$info$hour20.fch_sd[1]
        ), aes(color = "fch")
      ) +
      stat_function(
        fun = dnorm, n = 1000,
        args = list(
          mean = out$info$hour20.bf_mean[1],
          sd = out$info$hour20.bf_sd[1]
        ), aes(color = "bf")
      ) +
      stat_function(
        fun = dnorm, n = 1000,
        args = list(
          mean = out$info$hour20.et_mean[1],
          sd = out$info$hour20.et_sd[1]
        ), aes(color = "et")
      ) +
      scale_color_manual("",
        values = load_palette("covariates")
      ) +
      annotate("text",
        x = 0.3,
        y = 10,
        label = "19UTC",
        size = 9
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

    p_eg <- ggpubr::ggarrange(p10, p19, nrow = 2, common.legend = TRUE)
    ggsave(
      paste0(
        "./graphs/model",
        config$model,
        "/interpretation/eg_posterior_distributions.png"
      ),
      plot = p_eg,
      width = 5,
      height = 10,
      bg = "white",
      dpi = 300
    )
  }

  col_mean_fixed <- colnames(info)[grep("^hour.*_mean$", colnames(info))]
  col_sd_fixed <- colnames(info)[grep("^hour.*_sd$", colnames(info))]
  covar_names <- unique(sub(".*\\.(.*?)_.*", "\\1", col_mean_fixed))
  coeffs <- data.frame(
    rep(1:24, length(covar_names)),
    rep(covar_names, each = 24),
    t(out$info[1, col_mean_fixed]),
    t(out$info[1, col_sd_fixed])
  )

  colnames(coeffs) <- c("hour", "var", "mean", "sd")

  # coefficients evolution across the curse of the day
  ggplot(coeffs) +
    geom_line(aes(x = hour - 5, y = mean, color = var, group = var)) +
    geom_ribbon(
      aes(
        x = hour - 5,
        ymin = mean - 2 * sd,
        ymax = mean + 2 * sd, fill = var
      ),
      alpha = 0.1
    ) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    scale_color_manual("", values = load_palette("covariates")) +
    scale_fill_manual("", values = load_palette("covariates")) +
    ylim(c(-0.6, 0.6)) +
    xlab("local time") +
    # latex2exp
    ylab(latex2exp::TeX("coef $\\mu\\pm 2\\sigma$")) +
    scale_x_continuous(
      breaks = seq(-4, 19, 1),
      labels = c(20:23, 0:19)
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      axis.title = element_text(size = 18),
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
  ggsave(
    paste0(
      "./graphs/model",
      config$model,
      "/interpretation/coeffs_mean_pm2sd.png"
    ),
    width = 10,
    height = 5,
    bg = "white",
    dpi = 300
  )
  cat("...Done \n")
}


if (save_eval == TRUE) {
  cat("Save evaluation... \n")
  source("open_econet.R")
  ref <- open_econet("./input/econet/")
  lubridate::tz(ref$time)
  cws <- input$cws
  ref <- ref[which(between(ref$time, min(out$pred$time), max(out$pred$time))), ]
  timeseries_temp(
    out$pred,
    ref,
    as.POSIXct("2021-07-01 00:00:00", tz = "UTC"),
    as.POSIXct("2021-07-31 23:00:00", tz = "UTC")
  )
  ggsave(
    paste0(
      "./graphs/model",
      config$model,
      "/evaluation/timeserie_pred_avg_vs_stations.png"
    ),
    width = 10,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  # extract predictions at reference stations
  ref <- add_pred_to_pro(out$pred, ref)
  timeseries_res(ref)
  # TODO concat those scores to run out$info
  cat("R2 = ", cor(ref$temp, ref$pred_mean)**2, "\n")
  cat("RMSE =", sqrt(sum((ref$res)**2, na.rm = TRUE) /
    length(which(!is.na(ref$res)))), "\n")
  cat("MAE =", mean(abs(ref$res), na.rm = TRUE), "\n")
  cat("Median residuals", median(ref$res, na.rm = TRUE), "\n")
  # map RMSE per reference station
  imp <- terra::rast(paste0(
    "../input/data_files/",
    "nlcd_2021_impervious_l48_20230630.img"
  ))
  area_rect_p <- terra::project(input$area_rect, imp)
  imp_cropped <- terra::crop(imp, area_rect_p)
  terra::values(imp_cropped) <- as.numeric(terra::values(imp_cropped))
  map_rmse(ref, imp_cropped) +
    tidyterra::geom_spatvector(
      data = input$plot_shp,
      fill = "transparent",
      color = "red",
      linewidth = .1
    )
  ggsave(
    paste0(
      "./graphs/model",
      config$model,
      "/evaluation/rmse_station.png"
    ),
    width = 6,
    height = 5,
    dpi = 300
  )
  boxplot_per_station(ref)
  ggsave(
    paste0(
      "./graphs/model",
      config$model,
      "/evaluation/boxplot_hour_station.png"
    ),
    width = 4,
    height = 5,
    dpi = 300
  )
  boxplot_per_hour(ref)
  ggsave(
    paste0(
      "./graphs/model",
      config$model,
      "/evaluation/boxplot_hour_all_ref_network.png"
    ),
    width = 8,
    height = 5,
    dpi = 300
  )
  cat("...Done \n")
}

sink(file = NULL)
