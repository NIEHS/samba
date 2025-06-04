setwd("/ddn/gs1/home/marquesel/samba/paper/")
devtools::load_all()
source("load_materials.R")

library(ggplot2)
library(ggpointdensity)
library(viridis)
library(ggridges)

shape_type <- c("urb" = 8, "rur" = 13)
linetype_type <- c("urb" = "dashed", "rur" = "twodash")
color_type <- c("urb" = "darkgrey", "rur" = "darkolivegreen3")
color_cs_full <- c(
  "Phi/NYC 01/2021" = "navy",
  "Phi/NYC 07/2024" = "darkcyan",
  "Phoenix 07/2023" = "darkgoldenrod",
  "Triangle 07/2021" = "forestgreen"
)
color_cs <- c(
  "npw" = "navy",
  "nps" = "darkcyan",
  "phoe" = "darkgoldenrod",
  "tri" = "forestgreen"
)

# count ref network stations
nps_ref <- dplyr::distinct(input_nps$ref[, c("site_id", "network")])
nrow(nps_ref[which(nps_ref$network == "mesonet_nj"), ])
nrow(nps_ref[which(nps_ref$network == "mesonet_nyc"), ])

# create map of case studies LCZ + PWS locations + ref locations
lcz <- terra::rast("../input/lcz_conus_demuzere_2020.tif")

map_lcz <- function(lcz, plot_shp, stations) {
  p <- ggplot() +
    tidyterra::geom_spatraster(data = lcz) +
    tidyterra::geom_spatvector(
      data = plot_shp,
      fill = NA,
      size = 2,
      linewidth = .1
    ) +
    labs(fill = latex2exp::TeX("Local Climate Zone")) +
    tidyterra::geom_spatvector(
      data = stations,
      color = "white",
      size = 3
    ) +
    tidyterra::geom_spatvector(
      data = stations,
      aes(color = network, size = network, shape = network)
    ) +
    scale_color_manual(
      values = c("ref" = "cadetblue1", "pws" = "deeppink1")
    ) +
    scale_shape_manual(
      values = c("ref" = 16, "pws" = 18)
    ) +
    scale_size_manual(
      values = c("ref" = 2.5, "pws" = 2.5)
    ) +
    ggplot2::scale_fill_manual(
      values = load_palette("lcz")$col,
      breaks = load_palette("lcz")$num,
      labels = load_palette("lcz")$meaning,
      na.value = NA
    ) +
    ggspatial::annotation_scale(
      location = "tl", pad_x = ggplot2::unit(1, "cm"),
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
      axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = .5),
      plot.caption = ggplot2::element_text(size = 10),
      legend.position = "right",
      legend.text = ggplot2::element_text(size = 20),
      legend.title = ggplot2::element_text(size = 20),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  return(p)
}

cs <- c("npw", "nps", "phoe", "tri")
for (i in seq_along(cs)) {
  input <- get(paste0("input_", cs[[i]]))
  area_rect_p <- input$area_rect |>
    terra::project(lcz)
  ref <- input$ref[c("lon", "lat", "site_id", "geometry")] |>
    dplyr::distinct() |>
    terra::vect()
  ref$network <- "ref"
  cws <- input$cws[c("lon", "lat", "site_id", "geometry")] |>
    dplyr::distinct() |>
    terra::vect(geom = c("lon", "lat"), crs = "epsg:4326", keepgeom = FALSE)
  cws$network <- "pws"
  stations <- rbind(ref, cws)
  stations <- stations[which(stations$site_id != "Maricopa"), ]
  lcz_i <- input$lcz
  terra::values(lcz_i) <- as.factor(terra::values(lcz_i))
  p <- map_lcz(lcz_i, input$plot_shp, stations)
  assign(paste0("p_lcz_", cs[[i]]), p)
}
p_lcz <- ggpubr::ggarrange(
  p_lcz_npw,
  p_lcz_nps,
  p_lcz_phoe,
  p_lcz_tri,
  nrow = 2,
  ncol = 2,
  legend = "right",
  common.legend = TRUE
)
ggsave(
  plot = p_lcz,
  "./graphs/lcz_cws_and_ref.pdf",
  width = 14,
  height = 10,
  dpi = 300
)

map_uhi <- function(uhi, plot_shp) {
  p <- ggplot() +
    tidyterra::geom_spatraster(data = uhi) +
    tidyterra::geom_spatvector(
      data = plot_shp,
      fill = NA,
      size = 2,
      linewidth = .1
    ) +
    labs(fill = latex2exp::TeX("$UHI$(°C)")) +
    ggplot2::scale_fill_gradientn(
      colours = load_palette("temp_ipcc"),
      na.value = NA,
      limits = c(-9, 9),
      breaks = seq(-9, 9, by = 1)
    ) +
    guides(fill = guide_colourbar(barwidth = 30, barheight = 1.5)) +
    ggspatial::annotation_scale(
      location = "tl", pad_x = ggplot2::unit(1, "cm"),
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
      axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = .5),
      plot.caption = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 20),
      legend.title = ggplot2::element_text(size = 20),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  return(p)
}
for (cs in c("npw", "nps", "phoe", "tri")){
  if (cs %in% c("nps", "npw")) {
    city <- c("Philadelphia", "New York City")
  } else if (cs %in% c("phoe")) {
    city <- "Phoenix"
  } else if (cs %in% c("tri")) {
    city <- c("Raleigh", "Durham")
  }
  uhi <- get(paste0("uhiavg_", cs))
  raster <- get(paste0("raster_", cs))
  uhi_t <- raster - terra::global(raster, "mean", na.rm = TRUE)$mean
  # /!\ 10UTC = 5am in all case studies except for Phoenix
  uhi_night <- uhi_t[[which(lubridate::hour(terra::time(uhi_t)) == 10)]] |>
    terra::mean()
  uhi_corr <- get(paste0("uhiavg_corr_", cs))
  plot_shp <- get(paste0("input_", cs))$plot_shp
  p_uhi <- map_uhi(uhi, plot_shp) +
    tidyterra::geom_spatvector(
      data = pts[which(pts$city %in% city)],
      fill = NA,
      aes(shape = type),
      size = 6,
      stroke = 1.5,
      color = "black",
      linewidth = .1
    ) +
    scale_shape_manual(values = shape_type)
  p_uhi_night <- map_uhi(uhi_night, plot_shp) +
    tidyterra::geom_spatvector(
      data = pts[which(pts$city %in% city)],
      fill = NA,
      aes(shape = type),
      size = 6,
      stroke = 1.5,
      color = "black",
      linewidth = .1
    ) +
    scale_shape_manual(values = shape_type)
  p_uhi_corr <- map_uhi(uhi_corr, plot_shp) +
    tidyterra::geom_spatvector(
      data = pts[which(pts$city %in% city)],
      fill = NA,
      aes(shape = type),
      size = 6,
      stroke = 1.5,
      color = "black",
      linewidth = .1
    ) +
    scale_shape_manual(values = shape_type)
  assign(paste0("p_uhi_", cs), p_uhi)
  assign(paste0("p_uhi_corr_", cs), p_uhi_corr)
  assign(paste0("p_uhi_night_", cs), p_uhi_night)
}
p_uhi <- ggpubr::ggarrange(
  p_uhi_npw,
  p_uhi_nps,
  p_uhi_phoe,
  p_uhi_tri,
  nrow = 2,
  ncol = 2,
  legend = "top",
  common.legend = TRUE
)
p_uhi
ggsave(
  plot = p_uhi,
  "./graphs/uhi.pdf",
  width = 12,
  height = 12,
  dpi = 300
)
ggsave(
  plot = p_uhi_nps +
    guides(
      fill = guide_colourbar(barwidth = 30, barheight = 1.5),
      shape = "none"
    ),
  "./graphs/uhi_nps.pdf",
  width = 8,
  height = 8,
  dpi = 300
)
p_uhi_night <- ggpubr::ggarrange(
  p_uhi_night_npw,
  p_uhi_night_nps,
  p_uhi_night_phoe,
  p_uhi_night_tri,
  nrow = 2,
  ncol = 2,
  legend = "top",
  common.legend = TRUE
)
p_uhi_night
ggsave(
  plot = p_uhi_night,
  "./graphs/uhi_night.pdf",
  width = 12,
  height = 12,
  dpi = 300,
  bg = "white"
)
ggsave(
  plot = p_uhi_night_nps +
    guides(
      fill = guide_colourbar(barwidth = 30, barheight = 1.5),
      shape = "none"
    ),
  "./graphs/uhi_night_nps.pdf",
  width = 7,
  height = 7,
  dpi = 300,
  bg = "white"
)
p_uhi_corr <- ggpubr::ggarrange(
  p_uhi_corr_npw,
  p_uhi_corr_nps,
  p_uhi_corr_phoe,
  p_uhi_corr_tri,
  nrow = 2,
  ncol = 2,
  legend = "top",
  common.legend = TRUE
)
p_uhi_corr
ggsave(
  plot = p_uhi_corr,
  "./graphs/uhi_corr.pdf",
  width = 12,
  height = 12,
  dpi = 300
)


# Only one timestamp
uhi_t <- raster_nps - terra::global(raster_nps, "mean", na.rm = TRUE)$mean
uhi_time <- uhi_t[[which(lubridate::hour(terra::time(uhi_t)) == 10 &
  lubridate::day(terra::time(uhi_t)) == 2)]]
plot_shp <- input_nps$plot_shp
city <- c("Philadelphia", "New York City")
p_uhi <- map_uhi(uhi_time, plot_shp) +
    tidyterra::geom_spatvector(
      data = pts[which(pts$city %in% city)],
      fill = NA,
      aes(shape = type),
      size = 6,
      stroke = 1.5,
      color = "black",
      linewidth = .1
    ) +
    scale_shape_manual(values = shape_type)
p_uhi
ggsave(
  plot = p_uhi,
  "./graphs/uhi_nps_2024070210UTC.pdf",
  width = 7,
  height = 7,
  dpi = 300
)


# extract timeseries at each point
loc_cs <- list("pw", "nw", "ps", "ns", "phoe", "ral", "durh")
city <- list(
  "Philadelphia",
  "New York City",
  "Philadelphia",
  "New York City",
  "Phoenix",
  "Raleigh",
  "Durham"
)
raster <- list(
  raster_npw,
  raster_npw,
  raster_nps,
  raster_nps,
  raster_phoe,
  raster_tri,
  raster_tri
)
month <- list(
  "01/2021",
  "01/2021",
  "07/2024",
  "07/2024",
  "07/2023",
  "07/2021",
  "07/2021"
)
local_tz <- list(
  "America/New_York",
  "America/New_York",
  "America/New_York",
  "America/New_York",
  "America/Phoenix",
  "America/New_York",
  "America/New_York"
)
timeseries <- list()
timeseries_w <- list()
timeseries_day_w <- list()
for (i in 1:7) {
  timeserie <- terra::extract(
    raster[[i]],
    pts[which(pts$city == city[[i]]), ],
    ID = FALSE
  )
  ntimeserie <- ncol(timeserie)
  colnames(timeserie) <- terra::time(raster[[i]]) |>
    as.numeric() |>
    strftime(format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  timeserie$type <- pts[which(pts$city == city[[i]]), ]$type
  timeserie$city <- pts[which(pts$city == city[[i]]), ]$city
  timeserie$month <- month[[i]]
  timeserie <- timeserie |>
    tidyr::pivot_longer(
      cols = 1:ntimeserie,
      names_to = "time",
      values_to = "temp"
    )
  timeserie$time <- as.POSIXct(timeserie$time, tz = "UTC")
  timeserie$local_time <- lubridate::with_tz(
    timeserie$time,
    tzone = local_tz[[i]]
  )
  timeserie_w <- timeserie |>
    tidyr::pivot_wider(
      names_from = type,
      values_from = temp
    )
  timeserie_w$uhi <- timeserie_w$urb - timeserie_w$rur
  timeserie_day <- timeserie |>
    dplyr::group_by(
      utc = hour(time),
      hour = hour(local_time),
      type, city, month
    ) |>
    dplyr::summarise(med_temp = median(temp))
  timeserie_day_w <- timeserie_day |>
    tidyr::pivot_wider(
      names_from = type,
      values_from = med_temp
    )
  timeseries[[i]] <- timeserie
  timeseries_w[[i]] <- timeserie_w
  timeseries_day_w[[i]] <- timeserie_day_w
}
timeseries <- do.call("rbind", timeseries)
timeseries_w <- do.call("rbind", timeseries_w)
timeseries_day_w <- do.call("rbind", timeseries_day_w)

p_uhi <- ggplot(timeseries_day_w) +
  geom_line(
    aes(
      x = hour,
      y = urb - rur,
      group = interaction(city, month),
      linetype = month
    ),
    color = "black",
    linewidth = 1
  ) +
  labs(
    x = "Local time (h)",
    y = latex2exp::TeX("$T2M_{urb} - T2M_{rur}$ (°C)"),
    linetype = ""
  ) +
  facet_wrap(vars(city), ncol = 5) +
  geom_hline(yintercept = 0, color = "red") +
  guides(linetype = guide_legend(keywidth = unit(2, "cm"))) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 20),
    strip.text.x = element_text(size = 20),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 20),
    legend.title = ggplot2::element_text(size = 20),
    legend.position = "top",
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_uhi
ggsave(
  plot = p_uhi,
  "./graphs/uhi_avg_timeserie.pdf",
  width = 15,
  height = 5,
  dpi = 300
)

# plot timeserie for one casestudy
ts <- as.POSIXct("2024-07-01 00:00:00", tz = "UTC")
te <- as.POSIXct("2024-07-31 23:59:59", tz = "UTC")
df_plot <- timeseries[which(between(timeseries$time, ts, te)), ]
df_plot <- df_plot[which(df_plot$city == "New York City"), ]
p_ts_eg <- ggplot(df_plot) +
  geom_line(
    aes(
      x = local_time,
      y = temp,
      group = interaction(type, city),
      color = type
    ),
    linewidth = 1.5
  ) +
  geom_hline(yintercept = 32.22, color = "black") +
  labs(
    x = "",
    y =  latex2exp::TeX("$T2M_{BHM}$ (°C)")
  ) +
  scale_linetype_manual(values = linetype_type) +
  scale_color_manual(values = color_type) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 15),
    strip.text.x = element_text(size = 15),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 15),
    legend.title = ggplot2::element_text(size = 15),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_ts_eg
ggsave(
  plot = p_ts_eg,
  "./graphs/pred_timeserie_eg.pdf",
  width = 20,
  height = 10,
  dpi = 300
)

# UHI tileplots
mytile <- function(df, fill) {
  local_time <- NULL
  plot <- ggplot(
    df,
    aes(
      x = lubridate::hour(local_time),
      y = as.Date(local_time),
      fill = .data[[fill]]
    )
  ) +
    geom_tile() +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 6, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 12, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 18, linetype = "dashed", size = 0.2) +
    geom_vline(xintercept = 24, linetype = "dashed", size = 0.2) +
    scale_x_continuous(
      breaks = c(0, 6, 12, 18, 24),
      labels = as.character(c(0, 6, 12, 18, 24))
    ) +
    labs(
      x = "Local time (h)",
      y = "",
      fill = latex2exp::TeX("$T2M_{urb} - T2M_{rur}$ (°C)")
    ) +
    scale_y_date(
      date_labels = "%m/%d", date_breaks = "7 days",
    ) +
    scale_fill_stepsn(
      colours = load_palette("temp_ipcc"),
      breaks = seq(-9, 9, 1),
      limits = c(-9, 9),
      na.value = "grey"
    ) +
    guides(fill = guide_colourbar(barwidth = 45, barheight = 1.5)) +
    theme(
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22, angle = 90, hjust = 0.5),
      axis.title.x = element_text(size = 24),
      axis.title.y = element_text(size = 22),
      legend.key.width = unit(1, "cm"),
      panel.grid.major = element_line(color = "grey", size = 0.2),
      legend.text = element_text(size = 16),
      plot.caption = element_text(size = 14),
      legend.title = element_text(size = 18),
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "white")
    ) +
    guides(linetype = guide_legend(nrow = 2))

  return(plot)
}

p_nyw <- timeseries_w[which(
  timeseries_w$city == "New York City" &
    timeseries_w$month == "01/2021"
), ] |>
  mytile("uhi")
p_phw <- timeseries_w[which(
  timeseries_w$city == "Philadelphia" &
    timeseries_w$month == "01/2021"
), ] |>
  mytile("uhi")
p_nys <- timeseries_w[which(
  timeseries_w$city == "New York City" &
    timeseries_w$month == "07/2024"
), ] |>
  mytile("uhi")
p_phs <- timeseries_w[which(
  timeseries_w$city == "Philadelphia" &
    timeseries_w$month == "07/2024"
), ] |>
  mytile("uhi")
p_pho <- timeseries_w[which(
  timeseries_w$city == "Phoenix" &
    timeseries_w$month == "07/2023"
), ] |>
  mytile("uhi")
p_ral <- timeseries_w[which(
  timeseries_w$city == "Raleigh" &
    timeseries_w$month == "07/2021"
), ] |>
  mytile("uhi")
p_dur <- timeseries_w[which(
  timeseries_w$city == "Durham" &
    timeseries_w$month == "07/2021"
), ] |>
  mytile("uhi")
tile_uhi <- ggpubr::ggarrange(
  p_nyw,
  p_phw,
  p_nys,
  p_phs,
  p_pho,
  p_ral,
  p_dur,
  ncol = 7,
  common.legend = TRUE,
  legend = "bottom"
)
ggsave(
  plot = tile_uhi,
  "./graphs/tile_uhi.pdf",
  width = 20,
  height = 7,
  dpi = 300
)
tile_uhi_nps <- ggpubr::ggarrange(
  p_phs +
    guides(fill = guide_colourbar(barwidth = 22, barheight = 1.5)),
  p_nys +
    guides(fill = guide_colourbar(barwidth = 22, barheight = 1.5)),
  ncol = 2,
  common.legend = TRUE,
  legend = "bottom"
)
ggsave(
  plot = tile_uhi_nps,
  "./graphs/tile_uhi_nps.pdf",
  width = 11,
  height = 9,
  dpi = 300
)

# ggridges
ref_eval_npw$cs <- "Phi/NYC 01/2021"
ref_eval_nps$cs <- "Phi/NYC 07/2024"
ref_eval_phoe$cs <- "Phoenix 07/2023"
ref_eval_tri$cs <- "Triangle 07/2021"
ref_eval_tri$network <- "ECONET"
cols <- c(
  "time",
  "lon_pred",
  "lat_pred",
  "lon",
  "lat",
  "site_id",
  "temp",
  "network",
  "pred_mean",
  "geometry",
  "res",
  "cs"
)
ref <- rbind(
  ref_eval_npw[, ..cols],
  ref_eval_nps[, ..cols],
  ref_eval_phoe[, ..cols],
  ref_eval_tri[, ..cols]
)
ref$utc <- as.factor(hour(ref$time))
ref$tz <- lutz::tz_lookup_coords(ref$lat, ref$lon)
ref$local_hour <- lubridate::with_tz(ref$time, tz = ref$tz) |>
  lubridate::hour() |>
  as.factor()
ref <- as.data.frame(ref)
# ref_long is a concatenated df with res, cs, local_hour
p_res_ridges <- ggplot(ref) +
  geom_density_ridges_gradient(
    aes(x = res, y = local_hour, fill = stat(x)),
    scale = 3,
    rel_min_height = 0.01
  ) +
  #geom_density_ridges(aes(x = res, y = utc), scale = 5) +
  facet_wrap(vars(cs), ncol = 4) +
  geom_vline(xintercept = 0, color = "darkgreen") +
  geom_vline(xintercept = -1, color = "darkgreen", linetype = "dashed") +
  geom_vline(xintercept = 1, color = "darkgreen", linetype = "dashed") +
  ggplot2::scale_fill_gradientn(
    name = latex2exp::TeX("$T2M_{BHM} - T2M_{ref}$ (°C)"),
    colours = load_palette("temp_ipcc"),
    na.value = NA,
    limits = c(-8, 8),
    breaks = seq(-8, 8, by = 1)
  ) +
  xlab(latex2exp::TeX("$T2M_{BHM} - T2M_{ref}$ (°C)")) +
  ylab("Local time (h)") +
  guides(fill = guide_colourbar(barwidth = 1.5, barheight = 30)) +
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 24),
    axis.text = ggplot2::element_text(size = 18),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 18),
    legend.title = ggplot2::element_text(size = 18),
    strip.text.x = element_text(size = 18),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_res_ridges
ggsave(
  plot = p_res_ridges,
  "./graphs/residual_ridges.pdf",
  width = 12,
  height = 7,
  dpi = 300
)

p_res_ridges_nps <- ggplot(ref[which(ref$cs == "Phi/NYC 07/2024"), ]) +
  geom_density_ridges_gradient(
    aes(x = res, y = local_hour, fill = stat(x)),
    scale = 3,
    rel_min_height = 0.01
  ) +
  geom_vline(xintercept = 0, color = "darkgreen") +
  geom_vline(xintercept = -1, color = "darkgreen", linetype = "dashed") +
  geom_vline(xintercept = 1, color = "darkgreen", linetype = "dashed") +
  ggplot2::scale_fill_gradientn(
    name = "",
    colours = load_palette("temp_ipcc"),
    na.value = NA,
    limits = c(-6, 6),
    breaks = seq(-6, 6, by = 1)
  ) +
  scale_x_continuous(limits = c(-6, 6), breaks = seq(-6, 6, by = 1)) +
  xlab(latex2exp::TeX("$T2M_{BHM} - T2M_{ref}$ (°C)")) +
  ylab("Local time (h)") +
  guides(fill = guide_colourbar(barwidth = 1.5, barheight = 30)) +
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 24),
    axis.text = ggplot2::element_text(size = 18),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 18),
    legend.title = ggplot2::element_text(size = 18),
    strip.text.x = element_text(size = 18),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_res_ridges_nps
ggsave(
  plot = p_res_ridges_nps,
  "./graphs/residual_ridges_nps.pdf",
  width = 6,
  height = 12,
  dpi = 300
)

calculate_rmse <- function(res) {
  sqrt(mean(res^2, na.rm = TRUE))
}

calculate_rmse(ref$res)


rmse_by_cs <- ref |>
  dplyr::group_by(cs) |>
  dplyr::summarise(RMSE = calculate_rmse(res))
rmse_by_cs

rmse_by_site_id <- ref |>
  dplyr::group_by(site_id, cs) |>
  dplyr::summarise(RMSE = calculate_rmse(res))
rmse_by_site_id
ggplot(rmse_by_site_id) +
  geom_histogram(aes(x = RMSE))

rmse_by_cs_localhour <- ref |>
  dplyr::group_by(cs, local_hour) |>
  dplyr::summarise(RMSE = calculate_rmse(res))
rmse_by_cs_localhour

p_rmse <- ggplot(rmse_by_cs_localhour) +
  geom_line(
    aes(
      x = local_hour,
      y = RMSE,
      group = cs,
      color = cs
    ),
    linewidth = 1
  ) +
  scale_color_manual(values = color_cs_full) +
  labs(color = "", x = "Local time(h)") +
  guides(color = guide_legend(keywidth = unit(2, "cm"))) +
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 24),
    axis.text = ggplot2::element_text(size = 18),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 18),
    legend.title = ggplot2::element_text(size = 18),
    strip.text.x = element_text(size = 18),
    legend.position = "top",
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
ggsave(
  plot = p_rmse,
  "./graphs/rmse.pdf",
  width = 13,
  height = 5,
  dpi = 300
)

rmse_by_group <- ref |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(RMSE = calculate_rmse(res))

# Print the RMSE by group
print(rmse_by_group)

ggplot(ref) +
  geom_density_ridges_gradient(
    aes(x = res, y = site_id, fill = stat(x)),
    scale = 8,
    rel_min_height = 0.01
  ) +
  facet_wrap(vars(cs), ncol = 4) +
  geom_vline(xintercept = 0, color = "darkgreen") +
  geom_vline(xintercept = -1, color = "darkgreen", linetype = "dashed") +
  geom_vline(xintercept = 1, color = "darkgreen", linetype = "dashed") +
  ggplot2::scale_fill_gradientn(
    name = "Residuals [C]",
    colours = load_palette("temp_ipcc"),
    na.value = NA,
    limits = c(-8, 8),
    breaks = seq(-8, 8, by = 1)
  ) +
  xlab(latex2exp::TeX("$T2M_{BHM} - T2M_{ref}$")) +
  ggplot2::theme(
    axis.text.y = element_blank(),
    axis.text = ggplot2::element_text(size = 12),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )

ggplot(ref) +
  geom_point(aes(x = pred_mean, y = temp), alpha = .1) +
  geom_abline(color = "red")

ggplot(ref) +
  geom_point(aes(x = temp, y = res), alpha = .1) +
  coord_equal()

# PLOT PRO RMSE AND SD background
map_error_sd <- function(sd, plot_shp, pro, cws) {
  rmse <- NULL
  p <- ggplot() +
    tidyterra::geom_spatraster(data = sd) +
    tidyterra::geom_spatvector(
      data = plot_shp,
      fill = NA,
      size = 2,
      linewidth = .1
    ) +
    tidyterra::geom_spatvector(
      data = pro,
      color = "black",
      size = 4.4
    ) +
    tidyterra::geom_spatvector(
      data = cws,
      fill = "black",
      size = 1
    ) +
    tidyterra::geom_spatvector(
      data = pro,
      aes(color = rmse),
      size = 4
    ) +
    labs(fill = latex2exp::TeX("$\\sigma_{BHM}$ (°C)"), color = "RMSE (°C)") +
    ggplot2::scale_fill_stepsn(
      colours = c("seashell", "darkolivegreen4"),
      na.value = NA,
      limits = c(0, 2.5),
      breaks = seq(0, 2.5, by = .5)
    ) +
    ggplot2::scale_color_stepsn(
      colours = c("white", "hotpink4"),
      na.value = NA,
      limits = c(0, 2),
      breaks = seq(0, 2, by = 0.5)
    ) +
    guides(
      fill = guide_colourbar(
        barwidth = 1.5,
        barheight = 10,
        title.vjust = 2
      ),
      color = guide_colourbar(
        barwidth = 1.5,
        barheight = 5,
        title.vjust = 2
      )
    ) +
    ggspatial::annotation_scale(
      location = "tl", pad_x = ggplot2::unit(1, "cm"),
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
      axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = .5),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 20),
      legend.title = ggplot2::element_text(size = 20),
      legend.spacing.y = unit(1, "cm"),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  return(p)
}

for (cs in c("npw", "nps", "phoe", "tri")){
  if (cs %in% c("nps", "npw")) {
    city <- c("Philadelphia", "New York City")
  } else if (cs %in% c("phoe")) {
    city <- "Phoenix"
  } else if (cs %in% c("tri")) {
    city <- c("Raleigh", "Durham")
  }
  input <- get(paste0("input_", cs))
  cws <- input$cws[, c("site_id", "lon", "lat")] |>
    dplyr::distinct() |>
    terra::vect(geom = c("lon", "lat"), crs = "epsg:4326")
  pro <- get(paste0("ref_eval_", cs))
  pro <- pro[
    ,
    .(rmse = sqrt(mean(.SD$res^2, na.rm = TRUE))),
    by = .(site_id, lat, lon),
    .SDcols = "res"
  ] |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    terra::vect()
  p <- map_error_sd(
    get(paste0("sdavg_", cs)),
    get(paste0("input_", cs))$plot_shp,
    pro,
    cws
  ) # +
  #   tidyterra::geom_spatvector(
  #     data = pts[which(pts$city %in% city), ],
  #     fill = NA,
  #     aes(shape = type),
  #     size = 7,
  #     stroke = 1.5,
  #     color = "royalblue3",
  #     linewidth = .1
  #   ) +
  #   scale_shape_manual(values = shape_type)
  assign(
    paste0("pred_sd_", cs),
    p
  )
}
p_sd <- ggpubr::ggarrange(
  pred_sd_npw,
  pred_sd_nps,
  pred_sd_phoe,
  pred_sd_tri,
  nrow = 2,
  ncol = 2,
  legend = "right",
  common.legend = TRUE
)
p_sd
ggsave(
  plot = p_sd,
  "./graphs/sd.pdf",
  width = 12,
  height = 10,
  dpi = 300
)
ggsave(
  plot = pred_mean_corrnps,
  "./graphs/sd_nps.pdf",
  width = 9,
  height = 6,
  dpi = 300
)

# info analysis
plot(terra::time(raster_sd_phoe), raster_sd_phoe[2], type = "l")
plot(terra::time(raster_sd_phoe), raster_sd_phoe[2000], type = "l")
plot(terra::time(raster_sd_tri), raster_sd_tri[2], type = "l")
plot(terra::time(raster_sd_tri), raster_sd_tri[2000], type = "l")
plot(terra::time(raster_sd_npw), raster_sd_npw[2], type = "l")
plot(terra::time(raster_sd_npw), raster_sd_npw[2000], type = "l")
plot(terra::time(raster_sd_nps), raster_sd_nps[2], type = "l")
plot(terra::time(raster_sd_nps), raster_sd_nps[2000], type = "l")

input_npw$info$cs <- "npw"
input_nps$info$cs <- "nps"
input_phoe$info$cs <- "phoe"
input_tri$info$cs <- "tri"
info <- rbind(
  input_npw$info,
  input_nps$info,
  input_phoe$info,
  input_tri$info
)

tvar_coeffs <- list()
cs <- c("npw", "nps", "phoe", "tri")
for (i in seq_along(cs)) {
  info <- get(paste0("input_", cs[[i]]))$info |>
    data.frame()
  col_mean_fixed <- colnames(info)[grep("^local_hour.*_mean$", colnames(info))]
  col_sd_fixed <- colnames(info)[grep("^local_hour.*_sd$", colnames(info))]
  covar_names <- unique(sub(".*\\.(.*?)_.*", "\\1", col_mean_fixed))
  coeffs <- data.frame(
    rep(0:23, length(covar_names)),
    rep(covar_names, each = 24),
    t(info[1, col_mean_fixed]),
    t(info[1, col_sd_fixed])
  )
  colnames(coeffs) <- c("local_hour", "var", "mean", "sd")
  coeffs$cs <- cs[[i]]
  tvar_coeffs[[i]] <- coeffs
}
tvar_coeffs <-  do.call("rbind", tvar_coeffs)
tvar_coeffs[
  which(tvar_coeffs$cs == "phoe" & tvar_coeffs$var == "fch"),
]$mean <- NA
tvar_coeffs[
  which(tvar_coeffs$cs == "phoe" & tvar_coeffs$var == "fch"),
]$sd <- NA

var <- c("elev", "fch", "imp")
prior_mean <- c(-0.006, 0, 0)
prior_sd <- c(sqrt(1 / 10000), sqrt(1 / 100), sqrt(1 / 200))
yn <- c(-0.015, -0.1, -0.035)
yx <- - yn
p <- list()
for (i in seq_along(var)) {
  x_values <- data.frame(x = seq(yn[[i]], -yn[[i]], length.out = 100))
  p[[i]] <- ggplot(x_values, aes(x =  x)) +
    stat_function(
      fun = dnorm,
      args = list(mean = prior_mean[[i]], sd = prior_sd[[i]]),
      color = "black",
      size = 1
    ) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    labs(
      x = "",
      y = ""
    ) +
    coord_flip() +
    scale_y_reverse() +
    theme(
      axis.title = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.text.x = element_blank(),
      plot.caption = element_text(size = 18),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey")
    )
}
p_priors <- ggpubr::ggarrange(p[[1]], p[[2]], p[[3]], nrow = 3)
p_priors
ggsave(
  plot = p_priors,
  "./graphs/time_varying_coefficients_prior.pdf",
  width = 3,
  height = 9,
  dpi = 300
)

var <- c("elev", "fch", "imp")
blank_df <- data.frame(cbind(var, yn, yx))
names(blank_df) <- c("var", "yn", "yx")
blank_df <- blank_df |>
  tidyr::pivot_longer(
    cols = c(yn, yx),
    names_to = "type",
    values_to = "y"
  )
blank_df$x <- 2
blank_df$y <- as.numeric(blank_df$y)

p_tvar_coeffs <- ggplot(tvar_coeffs) +
  geom_line(aes(x = local_hour, y = mean, color = cs, group = cs)) +
  geom_ribbon(
    aes(
      x = local_hour,
      ymin = mean - 2 * sd,
      ymax = mean + 2 * sd, fill = cs
    ),
    alpha = 0.1
  ) +
  geom_blank(data = blank_df, aes(x = x, y = y)) +
  facet_wrap(vars(var), nrow = 3, scales = "free_y") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_color_manual("", values = color_cs) +
  scale_fill_manual("", values = color_cs) +
  xlab("Local time (h)") +
  ylab(latex2exp::TeX("coef $\\mu\\pm 2\\sigma$")) +
  scale_x_continuous(
    breaks = seq(0, 23, 1)
  ) +
  guides(
    color = guide_legend(keywidth = unit(2, "cm")),
    fill = guide_legend(keywidth = unit(2, "cm"))
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    axis.title = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(
      size = 12,
      hjust = .5
    ),
    strip.text.x = element_text(size = 18),
    plot.caption = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey")
  )
p_tvar_coeffs
ggsave(
  plot = p_tvar_coeffs,
  "./graphs/time_varying_coefficients_posterior.pdf",
  width = 10,
  height = 8,
  dpi = 300
)

info <- rbind(
  input_nps$info,
  input_npw$info,
  input_phoe$info,
  input_tri$info
) |>
  data.frame()
era5_info <- info |>
  dplyr::select(starts_with("era5_"), "cs")
era5_info_long <- era5_info |>
  tidyr::pivot_longer(
    cols = starts_with("era5_"),
    names_to = "var",
    values_to = "value"
  )
era5_info_long$stat <- sub(".*_(.*)", "\\1", era5_info_long$var)
era5_info_long$var <- sub("^[^_]*_([^_]*)_.*$", "\\1", era5_info_long$var)
era5_info_wide <- era5_info_long |>
  tidyr::pivot_wider(
    names_from = stat,  # Column to use for new column names
    values_from = value  # Column to use for values
  )

row_to_normal_df <- function(mean, sd, var, cs) {
  sd <- as.numeric(sd)
  mean <- as.numeric(mean)
  x <- seq(mean - 4 * sd, mean + 4 * sd, length.out = 100)
  y <- dnorm(x, mean = mean, sd = sd)
  df <- data.frame(cbind(x, y, rep(var, 100), rep(cs, 100)))
  names(df) <- c("x", "y", "var", "cs")
  return(df)
}
result_list <- apply(
  era5_info_wide,
  1,
  function(x) row_to_normal_df(x[3], x[4], x[2], x[1])
)
result_df <- do.call(rbind, result_list)
result_df$x <- as.numeric(result_df$x)
result_df$y <- as.numeric(result_df$y)

p_era5 <- ggplot(result_df, aes(x = x, y = y)) +
  geom_line(aes(group = cs, color = cs)) +
  facet_wrap(vars(var), scales = "free", ncol = 6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = color_cs) +
  labs(x = "", y = "", color = "") +
  guides(color = guide_legend(keywidth = unit(2, "cm"))) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 20),
    strip.text.x = element_text(size = 20),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 20),
    legend.title = ggplot2::element_text(size = 20),
    legend.position = "top",
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
ggsave(
  plot = p_era5,
  "./graphs/era5_coefficients_posterior.pdf",
  width = 13,
  height = 3,
  dpi = 300
)

# density of predicition at one given location
loc <- ref_eval_nps[which(ref_eval_nps$site_id == "MANH"), ]
time_t <- as.POSIXct("2024-07-20 16:00:00", tz = "America/New_York")
mean <- loc[which(loc$time == time_t), ]$pred_mean
sd <- loc[which(loc$time == time_t), ]$pred_sd
t_ref <- loc[which(loc$time == time_t), ]$temp
sd_shaded <- function(x, mean, sd, bound) {
  y <- dnorm(x, mean = mean, sd = sd)
  y[x < mean - bound] <- NA
  y[x > mean + bound] <- NA
  return(y)
}
x_values <- data.frame(x = seq(mean - 4 * sd, mean + 4 * sd, length.out = 100))
p_gauss <- ggplot(x_values, aes(x = x)) +
  stat_function(
    fun = sd_shaded,
    args = list(mean = mean, sd = sd, bound = sd * 2),
    geom = "area",
    fill = "ivory2",
    alpha = 1
  ) +
  stat_function(
    fun = sd_shaded,
    args = list(mean = mean, sd = sd, bound = sd),
    geom = "area",
    fill = "ivory",
    alpha = 1
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean, sd = sd),
    color = "hotpink2",
    size = 1
  ) +
  geom_vline(
    xintercept = mean,
    linetype = "dotted",
    color = "hotpink3",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = t_ref,
    linetype = "dashed",
    color = "coral",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = mean,
    y = 0.2,
    angle = 90,
    vjust = 1.5,
    label = latex2exp::TeX("$\\bar{p(z_{mt}|\\textbf{y})} = T2M_{BHM}$"),
    color = "hotpink3",
    size = 6
  ) +
  annotate(
    "text",
    x = t_ref,
    y = 0.25,
    angle = 90,
    vjust = 1.5,
    label = latex2exp::TeX("$T2M_{ref}$ (if available)"),
    color = "coral",
    size = 6
  ) +
  annotate(
    "text",
    x = mean - 2.5 * sd,
    y = 0.23,
    label = latex2exp::TeX("$p(z_{mt}|\\textbf{y})$"),
    color = "hotpink2",
    size = 8
  ) +
  labs(
    x = "",
    y = ""
  ) +
  scale_x_continuous(
    breaks = c(
      mean - 2 * sd,
      mean + 2 * sd,
      mean - sd,
      mean + sd,
      round(mean, 1)
    ),
    labels = c(
      latex2exp::TeX("$\\mu - 2\\sigma$"),
      latex2exp::TeX("$\\mu + 2\\sigma$"),
      latex2exp::TeX("$\\mu - \\sigma$"),
      latex2exp::TeX("$\\mu + \\sigma$"),
      latex2exp::TeX("$\\mu$")
    )
  ) +
  theme(
    title = element_text(size = 15),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_blank(),
    plot.caption = element_text(size = 18),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank()
  )
p_gauss
ggsave(
  plot = p_gauss,
  "./graphs/eg_posterior_mt.pdf",
  width = 9,
  height = 3,
  dpi = 300
)

map_hot_night_duration <- function(duration_rast, plot_shp, thresh) {
  p <- p <- ggplot() +
    tidyterra::geom_spatraster(data = duration_rast) +
    tidyterra::geom_spatvector(
      data = plot_shp,
      fill = NA,
      size = 2,
      linewidth = .1
    ) +
    labs(
      fill = latex2exp::TeX("$q_{0.5}(\\tau)\\phantom{x}$(h)"),
      title = latex2exp::TeX(sprintf("$\\delta_{July} = %f$°C", thresh))
    ) +
    ggplot2::scale_fill_stepsn(
      colours = c("white", "darkslateblue"),
      na.value = NA,
      limits = c(0, 9),
      breaks = seq(0, 9, by = 1)
    ) +
    guides(
      fill = guide_colourbar(
        barwidth = 10,
        barheight = 1.5,
        keywidth = 10
      )
    ) +
    ggspatial::annotation_scale(
      location = "tl", pad_x = ggplot2::unit(1, "cm"),
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
      axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = .5),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 20),
      plot.title = ggplot2::element_text(size = 20),
      legend.title = ggplot2::element_text(size = 20), #, hjust = .5),
      legend.position = "bottom",
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  return(p)
}
# Nocturnal exposure maps
cs <- c("nps", "phoe", "tri")
# following is the 0.90 percentile, change to
# 24.6, 32.8, 23.8 for 0.95 percentile
thresh <- c(23.8, 32, 23.3)
for (i in seq_along(cs)) {
  input <- get(paste0("input_", cs[[i]]))
  pred <- input$pred
  pred_night <- pred[which(!(pred$local_hour %in% 7:21)), ]
  pred_night$day <- ifelse(
    pred_night$local_hour < 7,
    lubridate::date(pred_night$time) - 1,
    lubridate::date(pred_night$time)
  )
  pred_night$hot <- pred_night$pred_mean >= thresh[[i]]
  hot_night_d <- pred_night |>
    dplyr::group_by(lat, lon, day) |>
    dplyr::summarise(duration = sum(hot)) |>
    dplyr::group_by(lat, lon) |>
    dplyr::summarise(
      avg_duration = mean(duration, na.rm = TRUE),
      med_duration = median(duration, na.rm = TRUE)
    )
  assign(paste0("df_hot_night_d_", cs[[i]]), hot_night_d)
  hot_night_d$time <- as.POSIXct("1973-02-02 00:00:00")
  hot_night_d <- rasterize_pred(hot_night_d, var = "med_duration")
  if (cs[[i]] %in% c("nps", "npw")) {
    city <- c("Philadelphia", "New York City")
  } else if (cs[[i]] %in% c("phoe")) {
    city <- "Phoenix"
  } else if (cs[[i]] %in% c("tri")) {
    city <- c("Raleigh", "Durham")
  }
  p <- map_hot_night_duration(hot_night_d, input$plot_shp, thresh[[i]]) +
    tidyterra::geom_spatvector(
      data = pts[which(pts$city %in% city)],
      fill = NA,
      aes(shape = type),
      size = 6,
      stroke = 1.5,
      color = "black",
      linewidth = .1
    ) +
    scale_shape_manual(values = shape_type)
  assign(paste0("hot_night_d_", cs[[i]]), hot_night_d)
  assign(paste0("p_hnd_", cs[[i]]), p)
}
p_hnd <- ggpubr::ggarrange(
  p_hnd_nps,
  p_hnd_phoe,
  p_hnd_tri,
  ncol = 3,
  common.legend = TRUE
)
p_hnd
ggsave(
  plot = p_hnd,
  "./graphs/hot_nocturnal_t2m_average_duration.pdf",
  width = 17,
  height = 7,
  dpi = 300
)
ggsave(
  plot = p_hnd_nps,
  "./graphs/hot_nocturnal_t2m_average_duration_nps.pdf",
  width = 7,
  height = 7,
  dpi = 300
)

# Population density
pop <- terra::rast(
  paste0(
    "../input/gpw-v4-population-density-rev11_2020_30_sec_tif/",
    "gpw_v4_population_density_rev11_2020_30_sec.tif"
  )
)


cs <- c("nps", "phoe", "tri")
for (i in seq_along(cs)) {
  df_hot_night_d <- get(paste0("df_hot_night_d_", cs[[i]]))
  myvect <- df_hot_night_d |>
    terra::vect(geom = c("lon", "lat"), crs = "epsg:4326") |>
    terra::project(terra::crs(pop))
  myvect$pop <- terra::extract(
    pop,
    myvect, fun = mean
  )$gpw_v4_population_density_rev11_2020_30_sec
  myvect$log10pop <- log10(myvect$pop)
  mydf <- myvect |>
    sf::st_as_sf(remove = FALSE) |>
    data.frame()
  p_pop <- ggplot(data = mydf, mapping = aes(x = pop, y = avg_duration)) +
    geom_pointdensity(alpha = 1) +
    scale_color_viridis() +
    scale_y_continuous(
      latex2exp::TeX(
        "Avg duration $\\phantom{x}T2M_{noct}\\geqq_{0.9}\\phantom{x}$(h)"
      ),
      breaks = seq(0, 9, 1)
    ) +
    scale_x_log10("Population (log scale)") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 20),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 20),
      plot.title = ggplot2::element_text(size = 20),
      legend.title = ggplot2::element_text(size = 20, hjust = .5),
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  assign(paste0("p_pop_dur_", cs[[i]]), p_pop)
}
p_pop_dur <- ggpubr::ggarrange(
  p_pop_dur_nps,
  p_pop_dur_phoe,
  p_pop_dur_tri,
  nrow = 3
)
ggsave(
  plot = p_pop_dur,
  "./graphs/hot_night_exposure_vs_population.pdf",
  width = 7,
  height = 15,
  dpi = 300
)

# Ridges per population range of average hot night duration
cs <- c("nps", "phoe", "tri")
for (i in seq_along(cs)) {
  df_hot_night_d <- get(paste0("df_hot_night_d_", cs[[i]]))
  myvect <- df_hot_night_d |>
    terra::vect(geom = c("lon", "lat"), crs = "epsg:4326") |>
    terra::project(terra::crs(pop))
  myvect$pop <- terra::extract(
    pop,
    myvect,
    fun = mean
  )$gpw_v4_population_density_rev11_2020_30_sec
  myvect <- myvect[which(!(is.na(myvect$pop))), ]
  myvect$log10pop <- ifelse(myvect$pop <= 1, 0, log10(myvect$pop))
  mydf <- myvect |>
    sf::st_as_sf(remove = FALSE) |>
    data.frame()
  mydf$cs <- cs[[i]]
  assign(paste0("mydf_", cs[[i]]), mydf)
  p_pop_ridges <- ggplot(data = mydf) +
    geom_density_ridges_gradient(
      aes(x = avg_duration, y = factor(floor(log10pop)), fill = after_stat(x)),
      scale = 2,
      rel_min_height = 0.01
    ) +
    scale_fill_stepsn(
      colors = c("white", "darkslateblue"),
      na.value = NA,
      limits = c(0, 9),
      breaks = seq(0, 9, by = 1)
    ) +
    guides(
      fill = guide_colourbar(
        barwidth = 30,
        barheight = 1.5,
        keywidth = 30
      )
    ) +
    scale_x_continuous(
      name = latex2exp::TeX("$\\tau_{AVG}\\phantom{x}$(h)"),
      limits = c(0, 9),
      breaks = seq(0, 9, 1)
    ) +
    scale_y_discrete(
      "Population (log scale)",
      breaks = c(0, 1, 2, 3, 4, 5),
      labels = c("0-1", "10", "100", "1000", "10e3", "10e4")
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 20),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 20),
      plot.title = ggplot2::element_text(size = 20),
      legend.title = ggplot2::element_text(size = 20, hjust = .5),
      legend.position = "bottom",
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  assign(paste0("p_pop_dur_rid_", cs[[i]]), p_pop_ridges)
}
mydf <- rbind(mydf_nps, mydf_phoe, mydf_tri)
p_pop_ridges <- ggplot(data = mydf) +
  geom_density_ridges_gradient(
    aes(x = avg_duration, y = factor(floor(log10pop)), fill = after_stat(x)),
    scale = 2,
    rel_min_height = 0.01
  ) +
  facet_wrap(vars(cs)) +
  scale_fill_stepsn(
    colors = c("white", "darkslateblue"),
    name = latex2exp::TeX("Hot night avg duration (h)"),
    na.value = NA,
    limits = c(0, 9),
    breaks = seq(0, 9, by = 1)
  ) +
  scale_x_continuous(
    name = latex2exp::TeX("$\\tau_{AVG}\\phantom{x}$(h)"),
    limits = c(0, 9),
    breaks = seq(0, 9, 1)
  ) +
  guides(
    fill = guide_colourbar(
      barwidth = 30,
      barheight = 1.5,
      keywidth = 30
    )
  ) +
  scale_y_discrete(
    "Population (log scale)",
    breaks = c(0, 1, 2, 3, 4, 5),
    labels = c("0-1", "10", "10e1", "10e2", "10e3", "10e4")
  ) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 20),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 20),
    plot.title = ggplot2::element_text(size = 20),
    strip.text.x = element_text(size = 20),
    legend.title = ggplot2::element_text(size = 24, hjust = .5),
    legend.position = "bottom",
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_pop_ridges
ggsave(
  plot = p_pop_ridges,
  "./graphs/hot_night_exposure_vs_population_ridges.pdf",
  width = 15,
  height = 7,
  dpi = 300
)


cs <- c("nps", "phoe", "tri")
for (i in seq_along(cs)) {
  uhiavg <- get(paste0("uhiavg_", cs[[i]]))
  myvect <- uhiavg |>
    terra::as.points() |>
    terra::project(terra::crs(pop))
  myvect$pop <- terra::extract(
    pop,
    myvect, fun = mean
  )$gpw_v4_population_density_rev11_2020_30_sec
  myvect <- myvect[which(!(is.na(myvect$pop))), ]
  myvect$log10pop <- ifelse(myvect$pop <= 1, 0, log10(myvect$pop))
  mydf <- myvect |>
    sf::st_as_sf(remove = FALSE) |>
    data.frame()
  mydf$cs <- cs[[i]]
  assign(paste0("mydf_", cs[[i]]), mydf)
  p_pop <- ggplot(data = mydf, mapping = aes(x = pop, y = mean)) +
    geom_pointdensity(alpha = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_viridis() +
    scale_x_log10("Population (log scale)") +
    labs(y = latex2exp::TeX("$UHI_{q0.5}$(°C)")) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 20),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 20),
      plot.title = ggplot2::element_text(size = 20),
      legend.title = ggplot2::element_text(size = 20, hjust = .5),
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  assign(paste0("p_pop_uhiavg_", cs[[i]]), p_pop)
}
p_pop_uhiavg <- ggpubr::ggarrange(
  p_pop_uhiavg_nps,
  p_pop_uhiavg_phoe,
  p_pop_uhiavg_tri,
  nrow = 3
)
ggsave(
  plot = p_pop_uhiavg,
  "./graphs/uhi_exposure_vs_population.pdf",
  width = 7,
  height = 15,
  dpi = 300
)

mydf <- rbind(mydf_nps, mydf_phoe, mydf_tri)
p_pop_ridges <- ggplot(data = mydf) +
  geom_density_ridges_gradient(
    aes(x = mean, y = factor(floor(log10pop)), fill = after_stat(x)),
    scale = 2,
    rel_min_height = 0.01
  ) +
  facet_wrap(vars(cs)) +
  scale_fill_gradientn(
    colours = load_palette("temp_ipcc"),
    na.value = NA,
    limits = c(-9, 9),
    breaks = seq(-9, 9, by = 1)
  ) +
  scale_x_continuous(
    name = latex2exp::TeX("$UHI_{AVG}$(°C)"),
    limits = c(-5, 5),
    breaks = seq(-5, 5, 1)
  ) +
  scale_y_discrete(
    "Population (log scale)",
    breaks = c(0, 1, 2, 3, 4, 5),
    labels = c("0-1", "10", "10e1", "10e2", "10e3", "10e4")
  ) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 20),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 20),
    plot.title = ggplot2::element_text(size = 20),
    strip.text.x = element_text(size = 20),
    legend.title = ggplot2::element_text(size = 20, hjust = .5),
    legend.position = "none",
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_pop_ridges
ggsave(
  plot = p_pop_ridges,
  "./graphs/uhi_exposure_vs_population_ridges.pdf",
  width = 15,
  height = 7,
  dpi = 300
)

p_pop_nps <- ggpubr::ggarrange(
  p_pop_dur_nps,
  p_pop_uhiavg_nps,
  ncol = 2
)
ggsave(
  plot = p_pop_nps,
  "./graphs/heat_exposure_vs_population_nps.pdf",
  width = 10,
  height = 5,
  dpi = 300
)

# consecutive days with TX > threshold and TN > threshold
# (definition of heatwave)
count_heatwave <- function(timeserie) {
  pixel <- rle(as.vector(timeserie)) |>
    unclass() |>
    data.frame()
  pixel <- pixel[which((pixel$values == TRUE) & (pixel$lengths >= 3)), ]
  pixel_nb_htw <- nrow(pixel)
  return(pixel_nb_htw)
}

avg_length_heatwave <- function(timeserie) {
  pixel <- rle(as.vector(timeserie)) |>
    unclass() |>
    data.frame()
  pixel <- pixel[which(pixel$values == TRUE & pixel$lengths >= 3), ]
  pixel_len_htw <- sum(pixel$lengths)
  return(pixel_len_htw)
}

cs <- c("nps", "phoe", "tri")
thresh_day <- c(35.2, 45.1, 36.1) # q95 tx July 1991 - 2020
thresh_night <- c(24.6, 32.8, 23.8)  # q95 tn July 1991 - 2020
for (i in seq_along(cs)) {
  input <- get(paste0("input_", cs[[i]]))
  df_day <- input$pred
  df_day$date <- lubridate::date(df_day$time)
  df_day <- df_day |>
    dplyr::group_by(lat, lon, date) |>
    dplyr::summarise(
      tx = max(pred_mean, na.rm = TRUE)
    )
  df_night <- input$pred
  df_night$date <- ifelse(
    lubridate::hour(df_night$time) <= 6,
    lubridate::date(df_night$time) - 1,
    lubridate::date(df_night$time)
  )
  df_night <- df_night |>
    dplyr::group_by(lat, lon, date) |>
    dplyr::summarise(
      tn = min(pred_mean, na.rm = TRUE)
    )
  df_wide <- merge(df_day, df_night, by = c("lon", "lat", "date"))
  df_wide$hot <- (df_wide$tn >= thresh_night[[i]]) &
    (df_wide$tx >= thresh_day[[i]])
  df_wide <- df_wide[, c("lat", "lon", "date", "hot")] |>
    tidyr::pivot_wider(
      names_from = date,
      values_from = c(hot)
    )
  df_day$hot <- df_day$tx >= thresh_day[[i]]
  df_wide_day <- df_day[, c("lat", "lon", "date", "hot")] |>
    tidyr::pivot_wider(
      names_from = date,
      values_from = c(hot)
    )
  df_night$hot <- df_night$tn >= thresh_night[[i]]
  df_wide_night <- df_night[, c("lat", "lon", "date", "hot")] |>
    tidyr::pivot_wider(
      names_from = date,
      values_from = c(hot)
    )
  df_wide$nb_htw <- apply(
    df_wide,
    1,
    function(x) count_heatwave(x[3:length(x)])
  )
  df_wide$len_htw <- apply(
    df_wide,
    1,
    function(x) avg_length_heatwave(x[3:length(x)])
  )
  df_wide$time <- as.POSIXct("2024-07-01 00:00:00", tz = "UTC")
  htw <- rasterize_pred(df_wide, "nb_htw")
  htw_len <- rasterize_pred(df_wide, "len_htw")
  df_wide_night$nb_htw <- apply(
    df_wide_night,
    1,
    function(x) count_heatwave(x[3:length(x)])
  )
  df_wide_night$len_htw <- apply(
    df_wide_night,
    1,
    function(x) avg_length_heatwave(x[3:length(x)])
  )
  df_wide_night$time <- as.POSIXct("2024-07-01 00:00:00", tz = "UTC")
  htw_night <- rasterize_pred(df_wide_night, "nb_htw")
  htw_night_len <- rasterize_pred(df_wide_night, "len_htw")
  df_wide_day$nb_htw <- apply(
    df_wide_day,
    1,
    function(x) count_heatwave(x[3:length(x)])
  )
  df_wide_day$len_htw <- apply(
    df_wide_day,
    1,
    function(x) avg_length_heatwave(x[3:length(x)])
  )
  df_wide_day$time <- as.POSIXct("2024-07-01 00:00:00", tz = "UTC")
  htw_day <- rasterize_pred(df_wide_day, "nb_htw")
  htw_day_len <- rasterize_pred(df_wide_day, "len_htw")
  assign(paste0("htw_", cs[[i]]), htw)
  assign(paste0("htw_day_", cs[[i]]), htw_day)
  assign(paste0("htw_night_", cs[[i]]), htw_night)
  assign(paste0("htw_len_", cs[[i]]), htw_len)
  assign(paste0("htw_day_len_", cs[[i]]), htw_day_len)
  assign(paste0("htw_night_len_", cs[[i]]), htw_night_len)
}

# map heatwave number
map_heatwave_number <- function(htw_rast, plot_shp) {
  terra::values(htw_rast) <- factor(
    terra::values(htw_rast),
    levels = c("0", "1", "2", "3", "4")
  )
  p <- ggplot() +
    tidyterra::geom_spatraster(data = htw_rast, show.legend = TRUE) +
    tidyterra::geom_spatvector(
      data = plot_shp,
      fill = NA,
      size = 2,
      linewidth = .1
    ) +
    labs(
      fill = latex2exp::TeX("Number of heatwaves")
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "0" = "green",
        "1" = "yellow",
        "2" = "orange",
        "3" = "red",
        "4" = "purple"
      ),
      drop = FALSE,
      na.value = NA,
      na.translate = FALSE
    ) +
    ggspatial::annotation_scale(
      location = "tl", pad_x = ggplot2::unit(1, "cm"),
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
      axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = .5),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 20),
      plot.title = ggplot2::element_text(size = 20),
      legend.title = ggplot2::element_text(size = 20, hjust = .5),
      legend.position = "bottom",
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  return(p)
}

map_heatwave_len <- function(htw_rast, plot_shp) {
  terra::values(htw_rast) <- as.integer(
    terra::values(htw_rast)
  )
  p <- ggplot() +
    tidyterra::geom_spatraster(data = htw_rast) +
    tidyterra::geom_spatvector(
      data = plot_shp,
      fill = NA,
      size = 2,
      linewidth = .1
    ) +
    labs(
      fill = latex2exp::TeX("Heatwave days in total")
    ) +
    ggplot2::scale_fill_stepsn(
      colors = c("white", "darkslateblue"),
      breaks = seq(0, 20, 2),
      limits = c(0, 20),
      na.value = NA
    ) +
    guides(
      fill = guide_colourbar(
        barwidth = 30,
        barheight = 1.5,
        keywidth = 30
      )
    ) +
    ggspatial::annotation_scale(
      location = "tl", pad_x = ggplot2::unit(1, "cm"),
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
      axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = .5),
      plot.caption = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 20),
      plot.title = ggplot2::element_text(size = 20),
      legend.title = ggplot2::element_text(size = 20, hjust = .5),
      legend.position = "bottom",
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major = ggplot2::element_line(colour = "grey")
    )
  return(p)
}

p_htw_nps_night <- map_heatwave_number(htw_night_nps, input_nps$plot_shp)
p_htw_nps_day <- map_heatwave_number(htw_day_nps, input_nps$plot_shp)
p_htw_nps <- map_heatwave_number(htw_nps, input_nps$plot_shp)
p_htw_len_nps_night <- map_heatwave_len(htw_night_len_nps, input_nps$plot_shp)
p_htw_len_nps_day <- map_heatwave_len(htw_day_len_nps, input_nps$plot_shp)
p_htw_len_nps <- map_heatwave_len(htw_len_nps, input_nps$plot_shp)
p_nps <- ggpubr::ggarrange(
  p_htw_nps_day,
  p_htw_nps_night,
  p_htw_nps,
  ncol = 3,
  common.legend = TRUE,
  legend = "bottom"
)
ggsave(
  plot = p_nps,
  "./graphs/number_heatwaves_nps_q95.pdf",
  width = 19,
  height = 6,
  dpi = 300
)
p_len_nps <- ggpubr::ggarrange(
  p_htw_len_nps_day,
  p_htw_len_nps_night,
  p_htw_len_nps,
  ncol = 3,
  common.legend = TRUE,
  legend = "bottom"
)
ggsave(
  plot = p_len_nps,
  "./graphs/length_heatwaves_nps_q95.pdf",
  width = 19,
  height = 6,
  dpi = 300
)

p_htw_phoe_night <- map_heatwave_number(htw_night_phoe, input_phoe$plot_shp)
p_htw_phoe_day <- map_heatwave_number(htw_day_phoe, input_phoe$plot_shp)
p_htw_phoe <- map_heatwave_number(htw_phoe, input_phoe$plot_shp)
p_htw_len_phoe_night <- map_heatwave_len(
  htw_night_len_phoe,
  input_phoe$plot_shp
)
p_htw_len_phoe_day <- map_heatwave_len(htw_day_len_phoe, input_phoe$plot_shp)
p_htw_len_phoe <- map_heatwave_len(htw_len_phoe, input_phoe$plot_shp)

p_htw_phoe_day
p_htw_phoe_night
p_phoe <- ggpubr::ggarrange(
  p_htw_phoe_day,
  p_htw_phoe_night,
  p_htw_phoe,
  ncol = 3,
  common.legend = TRUE,
  legend = "bottom"
)
p_len_phoe <- ggpubr::ggarrange(
  p_htw_len_phoe_day,
  p_htw_len_phoe_night,
  p_htw_len_phoe,
  ncol = 3,
  common.legend = TRUE,
  legend = "bottom"
)
p_len_phoe
