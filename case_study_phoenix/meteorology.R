# Triangle area: geographical context and meteorological conditions

devtools::load_all()
setwd("/ddn/gs1/home/marquesel/samba/case_study_phoenix")

# create storage folder if they do not exist
if (!dir.exists("./graphs")) {
  dir.create("./graphs")
}
if (!dir.exists("./graphs/meteorology")) {
  dir.create("./graphs/meteorology")
}

## Open area shapefiles
source("open_phoenix.R")
data <- open_phoenix(ref_network = TRUE, covariates = TRUE)

# METEOROLOGICAL CONDITIONS 

ggplot(data$ref) +
  geom_sf(data = data$area_rect, fill = NA, color = "black") +
  geom_sf(data = data$plot_shp, fill = NA, color = "red") +
  geom_text(aes(x = lon, y = lat, label = site_id), size = 3) +
  scale_color_gradientn(
    colours = load_palette("temp_ipcc"),
    na.value = "grey"
  ) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )

ref_agg <- data$ref |>
  as.data.table()
ref_agg <- ref_agg[,
  .(
    temp = median(temp, na.rm = TRUE),
    hour = lubridate::hour(time),
    sw = median(sw, na.rm = TRUE)
  ),
  by = .(site_id, lubridate::hour(time))
]
# plot temperature per hour
p_temp <- ggplot(ref_agg) +
  geom_line(aes(x = hour, y = temp, color = site_id)) +
  labs(x = "Hour (UTC)", y = "Temperature (°C)") +
  scale_y_continuous(
    breaks = seq(floor(min(ref_agg$temp)), ceiling(max(ref_agg$temp)), 1)
  ) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_temp
ggsave(
  "./graphs/meteorology/meteo_temp_hour.png",
  plot = p_temp,
  width = 10, height = 5, bg = "white", dpi = 300
)

p_sw <- ggplot(ref_agg) +
  geom_line(aes(x = hour, y = sw, color = site_id)) +
  labs(x = "Hour (UTC)", y = "Shortwave (W.m-2)") +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_sw
ggsave("./graphs/meteorology/meteo_sw_hour.png",
  plot = p_sw,
  width = 10, height = 5, bg = "white", dpi = 300
)
## Meteorological conditions

### T2m
site_id <- unique(data$ref$site_id)
temp <- list()
for (i in seq_along(site_id)) {
  temp[[i]] <- mytile(data$ref[which(data$ref$site_id == site_id[i]), ], "temp") +
    scale_fill_stepsn(
      colours = load_palette("temp_ipcc"),
      na.value = "grey",
      breaks = seq(
        floor(min(data$ref$temp, na.rm = TRUE)),
        ceiling(max(data$ref$temp, na.rm = TRUE)),
        1
      ),
      limits = c(
        floor(min(data$ref$temp, na.rm = TRUE)),
        ceiling(max(data$ref$temp, na.rm = TRUE))
      )
    ) +
    labs(
      fill = "T2m\n(°C)",
      title = site_id[i]
    )
}
temp

sw <- list()
for (i in seq_along(site_id)) {
  sw[[i]] <- mytile(data$ref[which(data$ref$site_id == site_id[i]), ], "sw") +
    scale_fill_gradientn(
      colours = c("navyblue", "orange", "yellow"),
      breaks = seq(0, 1100, 100),
      na.value = "grey",
      limits = range(data$ref$sw)
    ) +
    labs(
      fill = "SW\n(W.m-2)",
      title = site_id[i]
    )
}
sw

### Rain

hist(log(data$ref$rain))
rain <- list()
for (i in seq_along(site_id)) {
  rain[[i]] <- mytile(data$ref[which(data$ref$site_id == site_id[i]), ], "rain") +
    scale_fill_gradientn(
      colours = c("white", "navyblue"),
      na.value = "grey",
      breaks = seq(0, 80, 10),
      limits = c(0, 30)
    ) +
    labs(
      fill = "Rainfall\n(mm)",
      title = site_id[i]
    )
}
rain

rain_bol <- list()
for (i in seq_along(site_id)) {
  rain_bol[[i]] <- mytile_rain(data$ref[which(data$ref$site_id == site_id[i]), ]) +
    labs(
      title = site_id[i],
      fill = "Presence of rain"
    )
}
rain_bol

## Relative humidity
hist(data$ref$rh)
rh <- list()
for (i in seq_along(site_id)) {
  rh[[i]] <- mytile(data$ref[which(data$ref$site_id == site_id[i]), ], "rh") +
    scale_fill_gradientn(
      colours = c("white", "navyblue"),
      na.value = "grey",
      breaks = seq(0, 100, 10),
      limits = c(0, 100)
    ) +
    labs(
      fill = "RH\n(%)",
      title = site_id[i]
    )
}
rh

## Wind rose
ref_wind <- data$ref[which(!is.na(data$ref$wind_d)), ]
site_id <- unique(ref_wind$site_id)
wind <- list()
for (i in seq_along(site_id)) {
  wind[[i]] <- plot.windrose(
    data = data$ref[which(!is.na(data$ref$wind_s)), ][which(data$ref[which(!is.na(data$ref$wind_s)), ]$site_id == site_id[i]), ],
    spd = ref_wind[which(ref_wind$site_id == site_id[i]), ]$wind_s,
    dir = ref_wind[which(ref_wind$site_id == site_id[i]), ]$wind_d
  ) +
    labs(x = site_id[i], y = "") +
    ggtitle(site_id[i])
  # save
  ggsave(wind[[i]],
    filename = paste0(
      "./graphs/meteorology/meteo_wind_rose_",
      site_id[i],
      ".png"
    ),
    width = 6,
    height = 4,
    dpi = 300
  )
}

# merge temp, rain, rh, sw, soilm plot for each site_id and save
plots <- list()
for (i in seq_along(site_id)) {
  plots[[i]] <- ggpubr::ggarrange(temp[[i]],
    rain[[i]],
    rh[[i]],
    sw[[i]],
    ncol = 5,
    nrow = 1
  )
  ggsave(plots[[i]],
    filename = paste0(
      "./graphs/meteorology/meteo_",
      site_id[i],
      ".png"
    ),
    width = 25,
    height = 7,
    dpi = 300
  )
}



# era5
path_instant <- paste0(
  "../input/era5_us_2023_07/",
   "data_stream-oper_stepType-instant.nc"
)
path_accum <- paste0(
  "../input/era5_us_2023_07/",
  "data_stream-oper_stepType-accum.nc"
)

  # get ncdf time
  nc <- ncdf4::nc_open(path_accum)
  times_accum <- ncdf4::ncvar_get(nc, "valid_time")
  ncdf4::nc_close(nc)

  nc <- ncdf4::nc_open(path_instant)
  times_instant <- ncdf4::ncvar_get(nc, "valid_time")
  ncdf4::nc_close(nc)
 
  # Create a 30,000-meter buffer around points
  # and faster incoming computations
  buf_area_rect <- data$area_rect
  buf_area_rect <- terra::project(buf_area_rect, y = data$era5_instant)
  era5_instant <- terra::crop(data$era5_instant, buf_area_rect, snap = "out")
  era5_accum <- terra::crop(data$era5_accum, buf_area_rect, snap = "out")
  varnames_i <- terra::varnames(data$era5_instant)
  varnames_a <- terra::varnames(data$era5_accum)
  ts <- as.POSIXct("2023-07-01 00:00:00", tz = "UTC")
  te <- as.POSIXct("2023-07-31 23:59:59", tz = "UTC")

  for (i in varnames_i) {
    r <- era5_instant[i]
    terra::time(r) <- as.POSIXct(
      times_instant,
      origin = "1970-01-01 00:00:00",
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
    # Find the indices of the layers within the specified time range
    time_idx <- which(terra::time(r) >= ts & terra::time(r) <= te)
    r <- r[[time_idx]]
    names(r) <- format(terra::time(r), "%Y-%m-%d %H:%M:%S")
    assign(paste0("era5_", i), r)
  }
  era5_t2m <- era5_t2m - 273.15
  era5_d2m <- era5_d2m - 273.15
  # calculation of rh from t2m and d2m
  e_t2m <- 6.1078 * exp((17.1 * era5_t2m) / (235 + era5_t2m))
  e_d2m <- 6.1078 * exp((17.1 * era5_d2m) / (235 + era5_d2m))
  era5_rh <- e_d2m / e_t2m
  for (i in varnames_a) {
    r <- era5_accum[[grep(
      paste0("^", i, "_"),
      terra::names(era5_accum)
    )]]
    terra::time(r) <- as.POSIXct(
      times_accum,
      origin = "1970-01-01 00:00:00",
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
    # Find the indices of the layers within the specified time range
    time_idx <- which(terra::time(r) >= ts & terra::time(r) <= te)
    r <- r[[time_idx]]
    names(r) <- format(terra::time(r), "%Y-%m-%d %H:%M:%S")
    assign(paste0("era5_", i), r)
  }

terra::plot(era5_t2m[[1]])
df <- as.data.frame(era5_t2m, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_t2m <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("2 metre temperature (C)")

terra::plot(era5_rh[[1]])
df <- as.data.frame(era5_rh, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_rh <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("Relative Humidity")


df <- as.data.frame(era5_u10, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_u10 <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("10 metre U wind component")

df <- as.data.frame(era5_v10, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_v10 <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("10 metre V wind component")

df <- as.data.frame(era5_stl1, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_stl1 <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("Soil temperature level 1")

df <- as.data.frame(era5_lai_lv, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_lai_lv <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("Leaf area index, low vegetation")

df <- as.data.frame(era5_lai_hv, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_lai_hv <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("Leaf area index, high vegetation")
terra::plot(data$era5_instant["lai_hv"][[150]])

df <- as.data.frame(era5_tcc, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_tcc <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("Total cloud cover")
terra::plot(data$era5_instant["tcc"][[150]])

df <- as.data.frame(era5_tp, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_tp <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("Total precipitation (m)")

df <- as.data.frame(era5_slhf, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_slhf <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("Surface latent heat flux (J m**-2)")
terra::plot(data$era5_accum["slhf"][[15]])


df <- as.data.frame(era5_ssr, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_ssr <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("Surface net short-wave (solar) radiation (J m**-2)")
terra::plot(data$era5_accum["ssr"][[15]])

df <- as.data.frame(era5_sshf, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_sshf <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("Surface sensible heat flux (J m**-2)")
terra::plot(data$era5_accum["sshf"][[15]])

df <- as.data.frame(era5_ssrd, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_ssrd <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("Surface short-wave (solar) radiation downwards (J m**-2)")
terra::plot(data$era5_accum["ssrd"][[15]])

df <- as.data.frame(era5_e, xy = TRUE) |>
  tidyr::pivot_longer(-(x:y), names_to = "time", values_to = "var")
df$time <- as.POSIXct(df$time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
df$id <- paste0(df$x, "_", df$y)
p_e <- ggplot(df) +
  geom_line(aes(x = time, y = var, group = id, color = id)) +
  ylab("Evaporation (m of water equivalent)")
terra::plot(data$era5_accum["e"][[15]])

varnames <- c(
  "t2m",
  "rh",
  "u10",
  "v10",
  "stl1",
  "lai_hv",
  "lai_lv",
  "tcc",
  "tp",
  "slhf",
  "ssr",
  "sshf",
  "ssrd",
  "e"
)

for (i in varnames) {
  plot <- get(paste0("p_", i))
  ggsave(plot,
    filename = paste0(
      "./graphs/meteorology/meteo_timeserie_era5_",
      i,
      ".png"
    ),
    width = 25,
    height = 7,
    dpi = 300
  )
}
