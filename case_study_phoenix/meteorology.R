# Triangle area: geographical context and meteorological conditions

devtools::load_all()

# create storage folder if they do not exist
if (!dir.exists("./graphs")) {
  dir.create("./graphs")
}
if (!dir.exists("./graphs/meteorology")) {
  dir.create("./graphs/meteorology")
}

## Open area shapefiles
source("open_phoenix.R")
data <- open_phoenix(ref_network = TRUE)

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
