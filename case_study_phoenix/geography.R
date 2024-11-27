# Triangle area: geographical context and meteorological conditions

devtools::load_all()

# create storage folder if they do not exist
if (!dir.exists("./graphs")) {
  dir.create("./graphs")
}
if (!dir.exists("./graphs/geography")) {
  dir.create("./graphs/geography")
}
## Open area shapefiles
source("open_triangle.R")
data <- open_triangle(cws = TRUE, covariates = TRUE, ref_network = TRUE)

# merge ref and cws network for plotting
cws_locs <- data$cws[, c("site_id", "lat", "lon")] |>
  unique() |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
cws_locs$net <- "WU"
ref_locs <- data$ref[, c("site_id", "lat", "lon")] |>
  as.data.frame() |>
  unique() |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
ref_locs$net <- "ECONET"
all_locs <- rbind(cws_locs, ref_locs)

# store plot legend parameters for each network
net_pal <- list(
  col = c("red", "cyan"),
  shape = c(16, 18),
  size = c(3, 2),
  meaning = c("ECONET", "WU")
)

# plot and save every covariate
shp <- terra::project(data$plot_shp, data$lcz)
terra::values(data$lcz) <- as.factor(terra::values(data$lcz))
p_lcz <- ggplot() +
  tidyterra::geom_spatraster(data = data$lcz) +
  tidyterra::geom_spatvector(
    data = shp,
    fill = NA,
    size = 2,
    linewidth = .3,
    color = "white"
  ) +
  geom_sf(
    data = all_locs,
    aes(
      geometry = geometry,
      color = net,
      size = net,
      shape = net
    )
  ) +
  ggplot2::scale_color_manual(
    name = "Network",
    values = net_pal$col,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_shape_manual(
    name = "Network",
    values = net_pal$shape,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_size_manual(
    name = "Network",
    values = net_pal$size,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_fill_manual(
    values = load_palette("lcz")$col,
    breaks = load_palette("lcz")$num,
    labels = load_palette("lcz")$meaning,
    na.value = NA
  ) +
  labs(fill = "Local Climate Zones") +
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
    axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_lcz
# Save the plot
ggsave(
  plot = p_lcz,
  "./graphs/geography/lcz.png",
  width = 10,
  height = 7,
  dpi = 300
)

shp <- terra::project(data$plot_shp, data$tcc)
p_tcc <- ggplot() +
  tidyterra::geom_spatraster(data = as.numeric(data$tcc)) +
  tidyterra::geom_spatvector(
    data = shp,
    fill = NA,
    size = 2,
    linewidth = .5,
    color = "yellow"
  ) +
  geom_sf(
    data = all_locs,
    aes(
      geometry = geometry,
      color = net,
      size = net,
      shape = net
    )
  ) +
  ggplot2::scale_color_manual(
    name = "Network",
    values = net_pal$col,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_shape_manual(
    name = "Network",
    values = net_pal$shape,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_size_manual(
    name = "Network",
    values = net_pal$size,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_fill_stepsn(
    colours = c("black", "lightgrey", "darkgreen"),
    na.value = NA,
    values = c(0, 0.01, 1.1),
    breaks = seq(0, 100, 10)
  ) +
  labs(fill = "Tree Canopy Cover (%)") +
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
    axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_tcc

# Save the plot
ggsave(
  plot = p_tcc,
  "./graphs/geography/tcc.png",
  width = 10,
  height = 7,
  dpi = 300
)

shp <- terra::project(data$plot_shp, data$fch)
p_fch <- ggplot() +
  tidyterra::geom_spatraster(data = as.numeric(data$fch)) +
  tidyterra::geom_spatvector(
    data = shp,
    fill = NA,
    size = 2,
    linewidth = .5,
    color = "yellow"
  ) +
  geom_sf(
    data = all_locs,
    aes(
      geometry = geometry,
      color = net,
      size = net,
      shape = net
    )
  ) +
  ggplot2::scale_color_manual(
    name = "Network",
    values = net_pal$col,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_shape_manual(
    name = "Network",
    values = net_pal$shape,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_size_manual(
    name = "Network",
    values = net_pal$size,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_fill_stepsn(
    colours = c("black", "lightgrey", "darkgreen"),
    na.value = NA,
    values = c(0, 0.01, 1.1),
    breaks = seq(0, max(values(data$fch, na.rm = TRUE)), 5)
  ) +
  labs(fill = "Forest canopy height (m)") +
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
    axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_fch

# Save the plot
ggsave(
  plot = p_fch,
  "./graphs/geography/fch.png",
  width = 10,
  height = 7,
  dpi = 300
)

shp <- terra::project(data$plot_shp, data$et)
et_proj <- terra::project(data$et, "epsg:4326")
p_et <- ggplot() +
  tidyterra::geom_spatraster(data = as.numeric(et_proj)) +
  tidyterra::geom_spatvector(
    data = data$plot_shp,
    fill = NA,
    size = 2,
    linewidth = .5,
    color = "yellow"
  ) +
  geom_sf(
    data = all_locs,
    aes(
      geometry = geometry,
      color = net,
      size = net,
      shape = net
    )
  ) +
  ggplot2::scale_color_manual(
    name = "Network",
    values = net_pal$col,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_shape_manual(
    name = "Network",
    values = net_pal$shape,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_size_manual(
    name = "Network",
    values = net_pal$size,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_fill_stepsn(
    colours = c("white", "blue"),
    na.value = NA
  ) +
  labs(fill = "Evapotranspiration\n(kg/m²/year)") +
  ggspatial::annotation_scale(
    location = "bl", pad_x = ggplot2::unit(1, "cm"),
    pad_y = ggplot2::unit(1, "cm"),
    height = ggplot2::unit(0.30, "cm"),
    text_cex = 1
  ) +
  xlim(c(-79.21, -78.3)) +
  ggspatial::annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = ggplot2::unit(0.2, "cm"),
    pad_y = ggplot2::unit(0.2, "cm")
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
p_et

# Save the plot
ggsave(
  plot = p_et,
  "./graphs/geography/et.png",
  width = 10,
  height = 7,
  dpi = 300
)

shp <- terra::project(data$plot_shp, data$bf)
p_bf <- ggplot() +
  tidyterra::geom_spatraster(data = as.numeric(data$bf / 900)) +
  tidyterra::geom_spatvector(
    data = shp,
    fill = NA,
    size = 2,
    linewidth = .5,
    color = "black"
  ) +
  geom_sf(
    data = all_locs,
    aes(
      geometry = geometry,
      color = net,
      size = net,
      shape = net
    )
  ) +
  ggplot2::scale_color_manual(
    name = "Network",
    values = net_pal$col,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_shape_manual(
    name = "Network",
    values = net_pal$shape,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_size_manual(
    name = "Network",
    values = net_pal$size,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_fill_stepsn(
    colours = c("white", "red"),
    breaks = seq(0, 1, .1),
    na.value = NA
  ) +
  labs(fill = "Building footprint\n(ratio per 30m²)") +
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
    axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_bf
# Save the plot
ggsave(
  plot = p_bf,
  "./graphs/geography/bf.png",
  width = 10,
  height = 7,
  dpi = 300
)

shp <- terra::project(data$plot_shp, data$elev)
library("RColorBrewer")
p_elev <- ggplot() +
  tidyterra::geom_spatraster(data = data$elev[[1]]) +
  tidyterra::geom_spatvector(
    data = shp,
    fill = NA,
    size = 2,
    linewidth = .5,
    color = "black"
  ) +
  geom_sf(
    data = all_locs,
    aes(
      geometry = geometry,
      color = net,
      size = net,
      shape = net
    )
  ) +
  ggplot2::scale_color_manual(
    name = "Network",
    values = net_pal$col,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_shape_manual(
    name = "Network",
    values = net_pal$shape,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_size_manual(
    name = "Network",
    values = net_pal$size,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_fill_gradientn(
    colours = brewer.pal(n = 11, name = "BrBG"),
    na.value = NA
  ) +
  labs(fill = "Elevation (m)") +
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
    axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_elev
# Save the plot
ggsave(
  plot = p_elev,
  "./graphs/geography/elev.png",
  width = 10,
  height = 7,
  dpi = 300
)

shp <- terra::project(data$plot_shp, data$imp)
p_imp <- ggplot() +
  tidyterra::geom_spatraster(data = as.numeric(data$imp)) +
  tidyterra::geom_spatvector(
    data = shp,
    fill = NA,
    size = 2,
    linewidth = .3,
    color = "white"
  ) +
  geom_sf(
    data = all_locs,
    aes(
      geometry = geometry,
      color = net,
      size = net,
      shape = net
    )
  ) +
  ggplot2::scale_color_manual(
    name = "Network",
    values = net_pal$col,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_shape_manual(
    name = "Network",
    values = net_pal$shape,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_size_manual(
    name = "Network",
    values = net_pal$size,
    breaks = net_pal$meaning,
    labels = net_pal$meaning
  ) +
  ggplot2::scale_fill_gradientn(
    colours = c("black", "lightgrey", "rosybrown1", "indianred2", "violet", "purple"),
    values = c(0, seq(0.01, 1.1, 0.25)),
    na.value = NA
  ) +
  labs(fill = "Imperviousness (%)") +
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
    axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 10),
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "grey")
  )
p_imp

# Save the plot
ggsave(
  plot = p_imp,
  "./graphs/geography/imp.png",
  width = 10,
  height = 7,
  dpi = 300
)

p_all <- ggpubr::ggarrange(p_bf, p_imp, p_tcc, p_fch, p_et, p_elev, p_lcz,
  nrow = 2, ncol = 4)
ggsave(
  plot = p_all,
  "./graphs/geography/all.png",
  width = 30,
  height = 15,
  dpi = 300,
  bg = "white"
)
