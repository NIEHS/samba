setwd("/ddn/gs1/home/marquesel/samba/case_study_phi_nyc")
devtools::load_all()

# EXTRACT COVARIATES AT CWS LOCATIONS
source("open_phi_nyc_202407.R")
tz <- "America/New_York"
data <- open_phi_nyc(covariates = TRUE)
cat("...Covariates loaded\n")

# path to era5 data is necessary
path_instant <- paste0(
  "../input/era5_us_2024_07/",
  "data_stream-oper_stepType-instant.nc"
)
path_accum <- paste0(
  "../input/era5_us_2024_07/",
  "data_stream-oper_stepType-accum.nc"
)

# Open data cleaned through brassens pipeline
obs <- readRDS(
  paste0(
    "./input/cws/wu_cleaned_and_calibrated_with_mesonet_",
    "2024-07-01_2024-08-01.rds"
  )
)$obs |>
  terra::vect(crs = "epsg:4326")
cat("...CWS loaded\n")


# Select locations
pts <- terra::unique(obs[, c("site_id")])
# select pts in area
area_rect_p <- terra::project(data$area_rect, pts)
us_borders <- terra::vect(
  paste0(
    "../input/",
    "cb_2018_us_nation_5m/cb_2018_us_nation_5m.shp"
  )
) |>
  terra::project(pts)
pts <- terra::intersect(pts, area_rect_p)
pts <- terra::intersect(pts, us_borders)

# Add local hour
obs$local_hour <- lubridate::with_tz(obs$time, tz = tz) |>
  lubridate::hour()

# Extract spatio-tempo covariates at each location for the entire month
pts_covar <- pts |>
  extract_elevation(elev = data$elev) |>
  extract_imperviousness(imp = data$imp) |>
  extract_forest_canopy_height(fch = data$fch) |>
  extract_tree_canopy_cover(tcc = data$tcc) |>
  extract_local_climate_zone(lcz = data$lcz) |>
  extract_building_footprint(bf = data$bf) |>
  terra::project("epsg:4326") |>
  inflate(2024, 7)
pts_era5 <- extract_era5(
    pts_covar,
    data$era5_accum,
    data$era5_instant,
    path_accum,
    path_instant,
    ts = as.POSIXct("2024-07-01 00:00:00", tz = "UTC"),
    te = as.POSIXct("2024-07-31 23:00:00", tz = "UTC")
  )
pts_covar <- merge(
    as.data.frame(pts_covar, geom = "wkt"),
    as.data.frame(pts_era5, geom = "wkt"),
    by = c("time", "geometry")
  )
pts_covar$geometry <- sf::st_as_sfc(pts_covar$geometry)
pts_covar <- sf::st_as_sf(pts_covar)
sf::st_crs(pts_covar) <- 4326
# Merge with available observations
cws_covar <- terra::merge(obs, pts_covar, by = c("site_id", "time"))

# Save final dataset
saveRDS(cws_covar, "./input/cws/cws_covar_2024-07.rds")
write.csv(cws_covar, "./input/cws/cws_covar_2024-07.csv")
cat("...covariates extracted for CWS\n")

# EXTRACT COVARIATES AT PREDICTION GRID LOCATION

# Find 100km*100km cells id of data$area_rect
cells_id <- data$area_rect |>
  find_cells()
cells_id <- cells_id[-which(cells_id == 574)]
cells <- create_empty_grids(cells_id)

# extract covariates for each cell in the area
for (id in cells_id) {
  cat("covariates extraction for grid cell ", id, "...\n")
  pts <- cells[[id]]
  # select pts in area
  area_rect_p <- terra::project(data$area_rect, pts)
  pts <- terra::intersect(pts, area_rect_p)
  pts <- terra::intersect(pts, us_borders)
  # extract covariates
  pts_covar <- pts |>
    extract_elevation(elev = data$elev) |>
    extract_imperviousness(imp = data$imp) |>
    extract_forest_canopy_height(fch = data$fch) |>
    extract_tree_canopy_cover(tcc = data$tcc) |>
    extract_local_climate_zone(lcz = data$lcz) |>
    extract_building_footprint(bf = data$bf) |>
    terra::project("epsg:4326") |>
    inflate(2024, 7)
  pts_era5 <- extract_era5(
    pts_covar,
    data$era5_accum,
    data$era5_instant,
    path_accum,
    path_instant,
    ts = as.POSIXct("2024-07-01 00:00:00", tz = "UTC"),
    te = as.POSIXct("2024-07-31 23:00:00", tz = "UTC")
  )
  pts_covar <- merge(
    as.data.frame(pts_covar, geom = "wkt"),
    as.data.frame(pts_era5, geom = "wkt"),
    by = c("time", "geometry")
  )
  pts_covar$geometry <- sf::st_as_sfc(pts_covar$geometry)
  pts_covar <- sf::st_as_sf(pts_covar)
  pts_covar$lon <- sf::st_coordinates(pts_covar)[, 1]
  pts_covar$lat <- sf::st_coordinates(pts_covar)[, 2]
  sf::st_crs(pts_covar) <- 4326
    pts_covar$local_hour <- lubridate::with_tz(
    pts_covar$time,
    tz = tz) |>
    lubridate::hour()
  saveRDS(pts_covar,
    file = paste0("./input/grid_", id, "_with_covar_202407.rds")
  )
  write.csv(pts_covar,
    file = paste0("./input/grid_", id, "_with_covar_202407.csv")
  )
  cat("...covariates extracted for grid cell ", id, "\n")
}

# merge all prediction grids
cells <- find_cells(data$area_rect)
cells <- cells[-which(cells == 574)]

pred_list <- list()
for (i in seq_along(cells)) {
  pred_list[[i]] <- readRDS(
    paste0("./input/grid_", cells[i], "_with_covar_202407.rds")
  )
}
pred <- do.call("rbind", pred_list) |>
  sf::st_transform(crs = 4326)
pred$lon <- sf::st_coordinates(pred)[, 1]
pred$lat <- sf::st_coordinates(pred)[, 2]
pred <- as.data.frame(pred, xy = TRUE)
saveRDS(pred,
  file = "./input/grid_full_with_covar_202407.rds"
)
write.csv(pred,
  file = "./input/grid_full_with_covar_202407.csv"
 )
