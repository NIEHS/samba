setwd("/ddn/gs1/home/marquesel/samba/case_study_triangle")
devtools::load_all()

# EXTRACT COVARIATES AT CWS LOCATIONS
source("open_triangle.R")
data <- open_triangle(covariates = TRUE)
cat("...Covariates loaded\n")

# path to era5 data is necessary
path_instant <- paste0(
  "../input/era5_us_2021_05to09/",
   "data_stream-oper_stepType-instant.nc"
)
path_accum <- paste0(
  "../input/era5_us_2021_05to09/",
  "data_stream-oper_stepType-accum.nc"
)

# Open data cleaned through brassens pipeline and keep July 2021 only
path_init <- "./input/cws/wu_nc_cleaned_and_calibrated_"
samp1 <- readRDS(paste0(path_init, "2021-06-29_2021-07-13.rds"))
samp2 <- readRDS(paste0(path_init, "2021-07-13_2021-07-27.rds"))
samp3 <- readRDS(paste0(path_init, "2021-07-27_2021-08-10.rds"))
# transform to terra::vect and keep geometry
obs1 <- terra::vect(samp1$obs)
obs2 <- terra::vect(samp2$obs)
obs3 <- terra::vect(samp3$obs)
obs1 <- obs1[which(lubridate::month(obs1$time) == 7), ]
obs2 <- obs2[which(lubridate::month(obs2$time) == 7), ]
obs3 <- obs3[which(lubridate::month(obs3$time) == 7), ]
obs <- rbind(obs1, obs2, obs3)
cat("...CWS loaded\n")

# Select locations
pts <- terra::unique(obs[, c("site_id")])
# select pts in area
area_rect_p <- terra::project(data$area_rect, pts)
pts <- terra::intersect(pts, area_rect_p)

# Extract spatio-tempo covariates at each location for the entire month
pts_covar <- pts |>
  extract_elevation(elev = data$elev) |>
  extract_imperviousness(imp = data$imp) |>
  extract_forest_canopy_height(fch = data$fch) |>
  extract_tree_canopy_cover(tcc = data$tcc) |>
  extract_local_climate_zone(lcz = data$lcz) |>
  extract_building_footprint(bf = data$bf) |>
  extract_evapotranspiration(et = data$et) |>
  terra::project("epsg:4326") |>
  inflate(2021, 7)
pts_era5 <- extract_era5(
    pts_covar,
    data$era5_accum,
    data$era5_instant,
    path_accum,
    path_instant,
    ts = as.POSIXct("2021-07-01 00:00:00", tz = "UTC"),
    te = as.POSIXct("2021-07-31 23:00:00", tz = "UTC")
  ) |>
  sf::st_transform(crs = 4326)
pts_covar <- merge(
  as.data.frame(pts_covar),
  as.data.frame(pts_era5),
  by = c("time", "geometry")
)

# Merge with available observations
cws_covar <- terra::merge(obs, pts_covar, by = c("site_id", "time"))

# Save final dataset
saveRDS(cws_covar, "./input/cws/cws_covar_nc_2021-07.rds")
write.csv(cws_covar, "./input/cws/cws_covar_nc_2021-07.csv")
cat("...covariates extracted for CWS\n")

# EXTRACT COVARIATES AT PREDICTION GRID LOCATION

# Find 100km*100km cells id of data$area_rect
cells_id <- data$area_rect |>
  find_cells()
cells <- create_empty_grids(cells_id)

# extract covariates for each cell in the area
for (id in cells_id) {
  cat("covariates extraction for grid cell ", id, "...\n")
  pts <- cells[[id]]
  # select pts in area
  area_rect_p <- terra::project(data$area_rect, pts)
  pts <- terra::intersect(pts, area_rect_p)
  # extract covariates
  pts_covar <- pts |>
    extract_elevation(elev = data$elev) |>
    extract_imperviousness(imp = data$imp) |>
    extract_forest_canopy_height(fch = data$fch) |>
    extract_tree_canopy_cover(tcc = data$tcc) |>
    extract_local_climate_zone(lcz = data$lcz) |>
    extract_building_footprint(bf = data$bf) |>
    extract_evapotranspiration(et = data$et) |>
    terra::project("epsg:4326") |>
    inflate(2021, 7)
  pts_era5 <- extract_era5(
    pts_covar,
    data$era5_accum,
    data$era5_instant,
    path_accum,
    path_instant,
    ts = as.POSIXct("2021-07-01 00:00:00", tz = "UTC"),
    te = as.POSIXct("2021-07-31 23:00:00", tz = "UTC")
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
  saveRDS(pts_covar,
    file = paste0("./input/grid_", id, "_with_covar_202107.rds")
  )
  write.csv(pts_covar,
    file = paste0("./input/grid_", id, "_with_covar_202107.csv")
  )
  cat("...covariates extracted for grid cell ", id, "\n")
}