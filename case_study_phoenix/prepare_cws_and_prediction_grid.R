setwd("/ddn/gs1/home/marquesel/samba/case_study_phoenix")
devtools::load_all()


# EXTRACT COVARIATES AT CWS LOCATIONS
source("open_phoenix.R")
data <- open_phoenix(covariates = TRUE)
cat("...Covariates loaded\n")

# Open data cleaned through brassens pipeline and keep July 2021 only
path_init <- "./input/cws/wu_phoenix_cleaned_and_calibrated_"
samp <- readRDS(paste0(path_init, "2023-07-01_2023-08-01.rds"))
# transform to terra::vect and keep geometry
obs <- terra::vect(samp$obs)
cat("...CWS loaded\n")

# Select locations
pts <- terra::unique(obs[, c("site_id")])


# Extract spatio-tempo covariates at each location for the entire month
pts_covar <- pts |>
  extract_elevation(elev = data$elev) |>
  extract_imperviousness(imp = data$imp) |>
  extract_forest_canopy_height(fch = data$fch) |>
  extract_tree_canopy_cover(tcc = data$tcc) |>
  extract_local_climate_zone(lcz = data$lcz) |>
  extract_building_footprint(bf = data$bf) |>
  extract_evapotranspiration(et = data$et) |>
  inflate(2023, 7) #|>
  #extract_era5(data$era5)

# Merge with available observations
cws_covar <- terra::merge(obs, pts_covar, by = c("site_id", "time"))

# Save final dataset
saveRDS(cws_covar, "./input/cws/cws_covar_phoenix_2023-07.rds")
write.csv(cws_covar, "./input/cws/cws_covar_phoenix_2023-07.csv")
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
    inflate(2023, 7) #|>
    #extract_era5(data$era5)
  saveRDS(pts_covar,
    file = paste0("./input/grid_", id, "_with_covar_202307.rds")
  )
  write.csv(pts_covar,
    file = paste0("./input/grid_", id, "_with_covar_202307.csv")
  )
  cat("...covariates extracted for grid cell ", id, "\n")
}
