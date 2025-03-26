#' @title Prepare samba Bayesian Hierarchical model materials.
#' @description Prepare samba Bayesian Hierarchical model materials from
#' input list containing necessary raw data.
#' @param input list. Contains raw data: `cws_raw`, `elev`, `fch`,
#' `imp`, `era5_instant`, `era5_accum`, `area_shp`
#' @param era5_accum_path character. Path to era5 accumulated netcdf to extract
#' accurate time information.
#' @param era5_instant_path character. Path to era5 accumulated netcdf to
#' extract accurate time information.
#' @importFrom rnaturalearth ne_countries
#' @importFrom terra as.polygons buffer ext crs unique project
#' intersect crop merge
#' @importFrom lutz tz_lookup_coords
#' @importFrom lubridate with_tz hour
#' @importFrom sf st_as_sf st_as_sfc st_crs st_transform st_coordinates
#' @return a list with samba function materials.
#' @author Eva Marques
#' @export
prepare_bhm_materials <- function(
  input,
  era5_accum_path,
  era5_instant_path
) {
  bhm_materials <- list()
  bhm_materials$area_rect <- terra::ext(input$area_shp) |>
    terra::as.polygons(crs = terra::crs(input$area_shp)) |>
    terra::buffer(10000, joinstyle = "mitre")
  message("  area rectangle done!")
  elev <- process_elev(elev = input$elev, polygon = bhm_materials$area_rect)
  imp <- process_imp(imp = input$imp, polygon = bhm_materials$area_rect)
  fch <- process_fch(fch = input$fch, polygon = bhm_materials$area_rect)
  message("  spatial covariates processed!")
  locs <- terra::unique(input$cws_raw[, c("site_id")])
  area_rect_p <- terra::project(bhm_materials$area_rect, locs)
  us_borders <- rnaturalearth::ne_countries(
    type = "countries",
    country = "United States of America",
    scale = "small",
    returnclass = "sv"
  )
  locs <- terra::intersect(locs, area_rect_p)
  locs <- terra::crop(locs, us_borders)
  locs_spatial <- locs |>
    extract_elevation(elev = elev) |>
    extract_imperviousness(imp = imp) |>
    extract_forest_canopy_height(fch = fch) |>
    terra::project("epsg:4326") |>
    inflate(input$ts, input$te)
  message("  spatial covariates extracted at PWS locations!")
  locs_era5 <- extract_era5(
    locs_spatial,
    input$era5_accum,
    input$era5_instant,
    era5_accum_path,
    era5_instant_path,
    input$ts,
    input$te
  )
  message("  era5 covariates extracted at PWS locations!")
  locs_covar <- merge(
    as.data.frame(locs_spatial, geom = "wkt"),
    as.data.frame(locs_era5, geom = "wkt"),
    by = c("time", "geometry")
  )
  locs_covar$geometry <- sf::st_as_sfc(locs_covar$geometry)
  locs_covar <- sf::st_as_sf(locs_covar)
  sf::st_crs(locs_covar) <- 4326
  bhm_materials$cws <- terra::merge(
    input$cws_raw,
    locs_covar,
    by = c("site_id", "time")
  )
  # add local hour
  local_tz <- lutz::tz_lookup_coords(
    bhm_materials$cws$lat,
    bhm_materials$cws$lon
  )
  bhm_materials$cws$local_hour <- lubridate::with_tz(
    bhm_materials$cws$time,
    tz = local_tz
  ) |>
    lubridate::hour()
  # EXTRACT COVARIATES AT PREDICTION GRID LOCATION
  # Find 100km*100km cells id of bhm_materials$area_rect
  cells_id <- bhm_materials$area_rect |>
    find_cells()
  cells <- create_empty_grids(cells_id)
  pred_list <- list()
  message("  100km*100km cells in area found")
  # extract covariates for each cell in the area
  for (i in seq_along(cells_id)) {
    id <- cells_id[[i]]
    message("   covariates extraction for grid cell ", id, "...")
    locs <- cells[[id]]
    # select locs in area
    area_rect_p <- terra::project(bhm_materials$area_rect, locs)
    locs <- terra::intersect(locs, area_rect_p)
    locs <- terra::crop(locs, us_borders)
    # extract covariates
    locs_spatial <- locs |>
      extract_elevation(elev = elev) |>
      extract_imperviousness(imp = imp) |>
      extract_forest_canopy_height(fch = fch) |>
      terra::project("epsg:4326") |>
      inflate(input$ts, input$te)
    locs_era5 <- extract_era5(
      locs_spatial,
      input$era5_accum,
      input$era5_instant,
      era5_accum_path,
      era5_instant_path,
      ts = input$ts,
      te = input$te
    )
    locs_covar <- merge(
      as.data.frame(locs_spatial, geom = "wkt"),
      as.data.frame(locs_era5, geom = "wkt"),
      by = c("time", "geometry")
    )
    locs_covar$geometry <- sf::st_as_sfc(locs_covar$geometry)
    locs_covar <- sf::st_as_sf(locs_covar)
    locs_covar$lon <- sf::st_coordinates(locs_covar)[, 1]
    locs_covar$lat <- sf::st_coordinates(locs_covar)[, 2]
    sf::st_crs(locs_covar) <- 4326
    local_tz <- lutz::tz_lookup_coords(
      locs_covar$lat,
      locs_covar$lon
    )
    locs_covar$local_hour <- lubridate::with_tz(
      locs_covar$time,
      tz = local_tz
    ) |>
      lubridate::hour()
    pred_list[[i]] <- locs_covar
    message("   covariates extracted for grid cell ", id, "!")
  }
  # merge all prediction grids
  bhm_materials$pred <- do.call("rbind", pred_list) |>
    sf::st_transform(crs = 4326)
  bhm_materials$pred$lon <- sf::st_coordinates(bhm_materials$pred)[, 1]
  bhm_materials$pred$lat <- sf::st_coordinates(bhm_materials$pred)[, 2]
  bhm_materials$pred <- as.data.frame(bhm_materials$pred, xy = TRUE)
  message("  Prediction grid ready!")
  bhm_materials
}
