#' Extracts era5 at spatiotemporal points
#' @description Extracts era5 at spatiotemporal points
#' @param pts a SpatVector with "time" column (spatiotemporal points).
#' Should be a 100km*100km hourly grid with 1km resolution (created using
#' [`create_grids_from_cells()`]).
#' @param era5 a SpatRaster with era5 data. Layers correspond to time (hours).
extract_era5 <- function(pts, era5) {
  # check inputs
  if (!(methods::is(pts, "SpatVector") || methods::is(pts, "sf"))) {
    stop("pts is not a SpatVector or sf.")
  }
  if (!methods::is(era5, "SpatRaster")) {
    stop("era5 is not a SpatRaster.")
  }
  # check that time is included in SpatVector columns
  if (!"time" %in% names(pts)) {
    stop("time is not included in pts columns.")
  }
  # check that time is a POSIXct with formar "%Y-%m-%d %H:%M:%S"
  if (!inherits(pts$time, "POSIXct")) {
    stop("time is not a POSIXct.")
  }
  # extract era5 data
  start_time <- Sys.time()
  n_hours <- 24
  n_loc <- length(terra::unique(pts$geometry))
  batches <- 1:(nrow(pts) / (n_hours * n_loc))
  pts$era5 <- NA
  m <- terra::match(pts$time, terra::time(era5))
  for (i in batches) {
    indexes <- ((i - 1) * (n_hours * n_loc)) + 1:(n_hours * n_loc)
    # era5 t2m is in Kelvin
    pts[indexes, ]$era5 <- terra::extract(era5,
                                          pts[indexes, ],
                                          layer = m[indexes])[, 3] - 273.15
  }
  end_time <- Sys.time()
  end_time - start_time
  # return data
  return(pts)
}

#' Extracts elevation at spatial points
#' @description Extracts elevation at spatial points
#' @param pts a SpatVector or sf (should not be spatiotemporal)
#' [`create_grids_from_cells()`]).
#' @param elev a SpatRaster with elevation data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom exactextractr exact_extract
extract_elevation <- function(pts, elev, buf_radius = 500) {
  # check inputs
  if (!(methods::is(pts, "SpatVector"))) {
    stop("pts is not a SpatVector.")
  }
  if (!methods::is(elev, "SpatRaster")) {
    stop("elev is not a SpatRaster.")
  }
  # check that time is included in SpatVector columns
  if ("time" %in% names(pts)) {
    stop("pts is probably a spatiotemporal sample.")
  }
  start_time <- Sys.time()
  # check crs
  if (!terra::same.crs(pts, elev)) {
    pts <- terra::project(pts, terra::crs(elev))
  }
  # create polygons with radius arouns each pts
  bufs_pol <- terra::buffer(pts, width = buf_radius) |>
    sf::st_as_sf()
  all_cov <- exactextractr::exact_extract(elev,
                                          sf::st_geometry(bufs_pol),
                                          fun = "mean",
                                          progress = FALSE)
  pts$elev <- all_cov[, 1]
  pts$slope <- all_cov[, 2]
  pts$aspect <- all_cov[, 3]
  pts$flowdir <- all_cov[, 4]
  end_time <- Sys.time()
  end_time - start_time
  # return data
  return(pts)
}



#' Extracts imperviousness at spatial points
#' @description Extracts imperviousness at spatial points
#' @param pts a SpatVector or sf (should not be spatiotemporal)
#' [`create_grids_from_cells()`]).
#' @param imp a SpatRaster with imperviousness data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom exactextractr exact_extract
extract_imperviousness <- function(pts, imp, buf_radius = 500) {
  # check inputs
  if (!(methods::is(pts, "SpatVector"))) {
    stop("pts is not a SpatVector.")
  }
  if (!methods::is(imp, "SpatRaster")) {
    stop("imp is not a SpatRaster.")
  }
  # check that time is included in SpatVector columns
  if ("time" %in% names(pts)) {
    stop("pts is probably a spatiotemporal sample.")
  }
  start_time <- Sys.time()
  # check crs
  if (!terra::same.crs(pts, imp)) {
    pts <- terra::project(pts, terra::crs(imp))
  }
  # create polygons with radius arouns each pts
  bufs_pol <- terra::buffer(pts, width = buf_radius) |>
    sf::st_as_sf()
  pts$imp <- exactextractr::exact_extract(imp,
                                          sf::st_geometry(bufs_pol),
                                          fun = "mean",
                                          progress = FALSE)
  end_time <- Sys.time()
  end_time - start_time
  # return data
  return(pts)
}


#' Extracts tree canopy cover at spatial points
#' @description Extracts tree canopy cover at spatial points
#' @param pts a SpatVector or sf (should not be spatiotemporal)
#' [`create_grids_from_cells()`]).
#' @param tcc a SpatRaster with tree canopy cover data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom exactextractr exact_extract
extract_tree_canopy_cover <- function(pts, tcc, buf_radius = 500) {
  # check inputs
  if (!(methods::is(pts, "SpatVector"))) {
    stop("pts is not a SpatVector.")
  }
  if (!methods::is(tcc, "SpatRaster")) {
    stop("tcc is not a SpatRaster.")
  }
  # check that time is included in SpatVector columns
  if ("time" %in% names(pts)) {
    stop("pts is probably a spatiotemporal sample.")
  }
  start_time <- Sys.time()
  # check crs
  if (!terra::same.crs(pts, tcc)) {
    pts <- terra::project(pts, terra::crs(tcc))
  }
  # create polygons with radius arouns each pts
  bufs_pol <- terra::buffer(pts, width = buf_radius) |>
    sf::st_as_sf()
  pts$tcc <- exactextractr::exact_extract(tcc,
                                          sf::st_geometry(bufs_pol),
                                          fun = "mean",
                                          progress = FALSE)
  end_time <- Sys.time()
  end_time - start_time
  # return data
  return(pts)
}

#' Extracts forest canopy height at spatial points
#' @description Extracts forest canopy height at spatial points
#' @param pts a SpatVector or sf (should not be spatiotemporal)
#' [`create_grids_from_cells()`]).
#' @param fch a SpatRaster with forest canopy height data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom exactextractr exact_extract
extract_forest_canopy_height <- function(pts, fch, buf_radius = 500) {
  # check inputs
  if (!(methods::is(pts, "SpatVector"))) {
    stop("pts is not a SpatVector.")
  }
  if (!methods::is(fch, "SpatRaster")) {
    stop("fch is not a SpatRaster.")
  }
  # check that time is included in SpatVector columns
  if ("time" %in% names(pts)) {
    stop("pts is probably a spatiotemporal sample.")
  }
  start_time <- Sys.time()
  # check crs
  if (!terra::same.crs(pts, fch)) {
    pts <- terra::project(pts, terra::crs(fch))
  }
  # create polygons with radius arouns each pts
  bufs_pol <- terra::buffer(pts, width = buf_radius) |>
    sf::st_as_sf()
  pts$fch <- exactextractr::exact_extract(fch,
                                          sf::st_geometry(bufs_pol),
                                          fun = "mean",
                                          progress = FALSE)
  end_time <- Sys.time()
  end_time - start_time
  # return data
  return(pts)
}


#' Extracts building footprint at spatial points
#' @description Extracts building footprint at spatial points
#' @param pts a SpatVector or sf (should not be spatiotemporal)
#' [`create_grids_from_cells()`]).
#' @param bf a SpatRaster with building footprint data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom exactextractr exact_extract
extract_building_footprint <- function(pts, bf, buf_radius = 500) {
  # check inputs
  if (!(methods::is(pts, "SpatVector"))) {
    stop("pts is not a SpatVector.")
  }
  if (!methods::is(bf, "SpatRaster")) {
    stop("bf is not a SpatRaster.")
  }
  # check that time is included in SpatVector columns
  if ("time" %in% names(pts)) {
    stop("pts is probably a spatiotemporal sample.")
  }
  start_time <- Sys.time()
  # check crs
  if (!terra::same.crs(pts, bf)) {
    pts <- terra::project(pts, terra::crs(bf))
  }
  # create polygons with radius arouns each pts
  bufs_pol <- terra::buffer(pts, width = buf_radius) |>
    sf::st_as_sf()
  pts$bf <- exactextractr::exact_extract(bf,
                                         sf::st_geometry(bufs_pol),
                                         fun = "mean",
                                         progress = FALSE)
  end_time <- Sys.time()
  end_time - start_time
  # return data
  return(pts)
}


#' Extracts local climate zone at spatial points
#' @description Extracts local climate zone at spatial points
#' @param pts a SpatVector or sf (should not be spatiotemporal)
#' [`create_grids_from_cells()`]).
#' @param lcz a SpatRaster with local climate zone data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom exactextractr exact_extract
extract_local_climate_zone <- function(pts, lcz, buf_radius = 500) {
  # check inputs
  if (!(methods::is(pts, "SpatVector"))) {
    stop("pts is not a SpatVector.")
  }
  if (!methods::is(lcz, "SpatRaster")) {
    stop("lcz is not a SpatRaster.")
  }
  # check that time is included in SpatVector columns
  if ("time" %in% names(pts)) {
    stop("pts is probably a spatiotemporal sample.")
  }
  if (!is.numeric(buf_radius)) {
    stop("buf_radius is not a numeric.")
  }
  start_time <- Sys.time()
  # check crs
  if (!terra::same.crs(pts, lcz)) {
    pts <- terra::project(pts, terra::crs(lcz))
  }
  # create polygons with radius arouns each pts
  bufs_pol <- terra::buffer(pts, width = buf_radius) |>
    sf::st_as_sf()
  at_bufs <- exactextractr::exact_extract(lcz,
                                          sf::st_geometry(bufs_pol),
                                          fun = "frac",
                                          stack_apply = TRUE,
                                          progress = FALSE)
  # select only the columns of interest
  at_bufs <- at_bufs[names(at_bufs)[grepl("frac_", names(at_bufs))]]
  # change column names
  lcz_classes <- list(
    id = seq(1, 17, 1),
    class = c(seq(1, 10, 1), c("A", "B", "C", "D", "E", "F", "G")),
    desc = c("Compact highrise",
             "Compact midrise",
             "Compact lowrise",
             "Open highrise",
             "Open midrise",
             "Open lowrise",
             "Lightweight low-rise",
             "Large lowrise",
             "Sparsely built",
             "Heavy Industry",
             "Dense trees",
             "Scattered trees",
             "Bush, scrub",
             "Low plants",
             "Bare rock or paved",
             "Bare soil or sand",
             "Water"),
    col = c("#910613",
            "#D9081C",
            "#FF0A22",
            "#C54F1E",
            "#FF6628",
            "#FF985E",
            "#FDED3F",
            "#BBBBBB",
            "#FFCBAB",
            "#565656",
            "#006A18",
            "#00A926",
            "#628432",
            "#B5DA7F",
            "#000000",
            "#FCF7B1",
            "#656BFA")
  ) |>
    as.data.frame()
  lcz_names <- names(at_bufs) |>
    sub(pattern = "frac_", replacement = "") |>
    as.numeric()
  lcz_names <- lcz_classes[lcz_classes$id %in% lcz_names, c("class")]
  new_names <- sapply(
    lcz_names,
    function(x) {
      paste0("frac_", x, "_", buf_radius, "m")
    }
  )
  names(at_bufs) <- new_names
  # merge data_vect with nlcd class fractions (and reproject)
  new_pts <- cbind(pts, at_bufs)
  end_time <- Sys.time()
  end_time - start_time
  # return data
  return(new_pts)
}
