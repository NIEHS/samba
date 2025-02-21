extract_era5 <- function(
  pts_st,
  era5_accum,
  era5_instant,
  path_accum,
  path_instant,
  ts,
  te
  ) {
  varnames_i <- terra::varnames(era5_instant)
  varnames_a <- terra::varnames(era5_accum)
  all_var <- c(
    "u10",
    "v10",
    "d2m",
    "t2m",
    "tcc",
    "tp"
  )
  stopifnot(
    "era5 variables missing" =
      all_var %in% cbind(varnames_i, varnames_a)
  )
  # get ncdf time
  nc <- ncdf4::nc_open(path_accum)
  times_accum <- ncdf4::ncvar_get(nc, "valid_time")
  ncdf4::nc_close(nc)

  nc <- ncdf4::nc_open(path_instant)
  times_instant <- ncdf4::ncvar_get(nc, "valid_time")
  ncdf4::nc_close(nc)
  # select only points
  pts <- terra::geom(pts_st, wkt = TRUE) |>
    unique() |>
    terra::vect(crs = terra::crs(pts_st))
  # Create a 30,000-meter buffer around points
  # and faster incoming computations
  buf_area_rect <- terra::ext(pts) |>
    terra::as.polygons(crs = terra::crs(pts)) |>
    terra::buffer(30000, joinstyle = "mitre")
  buf_area_rect <- terra::project(buf_area_rect, y = era5_instant)
  era5_instant <- terra::crop(era5_instant, buf_area_rect, snap = "out")
  era5_accum <- terra::crop(era5_accum, buf_area_rect, snap = "out")

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
  bufs_pol <- terra::buffer(pts, width = 15000) |>
    sf::st_as_sf()
  era5_var <- c(
    "era5_t2m",
    "era5_d2m",
    "era5_rh",
    "era5_tcc",
    "era5_lai_hv",
    "era5_lai_lv",
    "era5_stl1",
    "era5_u10",
    "era5_v10",
    "era5_ssr",
    "era5_ssrd",
    "era5_tp",
    "era5_slhf",
    "era5_e",
    "era5_sshf"
  )
  era5_pts <- list()
  for (i in seq_along(era5_var)) {
    era5_pts[[i]] <- exactextractr::exact_extract(
      get(era5_var[i]),
      sf::st_geometry(bufs_pol),
      fun = "mean",
      progress = FALSE) |>
      tidyr::pivot_longer(
        cols = starts_with("mean."),
        names_to = "time",
        values_to = era5_var[i]
    ) |>
    dplyr::mutate(
      time = sub("mean.", "", time),
      time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )
    era5_pts[[i]]$geometry <- rep(
      terra::geom(pts, wkt = TRUE),
      each = length(unique(era5_pts[[i]]$time))
    )
  }
  pts_era5 <- Reduce(
    function(x, y) merge(x, y, by = c("time", "geometry")),
    era5_pts
  ) |>
    terra::vect(geom = "geometry", crs = "epsg:4326", keepgeom = TRUE)
  return(pts_era5)
}

#' Extracts elevation at spatial points
#' @description Extracts elevation at spatial points
#' @param pts a SpatVector or sf (should not be spatiotemporal)
#' @param elev a SpatRaster with elevation data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom exactextractr exact_extract
#' @importFrom methods is
#' @importFrom terra same.crs project buffer
#' @importFrom sf st_as_sf
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
#' @param imp a SpatRaster with imperviousness data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom methods is
#' @importFrom terra same.crs project buffer
#' @importFrom sf st_as_sf
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
#' @param tcc a SpatRaster with tree canopy cover data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom methods is
#' @importFrom terra same.crs project buffer
#' @importFrom sf st_as_sf
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
#' @param fch a SpatRaster with forest canopy height data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom exactextractr exact_extract
#' @importFrom methods is
#' @importFrom terra same.crs project buffer
#' @importFrom sf st_as_sf
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
#' @param bf a SpatRaster with building footprint data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom exactextractr exact_extract
#' @importFrom methods is
#' @importFrom terra same.crs project buffer
#' @importFrom sf st_as_sf
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


#' Extracts evapotranspiration at spatial points
#' @description Extracts evapotranspiration at spatial points
#' @param pts a SpatVector or sf (should not be spatiotemporal)
#' @param et a SpatRaster with building footprint data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom exactextractr exact_extract
#' @importFrom methods is
#' @importFrom terra same.crs project buffer
#' @importFrom sf st_as_sf
extract_evapotranspiration <- function(pts, et, buf_radius = 500) {
  # check inputs
  if (!(methods::is(pts, "SpatVector"))) {
    stop("pts is not a SpatVector.")
  }
  if (!methods::is(et, "SpatRaster")) {
    stop("et is not a SpatRaster.")
  }
  # check that time is included in SpatVector columns
  if ("time" %in% names(pts)) {
    stop("pts is probably a spatiotemporal sample.")
  }
  start_time <- Sys.time()
  # check crs
  if (!terra::same.crs(pts, et)) {
    pts <- terra::project(pts, terra::crs(et))
  }
  # create polygons with radius arouns each pts
  bufs_pol <- terra::buffer(pts, width = buf_radius) |>
    sf::st_as_sf()
  pts$et <- exactextractr::exact_extract(et,
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
#' @param lcz a SpatRaster with local climate zone data.
#' @param buf_radius a numeric with the radius of the buffer around each point.
#' @importFrom exactextractr exact_extract
#' @importFrom methods is
#' @importFrom terra same.crs project buffer
#' @importFrom sf st_as_sf
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
  # add classes that are not present with 0 ratio
  expected_cols <- sapply(
    lcz_classes$class,
    function(x) {
      paste0("frac_", x, "_", buf_radius, "m")
    }
  )
  missing_cols <- expected_cols[which(!(expected_cols %in% colnames(at_bufs)))]
  at_bufs[, missing_cols] <- 0
  # merge data_vect with nlcd class fractions (and reproject)
  new_pts <- cbind(pts, at_bufs)
  end_time <- Sys.time()
  end_time - start_time
  # return data
  return(new_pts)
}
