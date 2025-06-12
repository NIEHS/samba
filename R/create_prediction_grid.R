#' Find cells id in a polygon
#' @description Find cells id in a polygon
#' @param polygon a terra::SpatVector polygon
#' @return a vector of cells id
#' @importFrom terra ext vect rast values ncell crop project
#' @importFrom tigris states
#' @export
find_cells <- function(polygon) {
  # US 100km * 100km grid
  contig_us_grid <- terra::ext(
    -124.736342,
    -66.945392,
    24.521208,
    49.382808
  ) |>
    terra::vect(crs = "epsg:4326") |>
    terra::rast(res = 1)
  terra::values(contig_us_grid) <- 1:terra::ncell(contig_us_grid)
  names(contig_us_grid) <- "id"
  us_states <- terra::vect(
    "input/cb_2018_us_state_5m/cb_2018_us_state_5m.shp"
  )
  no_contig <- c("VI", "MP", "AK", "PR", "AS", "GU", "HI")
  us_contig <- us_states[which(!(us_states$STUSPS %in% no_contig)), "NAME"] |>
    terra::vect() |>
    terra::project("epsg:4326")
  contig_us_grid <- terra::crop(
    contig_us_grid,
    us_contig,
    snap = "in",
    mask = TRUE
  )
  # polygon projection
  poly <- terra::project(polygon, "epsg:4326")
  # select cells that intersect with the polygon
  cropped <- terra::crop(contig_us_grid, poly, mask = TRUE, snap = "out")
  cell_ids <- terra::values(cropped, na.rm = TRUE)
  as.vector(cell_ids)
}

#' Create 100km*100km grids inside US cells provided by their id
#' @description Create 100km*100km grids within US extent
#' @param cells_id a numeric vector of cell ids. See find_cells() for more
#' details. Use 1:1450 to create grids for the whole US.
#' @return a list of sf objects. The ith element of the list corresponds to the
#' ith cell id in US 100km resolution grid.
#' @importFrom terra ext vect rast as.polygons values ncell as.points
#' @export
create_empty_grids <- function(cells_id) {
  # cut US extent in approx. 100km*100km polygons
  contig_us_grid <- terra::ext(
    -124.736342,
    -66.945392,
    24.521208,
    49.382808
  ) |>
    terra::vect(crs = "epsg:4326") |>
    terra::rast(res = 1)
  # enumerate each polygon (1450 in total, 58 col * 25 row)
  terra::values(contig_us_grid) <- 1:terra::ncell(contig_us_grid)
  # raster to vector of polygons
  contig_us_grid <- terra::as.polygons(contig_us_grid)
  # change names
  names(contig_us_grid) <- "id"
  # for each polygon...
  cells <- list()
  for (i in cells_id) {
    cat("Processing cell ", i, "\n")
    #   - create a grid with 1km resolution
    cells[[i]] <- contig_us_grid[i, ] |>
      terra::rast(res = 0.01) |>
      terra::as.points()
  }
  cells
}

#' Create monthly 100km*100km grids inside US cells
#' @description Create 100km*100km grids within US extent
#' @param cells_id a numeric vector of cell ids. See find_cells() for more
#' details. Use 1:1450 to create grids for the whole US.
#' @param yyyy a numeric year
#' @param mm a numeric month
#' @param directory a character path to the grid folder
#' @importFrom terra ext vect rast as.polygons values ncell
#' @importFrom lubridate hours
#' @importFrom sf st_as_sf
#' @export
create_st_grids_from_cells <- function(cells_id, yyyy, mm, directory) {
  # add "/" to directory if not present
  chars_dir <- nchar(directory)
  if (substr(directory, chars_dir, chars_dir) != "/") {
    directory <- paste(directory, "/", sep = "")
  }
  mm_str <- ifelse(mm < 10, paste0("0", mm), as.character(mm))
  dir <- paste0(directory, yyyy, mm_str, "/")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  # cut US extent in approx. 100km*100km polygons
  contig_us_grid <- terra::ext(
    -124.736342,
    -66.945392,
    24.521208,
    49.382808
  ) |>
    terra::vect(crs = "epsg:4326") |>
    terra::rast(res = 1)
  # enumerate each polygon (1450 in total, 58 col * 25 row)
  terra::values(contig_us_grid) <- 1:terra::ncell(contig_us_grid)
  # raster to vector of polygons
  contig_us_grid <- terra::as.polygons(contig_us_grid)
  # change names
  names(contig_us_grid) <- "id"
  # create a vector of hourly timestamps for the given mm and yyyy
  d_start <- as.POSIXct(
    paste0(as.character(yyyy), "-", as.character(mm), "-01 00:00:00"),
    tz = "UTC"
  )
  dates <- seq(
    from = d_start,
    to = d_start + months(1) - lubridate::hours(1),
    by = "hour"
  )
  # for each polygon...
  for (i in cells_id) {
    cat("Processing cell ", i, "\n")
    #   - create a grid with 1km resolution
    cells <- contig_us_grid[i, ] |>
      terra::rast(res = 0.01) |>
      terra::as.points() |>
      sf::st_as_sf()
    #   - create a sf with 10000 points * length of timestamps
    grid_i <- cells[rep(seq_len(nrow(cells)), times = length(dates)), ]
    grid_i$time <- rep(dates, each = nrow(cells))
    #   - save the grid in a subdirectory directory/yyyymm/grid_i.rds
    saveRDS(
      grid_i,
      file = paste0(dir, "grid_", i, ".rds")
    )
  }
}
