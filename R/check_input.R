check_cws_raw <- function(cws_raw) {
  stopifnot(inherits(cws_raw, "SpatVector"))
  var <- c("lon", "lat", "time", "temp", "site_id")
  stopifnot("missing columns in cws_raw" = all(var %in% names(cws_raw)))
  message("  cws_raw valid!")
}

check_fch <- function(fch) {
  stopifnot(inherits(fch, "SpatRaster"))
  message("  fch valid!")
}

check_elev <- function(elev) {
  stopifnot(inherits(elev, "SpatRaster"))
  message("  elev valid!")
}

check_imp <- function(imp) {
  stopifnot(inherits(imp, "SpatRaster"))
  message("  imp valid!")
}

check_era5_instant <- function(era5_instant) {
  stopifnot(inherits(era5_instant, "SpatRaster"))
  message("  era5_instant valid!")
}

check_era5_accum <- function(era5_accum) {
  stopifnot(inherits(era5_accum, "SpatRaster"))
  message("  accum valid!")
}

check_area_shp <- function(area_shp) {
  stopifnot(inherits(area_shp, "SpatVector"))
  message("  area_shp valid!")
}

check_ts_te <- function(ts, te) {
  stopifnot("ts is not a POSIXct" = inherits(ts, "POSIXct"))
  stopifnot("te is not a POSIXct" = inherits(te, "POSIXct"))
  stopifnot("ts is more recent than te" = ts < te)
}

check_input <- function(input) {
  content <- c(
    "cws_raw",
    "elev",
    "fch",
    "imp",
    "era5_instant",
    "era5_accum",
    "area_shp"
  )
  stopifnot("missing data in input" = all(content %in% names(input)))
  check_cws_raw(input$cws_raw)
  check_elev(input$elev)
  check_fch(input$fch)
  check_imp(input$imp)
  check_era5_instant(input$era5_instant)
  check_era5_accum(input$era5_accum)
  check_area_shp(input$area_shp)
  check_ts_te(input$ts, input$te)
}
