#' Check raw cws format
#' @description
#' Check raw cws format
#' @param cws_raw citizen weather station dataset
#' @author Eva Marques
#' @export
check_cws_raw <- function(cws_raw) {
  stopifnot(inherits(cws_raw, "SpatVector"))
  var <- c("lon", "lat", "time", "temp", "site_id")
  stopifnot("missing columns in cws_raw" = all(var %in% names(cws_raw)))
  message("  cws_raw valid!")
}

#' Check forest canopy height format
#' @description
#' Check forest canopy height format
#' @param fch forest canopy height
#' @author Eva Marques
#' @export
check_fch <- function(fch) {
  stopifnot(inherits(fch, "SpatRaster"))
  message("  fch valid!")
}

#' Check elevation format
#' @description
#' Check elevation format
#' @param elev elevation
#' @author Eva Marques
#' @export
check_elev <- function(elev) {
  stopifnot(inherits(elev, "SpatRaster"))
  message("  elev valid!")
}

#' Check imperviousness format
#' @description
#' Check imperviousness format
#' @param imp imperviousness
#' @author Eva Marques
#' @export
check_imp <- function(imp) {
  stopifnot(inherits(imp, "SpatRaster"))
  message("  imp valid!")
}

#' Check era5 instantaneous format
#' @description
#' Check era5 instantaneous format
#' @param era5_instant era5 instantaneous
#' @author Eva Marques
#' @export
check_era5_instant <- function(era5_instant) {
  stopifnot(inherits(era5_instant, "SpatRaster"))
  message("  era5_instant valid!")
}

#' Check era5 accumulated format
#' @description
#' Check era5 accumulated format
#' @param era5_accum era5 accumulated
#' @author Eva Marques
#' @export
check_era5_accum <- function(era5_accum) {
  stopifnot(inherits(era5_accum, "SpatRaster"))
  message("  accum valid!")
}

#' Check area shapefile format
#' @description
#' Check area shapefile  format
#' @param area_shp area shapefile
#' @author Eva Marques
#' @export
check_area_shp <- function(area_shp) {
  stopifnot(inherits(area_shp, "SpatVector"))
  message("  area_shp valid!")
}

#' Check time start and time end format
#' @description
#' Check time start and time end  format
#' @param ts time start
#' @param te time end
#' @author Eva Marques
#' @export
check_ts_te <- function(ts, te) {
  stopifnot("ts is not a POSIXct" = inherits(ts, "POSIXct"))
  stopifnot("te is not a POSIXct" = inherits(te, "POSIXct"))
  stopifnot("ts is more recent than te" = ts < te)
}

#' Check input list format
#' @description
#' Check the content of the input list
#' @param input a list with all elements needed for running the Bayesian model
#' @author Eva Marques
#' @export
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
