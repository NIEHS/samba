#' Download ERA5 reanalysis of temperature for a given period and area
#' data is stored per month in a netcdf file
#' /!\ download quite long...
#' @param user_od character with user Copernicus UID
#' @param years vector of years to download
#' @param months vector of characters for months to download
#' (e.g. c("01", "02", "03")).
#' @param latn minimum latitude
#' @param latx maximum latitude
#' @param lonn minimum longitude
#' @param lonx maximum longitude
#' @param storage_dir: directory to save the downloaded files
#' @importFrom ecmwfr wf_set_key wf_request
#' @importFrom lubridate year
#' @importFrom sf st_bbox
download_era5 <- function(user_id,
                          years,
                          months,
                          latn,
                          latx,
                          lonn,
                          lonx,
                          storage_dir) {
  # input your login info with an interactive request
  ecmwfr::wf_set_key(service = "cds")
  ph <- storage_dir
  vr <- c("2m_temperature")
  yr <- years
  mn <- months
  dy <- c(sprintf("0%d", 1:9), 10:31)
  tm <- c(paste0("0", 0:9, ":00"), paste0(10:23, ":00"))
  for (i in seq_along(yr)) {
    for (j in seq_along(mn)) {
      request <- list(
        "dataset_short_name" = "reanalysis-era5-land",
        "product_type" = "reanalysis",
        "variable" = vr[1],
        "year" = yr[i],
        "month" = mn[j],
        "day" = dy,
        "time" = tm,
        "area" = paste0(latx, "/", lonn, "/", latn, "/", lonx),
        "format" = "netcdf",
        "target" = paste0("era5_", yr[i], "_", mn[j], ".nc")
      )
      ecmwfr::wf_request(
        user     = user_id,
        request  = request,
        transfer = TRUE,
        path     = ph
      )
    }
  }
}
