samba <- function(
  ts,
  te,
  area_shp_path,
  cws_raw_path,
  fch_path,
  elev_path,
  imp_path,
  era5_accum_path,
  era5_instant_path
) {
  message("...load input...")
  input <- list()
  input$cws_raw <- readRDS(cws_raw_path)$obs |>
    terra::vect(crs = "epsg:4326", keepgeom = TRUE)
  input$fch <- terra::rast(fch_path)
  input$elev <- terra::rast(elev_path)
  input$imp <- terra::rast(imp_path)
  input$era5_instant <- terra::rast(era5_instant_path)
  input$era5_accum <- terra::rast(era5_accum_path)
  input$area_shp <- terra::vect(area_shp_path)
  input$ts <- ts
  input$te <- te
  message("...check input...")
  check_input(input)
  message("...prepare BHM materials...")
  # bhm_materials = list with cws, pred, area_rect
  bhm_materials <- prepare_bhm_materials(
    input,
    era5_accum_path,
    era5_instant_path
  )
  message("...BHM inference...")
  inf_out <- inference(
    bhm_materials$cws,
    bhm_materials$pred,
    polygon = bhm_materials$area_rect,
    input$ts,
    input$te,
    verbose = TRUE,
    debug = TRUE
  )
  message("...predictions rasterization...")
  # rasterize pred_mean and pred_sd
  pred_mean <- rasterize_pred(inf_out$pred, varname = "pred_mean")
  pred_sd <- rasterize_pred(inf_out$pred, varname = "pred_sd")
  output <- list(
    "input" = input,
    "bhm_materials" = bhm_materials,
    "inf_out" = inf_out,
    "pred_mean" = pred_mean,
    "pred_sd" = pred_sd
  )
  message("...work completed!")
  return(output)
}
