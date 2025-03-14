setwd("/ddn/gs1/home/marquesel/samba/paper/")
devtools::load_all()

model <- "9"

# open input data for each case study
source("../case_study_phi_nyc/open_phi_nyc_202101.R")
input_npw <- open_phi_nyc(
  ref_network = TRUE,
  path = paste0(getwd(), "/../case_study_phi_nyc/")
)
source("../case_study_phi_nyc/open_phi_nyc_202407.R")
input_nps <- open_phi_nyc(
  ref_network = TRUE,
  path = paste0(getwd(), "/../case_study_phi_nyc/")
)
source("../case_study_phoenix/open_phoenix.R")
input_phoe <- open_phoenix(
  ref_network = TRUE,
  path = paste0(getwd(), "/../case_study_phoenix/")
)
source("../case_study_triangle/open_triangle.R")
input_tri <- open_triangle(
  ref_network = TRUE,
  path = paste0(getwd(), "/../case_study_triangle/")
)
input_npw$pred <- data.table::fread(
  file = paste0(
    "../case_study_phi_nyc/output/model",
    model,
    "_202101/model",
    model,
    "_inference_pred_",
    "2021010106_2021013105",
    ".csv"
  )
)
input_nps$pred <- data.table::fread(
  file = paste0(
    "../case_study_phi_nyc/output/model",
    model,
    "_202407/model",
    model,
    "_inference_pred_",
    "2024070105_2024073104",
    ".csv"
  )
)
input_phoe$pred <- data.table::fread(
  file = paste0(
    "../case_study_phoenix/output/model",
    model,
    "/model",
    model,
    "_inference_pred_",
    "2023070109_2023073123",
    ".csv"
  )
)
input_tri$pred <- data.table::fread(
  file = paste0(
    "../case_study_triangle/output/model",
    model,
    "/model",
    model,
    "_inference_pred_",
    "2021070105_2021073123",
    ".csv"
  )
)
cat("prediction loaded \n")
input_npw$info <- data.table::fread(
  file = paste0(
    "../case_study_phi_nyc/output/model",
    model,
    "_202101/model",
    model,
    "_inference_info_",
    "2021010106_2021013105",
    ".csv"
  )
)
input_nps$info <- data.table::fread(
  file = paste0(
    "../case_study_phi_nyc/output/model",
    model,
    "_202407/model",
    model,
    "_inference_info_",
    "2024070105_2024073104",
    ".csv"
  )
)
input_phoe$info <- data.table::fread(
  file = paste0(
    "../case_study_phoenix/output/model",
    model,
    "/model",
    model,
    "_inference_info_",
    "2023070109_2023073123",
    ".csv"
  )
)
input_tri$info <- data.table::fread(
  file = paste0(
    "../case_study_triangle/output/model",
    model,
    "/model",
    model,
    "_inference_info_",
    "2021070105_2021073123",
    ".csv"
  )
)
cat("info loaded")

if (FALSE) {
input_npw$mod <- readRDS(
  file = paste0(
    "../case_study_phi_nyc/output/model",
    model,
    "_202101/model",
    model,
    "_inference_mod_",
    "2021010106_2021013105",
    ".csv"
  )
)
input_nps$mod <- readRDS(
  file = paste0(
    "../case_study_phi_nyc/output/model",
    model,
    "_202407/model",
    model,
    "_inference_mod_",
    "2024070106_2024073104",
    ".csv"
  )
)
input_phoe$mod <- readRDS(
  file = paste0(
    "../case_study_phoenix/output/model",
    model,
    "/model",
    model,
    "_inference_mod_",
    "2023070109_2023073123",
    ".csv"
  )
)
input_tri$mod <- readRDS(
  file = paste0(
    "../case_study_triangle/output/model",
    model,
    "/model",
    model,
    "_inference_mod_",
    "2021070105_2021073123",
    ".csv"
  )
)
}

grad_alt <- function(z, temp) {
    temp <- as.numeric(temp)
    z <- as.numeric(z)
    delta  <- - 0.006 * z
    temp_sea <- temp - delta
    return(temp_sea = temp_sea)
}
for (cs in c("npw", "nps", "phoe", "tri")){
  input <- get(paste0("input_", cs))
  input$pred$pred_mean_demcorr <- grad_alt(input$pred$elev, input$pred$pred_mean)
  pred_mean_corr <- rasterize_pred(
    input$pred,
    varname = "pred_mean_demcorr"
  )
  pred_mean <- rasterize_pred(
    input$pred,
    varname = "pred_mean"
  )
  pred_sd <- rasterize_pred(
    input$pred,
    varname = "pred_sd"
  )
  pred_mean_avg <- terra::global(pred_mean, "mean", na.rm = TRUE)
  pred_mean_avg$time <- terra::time(pred_mean)
  uhi <- pred_mean - terra::global(pred_mean, "mean", na.rm = TRUE)$mean
  
  pred_mean_corr_avg <- terra::global(pred_mean_corr, "mean", na.rm = TRUE)
  pred_mean_corr_avg$time <- terra::time(pred_mean_corr)
  uhi_corr <- pred_mean_corr - terra::global(pred_mean_corr, "mean", na.rm = TRUE)$mean
  # save all uhi maps
  period <- seq(min(input$pred$time), max(input$pred$time), by = "1 hour")
  tz <- "UTC"
  # save average uhi on the period
  assign(paste0("raster_", cs), pred_mean)
  assign(paste0("uhiavg_corr_", cs), terra::mean(uhi_corr))
  assign(paste0("uhiavg_", cs), terra::mean(uhi))
  assign(paste0("raster_sd_", cs), pred_sd)
  assign(paste0("sdavg_", cs), terra::mean(pred_sd))
  # select ref stations within the rectangle area
  ref <- input$ref |>
    terra::vect() |>
    terra::project(input$area_rect) |>
    terra::intersect(input$area_rect) |>
    terra::project("epsg:4326") |>
    sf::st_as_sf()
  ref <- ref[which(
    between(ref$time, min(input$pred$time), max(input$pred$time))
  ), ]
  assign(paste0("ref_eval_", cs), add_pred_to_pro(input$pred, ref))
  cat(cs, " raster, uhi avg loaded\n")
}

for (cs in c("npw", "nps", "phoe", "tri")) {
  r <- get(paste0("raster_", cs))
  ts <- lubridate::with_tz(as.POSIXct(min(terra::time(r))), "UTC")
  te <- lubridate::with_tz(as.POSIXct(max(terra::time(r))), "UTC")
  # save rasters
  terra::writeRaster(
    x = r,
    file = paste0(
      "./output/",
      cs,
      "_inference_predmean_",
      format(ts, "%Y%m%d%H"),
      "_",
      format(te, "%Y%m%d%H"),
      ".tif"
    )
  )
}

for (cs in c("npw", "nps", "phoe", "tri")){
  input <- get(paste0("input_", cs))
# select ref stations within the rectangle area
  ref <- input$ref |>
    terra::vect() |>
    terra::project(input$area_rect) |>
    terra::intersect(input$area_rect) |>
    terra::project("epsg:4326") |>
    sf::st_as_sf()
  ref <- ref[which(
    between(ref$time, min(input$pred$time), max(input$pred$time))
  ), ]
  assign(paste0("ref_eval_", cs), add_pred_to_pro(input$pred, ref))
}

# create rural and urban points in each city to compute the UHI 
urb_pts <- rbind(
  c(-75.1, 40),
  c(-74, 40.72),
  c(-112.11, 33.49),
  c(-78.65, 35.78),
  c(-78.9, 36.01)
) |>
  as.data.frame()
colnames(urb_pts) <- c("lon", "lat")
urb_pts$city <- c("Philadelphia", "New York City", "Phoenix", "Raleigh", "Durham")
urb_pts$type <- "urb"
rur_pts <- rbind(
  c(-75.23, 40.08),
  c(-74.32, 40.72),
  c(-112.4, 33.4),
  c(-78.5, 35.75),
  c(-79.01, 36.02)
) |>
  as.data.frame()
colnames(rur_pts) <- c("lon", "lat")
rur_pts$city <- c("Philadelphia", "New York City", "Phoenix", "Raleigh", "Durham")
rur_pts$type <- "rur"
pts <- rbind(urb_pts, rur_pts) |>
  terra::vect(geom = c("lon", "lat"), crs = "epsg:4326")


# load cws
source("../case_study_phi_nyc/open_phi_nyc_202101.R")
data <- open_phi_nyc(cws = TRUE, covariates = TRUE, path = "../case_study_phi_nyc/")
input_npw$cws <- data$cws
input_npw$lcz <- data$lcz
source("../case_study_phi_nyc/open_phi_nyc_202407.R")
data <- open_phi_nyc(cws = TRUE, covariates = TRUE, path = "../case_study_phi_nyc/")
input_nps$cws <- data$cws
input_nps$lcz <- data$lcz
data <- open_phoenix(cws = TRUE, covariates = TRUE, path = "../case_study_phoenix/")
input_phoe$cws <- data$cws
input_phoe$lcz <- data$lcz
data <- open_triangle(cws = TRUE, covariates = TRUE, path = "../case_study_triangle/")
input_tri$cws <- data$cws
input_tri$lcz <- data$lcz

