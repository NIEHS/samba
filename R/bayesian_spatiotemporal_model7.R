

#' Spatiotemporal model inference with INLA
#' @description
#' Perform inference of a spatiotemporal model using INLA.
#' @param data a dataframe with columns lon, lat, time, temp and all covariates.
#' Coordinates are in EPSG:4326.
#' @param pred a dataframe with columns lon, lat, time and all covariates.
#' Coordinates are in EPSG:4326.
#' @param polygon a terra::SpatVector with the study area
#' @param ts model start time
#' @param te model end time
#' @param verbose logical, print INLA output
#' @return a list with the prediction and the model object
#' @import INLA
#' @importFrom dplyr between rename
#' @importFrom terra as.points as.polygons buffer crs ext intersect project
#' @author Eva Marques
#' @export
inference_mod7 <- function(data,
                           pred,
                           polygon,
                           ts,
                           te,
                           verbose = FALSE,
                           debug = FALSE) {
  stopifnot(
    "lon missing" = "lon" %in% colnames(data),
    "lat missing" = "lat" %in% colnames(data),
    "time missing" = "time" %in% colnames(data),
    "temp missing" = "temp" %in% colnames(data)#,
    #"temp_cal missing" = "temp_cal" %in% colnames(data)
  )
  # check that polygon is a SpatVector
  stopifnot(
    "polygon must be a SpatVector" =
      inherits(polygon, "SpatVector")
  )
  stopifnot(
    "lon missing" = "lon" %in% colnames(pred),
    "lat missing" = "lat" %in% colnames(pred),
    "time missing" = "time" %in% colnames(pred)
  )

  # store model parameters info
  info <- list()

  # select times between ts and te
  data <- data[which(dplyr::between(data$time, ts, te)), ]
  pred <- pred[which(dplyr::between(pred$time, ts, te)), ]
  polygon <- terra::ext(polygon) |>
    terra::as.polygons(crs = terra::crs(polygon)) |>
    terra::buffer(10000, joinstyle = "mitre")
  # select locations inside the polygon
  data <- terra::vect(data, geom = c("lon", "lat"), crs = "epsg:4326") |>
    terra::project(polygon) |>
    terra::intersect(polygon) |>
    terra::project("epsg:4326") |>
    as.data.frame(geom = "xy") |>
    dplyr::rename(lon = "x", lat = "y")
  pred <- terra::vect(pred, geom = c("lon", "lat"), crs = "epsg:4326") |>
    terra::project(polygon) |>
    terra::intersect(polygon) |>
    terra::project("epsg:4326") |>
    as.data.frame(geom = "xy") |>
    dplyr::rename(lon = "x", lat = "y")

  cat("Number of data points: ", nrow(data), "\n")
  cat("Number of prediction points: ", nrow(pred), "\n")

  # scale covariates
  covar <- c(
    "elev",
    "slope",
    "tcc",
    "imp",
    "fch",
    "bf",
    "era5_t2m", # 2 metre temperature
    "era5_rh", # Relative humidity deduced from t2m and d2m 
    "era5_lai_lv", # Leaf area index, low vegetation
    "era5_lai_hv", # Leaf area index, high vegetation
    "era5_u10", # 10 metre U wind component
    "era5_v10", # 10 metre V wind component
    "era5_stl1", # Soil temperature level 1
    "era5_tcc", # Total cloud cover
    "era5_tp", # Total precipitation
    "era5_slhf", # Surface latent heat flux
    "era5_ssr", # Surface net short-wave (solar) radiation
    "era5_sshf", # Surface sensible heat flux
    "era5_ssrd", # Surface short-wave (solar) radiation downwards
    "era5_e" # Evaporation
  )
  for (p in covar) {
    data[[p]] <- scale(data[[p]],
      center = mean(pred[[p]], na.rm = TRUE),
      scale = sd(pred[[p]], na.rm = TRUE)
    )
    pred[[p]] <- scale(pred[[p]],
      center = mean(pred[[p]], na.rm = TRUE),
      scale = sd(pred[[p]], na.rm = TRUE)
    )
  }

  info$mesh_max_edge <- 0.1
  info$mesh_cutoff <- 0.005
  # spatial mesh
  domain <- polygon |>
    terra::project("epsg:4326") |>
    terra::as.points()
  locs <- unique(data[, c("lon", "lat")])
  mesh_s <- inla.mesh.2d(
    loc = cbind(locs$lon, locs$lat),
    max.edge = info$mesh_max_edge,
    cutoff = info$mesh_cutoff,
    loc.domain = cbind(
      terra::geom(domain)[, c("x")],
      terra::geom(domain)[, c("y")]
    )
  )
  # save mesh to ./graphs/mesh.png
  # png("./graphs/mesh.png")
  # plot(mesh_s)
  # dev.off()

  # temporal mesh
  timedim <- list(
    time = sort(unique(data$time)),
    timeidx = seq_along(unique(data$time))
  ) |>
    as.data.frame()
  data <- merge(data, timedim, by = "time")
  pred <- merge(pred, timedim, by = "time")
  data$hour <- lubridate::hour(data$time) + 1
  pred$hour <- lubridate::hour(pred$time) + 1
  # spde model: p(range < r0) = p_r0 and p(sigma > sd0) = p_sd0
  info$pcrange_thresh <- 0.1
  info$pcrange_proba <- 0.99
  info$pcsigma_thresh <- 4
  info$pcsigma_proba <- 0.01
  spde <- INLA::inla.spde2.pcmatern(
    mesh_s,
    prior.range = c(info$pcrange_thresh, info$pcrange_proba),
    prior.sigma = c(info$pcsigma_thresh, info$pcsigma_proba)
  )
  m <- spde$n.spde
  # projection matrices A and Ap (p for prediction)
  # dim(a_st) = nrow(data), nrow(timedim) x m
  a_st <- INLA::inla.spde.make.A(
    mesh = mesh_s,
    loc = cbind(data$lon, data$lat),
    group = data$timeidx
  )
  # dim(ap_st) = nrow(pred), nrow(timedim) x m
  ap_st <- INLA::inla.spde.make.A(
    mesh = mesh_s,
    loc = cbind(pred$lon, pred$lat),
    group = pred$timeidx
  )
  # space-time index
  # --> s: indices of the SPDE vertices repeated the number of times,
  # --> s.group: indices of the times repeated the number of mesh vertices,
  # --> s.repl: vector of 1s with length given by the number of mesh vertices
  #             times the number of times (m*nrow(timedim)).
  s_idx <- INLA::inla.spde.make.index("s", n.spde = m, n.group = nrow(timedim))

  # data wrapper
  stk_data <- INLA::inla.stack(
    tag = "data",
    data = list(
      #y = data$temp_cal
      y = data$temp
    ),
    A = list(1, a_st),
    effects = list(
      data.frame(
        int = rep(1, nrow(data)),
        elev = data$elev,
        tcc = data$tcc,
        imp = data$imp,
        fch = data$fch,
        bf = data$bf,
        era5_t2m = data$era5_t2m,
        era5_rh = data$era5_rh,
        era5_lai_lv = data$era5_lai_lv,
        era5_lai_hv = data$era5_lai_hv,
        era5_u10 = data$era5_u10,
        era5_v10 = data$era5_v10,
        era5_stl1 = data$era5_stl1,
        era5_tp = data$era5_tp,
        era5_slhf = data$era5_slhf,
        era5_ssr = data$era5_ssr,
        era5_sshf = data$era5_sshf,
        era5_ssrd = data$era5_ssrd,
        era5_tcc = data$era5_tcc,
        timeidx = data$timeidx,
        hour = as.factor(data$hour)
      ),
      s = s_idx
    )
  )
  stk_pred <- INLA::inla.stack(
    tag = "pred",
    data = list(y = rep(NA, nrow(pred))),
    A = list(1, ap_st),
    effects = list(
      data.frame(
        int = rep(1, nrow(pred)),
        elev = pred$elev,
        tcc = pred$tcc,
        imp = pred$imp,
        fch = pred$fch,
        bf = pred$bf,
        era5_t2m = pred$era5_t2m,
        era5_rh = pred$era5_rh,
        era5_lai_lv = pred$era5_lai_lv,
        era5_lai_hv = pred$era5_lai_hv,
        era5_u10 = pred$era5_u10,
        era5_v10 = pred$era5_v10,
        era5_stl1 = pred$era5_stl1,
        era5_tp = pred$era5_tp,
        era5_slhf = pred$era5_slhf,
        era5_ssr = pred$era5_ssr,
        era5_sshf = pred$era5_sshf,
        era5_ssrd = pred$era5_ssrd,
        era5_tcc = pred$era5_tcc,
        timeidx = pred$timeidx,
        hour = as.factor(pred$hour)
      ),
      s = s_idx
    )
  )
  stk_full <- INLA::inla.stack(stk_data, stk_pred)

  # rho = correlation with p(rho>0.5)=0.7 (prior for ar1)
  # p(rho > 0.6) = 0.9
  info$pccor1_thresh <- 0.6
  info$pccor1_proba <- 0.9

  # data variance prior
  info$s2_data_prior <- 0.4 # set with sd(cws$bias)**2

  est_loggamma_param <- function(a, b) {
    s <- runif(n = 50000, min = a, max = b)
    prec <- 1 / s**2
    a2 <- mean(prec)**2 / var(prec)
    b2 <- a2 / mean(prec)
    return(list(a2 = a2, b2 = b2))
  }
  prec_param <- est_loggamma_param(info$s2_data_prior - 0.2,
                                   info$s2_data_prior + 0.2)
  info$prior_a2 <- prec_param$a2
  info$prior_b2 <- prec_param$b2
  control_family_loggamma <- list(
    hyper = list(
      prec = list(
        prior = "loggamma",
        param = c(info$a2, info$b2)
      )
    )
  )

  # model inference
  formula <- y ~ -1 + f(timeidx, model = "ar1") +
    hour:elev + hour:fch + hour:bf + hour:imp + hour:tcc +
    era5_t2m + era5_rh + era5_slhf + era5_sshf + era5_ssrd +
    era5_tp + era5_u10 + era5_v10 + era5_tcc +
    f(s,
      model = spde, # within group effect
      group = s.group,
      control.group = list(
        model = "ar1", # between group  effect
        hyper = list(
          rho = list(
            prior = "pccor1",
            param = c(info$pccor1_thresh, info$pccor1_proba)))
      )
    )
  # -- to faster the algo
  cinla <- list(strategy = "adaptive", int.strategy = "eb")
  mod <- INLA::inla(
    formula = formula,
    data = INLA::inla.stack.data(stk_full),
    family = c("gaussian"),
    control.family = control_family_loggamma,
    control.predictor = list(
      compute = TRUE,
      A = INLA::inla.stack.A(stk_full)
    ),
    # see https://becarioprecario.bitbucket.io/spde-gitbook/"
    # 7 space-time models, discrete-time-domain
    control.fixed = list(
      mean = list(elev = -0.0065),
      prec = list(elev = 10),
      expand.factor.strategy = "inla"),
    control.compute = list(config = TRUE),
    control.inla = cinla,
    verbose = verbose,
    debug = debug
  )
  index <- INLA::inla.stack.index(stk_full, tag = "pred")$data
  pred$pred_mean <- mod$summary.fitted.values[index, "mean"]
  pred$pred_ll <- mod$summary.fitted.values[index, "0.025quant"]
  pred$pred_ul <- mod$summary.fitted.values[index, "0.975quant"]
  pred$pred_sd <- mod$summary.fitted.values[index, "sd"]
  info <- as.data.frame(info)
  info <- store_post_info(mod, info)

  return(list(pred = pred, mod = mod, info = info))
}