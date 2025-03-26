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
#' @param debug logical, print INLA debugging info
#' @return a list with the prediction and the model object
#' @import INLA
#' @importFrom terra ext as.polygons buffer crs vect project intersect
#' as.points geom
#' @importFrom lubridate with_tz hour
#' @importFrom lutz tz_lookup_coords
#' @importFrom dplyr between rename
#' @importFrom terra as.points as.polygons buffer crs ext intersect project
#' @author Eva Marques
#' @export
inference <- function(
  data,
  pred,
  polygon,
  ts,
  te,
  verbose = FALSE,
  debug = FALSE
) {
  stopifnot(
    "lon missing" = "lon" %in% colnames(data),
    "lat missing" = "lat" %in% colnames(data),
    "time missing" = "time" %in% colnames(data),
    "temp missing" = "temp" %in% colnames(data),
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

  info$mesh_max_edge <- 0.1
  info$mesh_cutoff <- 0.005
  # spatial mesh
  domain <- polygon |>
    terra::project("epsg:4326") |>
    terra::as.points()
  locs <- unique(data[, c("lon", "lat")])
  mesh_s <- INLA::inla.mesh.2d(
    loc = cbind(locs$lon, locs$lat),
    max.edge = info$mesh_max_edge,
    cutoff = info$mesh_cutoff,
    loc.domain = cbind(
      terra::geom(domain)[, c("x")],
      terra::geom(domain)[, c("y")]
    )
  )
  # temporal mesh
  timedim <- list(
    time = sort(unique(data$time)),
    timeidx = seq_along(unique(data$time))
  ) |>
    as.data.frame()
  data <- merge(data, timedim, by = "time")
  pred <- merge(pred, timedim, by = "time")
  data$utc <- lubridate::hour(data$time)
  pred$utc <- lubridate::hour(pred$time)
  data$tz <- lutz::tz_lookup_coords(data$lat, data$lon)
  pred$tz <- lutz::tz_lookup_coords(pred$lat, pred$lon)
  data$local_hour <- lubridate::with_tz(data$time, tz = data$tz) |>
    lubridate::hour()
  pred$local_hour <- lubridate::with_tz(pred$time, tz = pred$tz) |>
    lubridate::hour()

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
      y = data$temp
    ),
    A = list(1, a_st),
    effects = list(
      data.frame(
        int = rep(1, nrow(data)),
        elev = data$elev,
        imp = data$imp,
        fch = data$fch,
        era5_t2m = data$era5_t2m,
        era5_rh = data$era5_rh,
        era5_u10 = data$era5_u10,
        era5_v10 = data$era5_v10,
        era5_tp = data$era5_tp,
        era5_tcc = data$era5_tcc,
        timeidx = data$timeidx,
        local_hour = as.factor(data$local_hour)
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
        imp = pred$imp,
        fch = pred$fch,
        era5_t2m = pred$era5_t2m,
        era5_rh = pred$era5_rh,
        era5_u10 = pred$era5_u10,
        era5_v10 = pred$era5_v10,
        era5_tp = pred$era5_tp,
        era5_tcc = pred$era5_tcc,
        timeidx = pred$timeidx,
        local_hour = as.factor(pred$local_hour)
      ),
      s = s_idx
    )
  )
  stk_full <- INLA::inla.stack(stk_data, stk_pred)

  # rho = correlation with p(rho>0.5)=0.7 (prior for ar1)
  # and p(rho > 0.6) = 0.9
  info$pccor1_thresh <- 0.6
  info$pccor1_proba <- 0.9

  # data variance prior
  info$s2n_data_prior <- 0.4
  info$s2x_data_prior <- 1.9

  est_loggamma_param <- function(a, b) {
    s <- runif(n = 50000, min = a, max = b)
    prec <- 1 / s**2
    a2 <- mean(prec)**2 / var(prec)
    b2 <- a2 / mean(prec)
    list(a2 = a2, b2 = b2)
  }
  prec_param <- est_loggamma_param(info$s2n_data_prior,
                                   info$s2x_data_prior)
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
  formula <- y ~ -1 + int + local_hour:elev + local_hour:fch + local_hour:imp +
    era5_t2m + era5_rh + era5_tp + era5_u10 + era5_v10 + era5_tcc +
    f(s,
      model = spde, # within group effect
      group = s.group,
      control.group = list(
        model = "ar1", # between group  effect
        hyper = list(
          rho = list(
            prior = "pccor1",
            param = c(info$pccor1_thresh, info$pccor1_proba)
          )
        )
      )
    )
  # -- to faster the algo
  cinla <- list(strategy = "adaptive", int.strategy = "eb")
  # compute only the hyper-parameters modes only for stk_data
  mod_mode <- INLA::inla(
    formula = formula,
    data = INLA::inla.stack.data(stk_data),
    family = c("gaussian"),
    control.family = control_family_loggamma,
    control.predictor = list(
      compute = FALSE,
      A = INLA::inla.stack.A(stk_data)
    ),
    # see https://becarioprecario.bitbucket.io/spde-gitbook/"
    # 7 space-time models, discrete-time-domain
    control.fixed = list(
      mean = list(
        int = 0,
        era5_t2m = 1,
        era5_rh = 0,
        era5_tcc = 0,
        `local_hour0:elev` = -0.006,
        `local_hour1:elev` = -0.006,
        `local_hour2:elev` = -0.006,
        `local_hour3:elev` = -0.006,
        `local_hour4:elev` = -0.006,
        `local_hour5:elev` = -0.006,
        `local_hour6:elev` = -0.006,
        `local_hour7:elev` = -0.006,
        `local_hour8:elev` = -0.006,
        `local_hour9:elev` = -0.006,
        `local_hour10:elev` = -0.006,
        `local_hour11:elev` = -0.006,
        `local_hour12:elev` = -0.006,
        `local_hour13:elev` = -0.006,
        `local_hour14:elev` = -0.006,
        `local_hour15:elev` = -0.006,
        `local_hour16:elev` = -0.006,
        `local_hour17:elev` = -0.006,
        `local_hour18:elev` = -0.006,
        `local_hour19:elev` = -0.006,
        `local_hour20:elev` = -0.006,
        `local_hour21:elev` = -0.006,
        `local_hour22:elev` = -0.006,
        `local_hour23:elev` = -0.006,
        `local_hour0:fch` = 0,
        `local_hour1:fch` = 0,
        `local_hour2:fch` = 0,
        `local_hour3:fch` = 0,
        `local_hour4:fch` = 0,
        `local_hour5:fch` = 0,
        `local_hour6:fch` = 0,
        `local_hour7:fch` = 0,
        `local_hour8:fch` = 0,
        `local_hour9:fch` = 0,
        `local_hour10:fch` = 0,
        `local_hour11:fch` = 0,
        `local_hour12:fch` = 0,
        `local_hour13:fch` = 0,
        `local_hour14:fch` = 0,
        `local_hour15:fch` = 0,
        `local_hour16:fch` = 0,
        `local_hour17:fch` = 0,
        `local_hour18:fch` = 0,
        `local_hour19:fch` = 0,
        `local_hour20:fch` = 0,
        `local_hour21:fch` = 0,
        `local_hour22:fch` = 0,
        `local_hour23:fch` = 0,
        `local_hour0:imp` = 0,
        `local_hour1:imp` = 0,
        `local_hour2:imp` = 0,
        `local_hour3:imp` = 0,
        `local_hour4:imp` = 0,
        `local_hour5:imp` = 0,
        `local_hour6:imp` = 0,
        `local_hour7:imp` = 0,
        `local_hour8:imp` = 0,
        `local_hour9:imp` = 0,
        `local_hour10:imp` = 0,
        `local_hour11:imp` = 0,
        `local_hour12:imp` = 0,
        `local_hour13:imp` = 0,
        `local_hour14:imp` = 0,
        `local_hour15:imp` = 0,
        `local_hour16:imp` = 0,
        `local_hour17:imp` = 0,
        `local_hour18:imp` = 0,
        `local_hour19:imp` = 0,
        `local_hour20:imp` = 0,
        `local_hour21:imp` = 0,
        `local_hour22:imp` = 0,
        `local_hour23:imp` = 0
      ),
      prec = list(
        int = 10,
        era5_t2m = 10,
        era5_rh = 0.02,
        era5_tcc = 0.02,
        `local_hour0:elev` = 10000,
        `local_hour1:elev` = 10000,
        `local_hour2:elev` = 10000,
        `local_hour3:elev` = 10000,
        `local_hour4:elev` = 10000,
        `local_hour5:elev` = 10000,
        `local_hour6:elev` = 10000,
        `local_hour7:elev` = 10000,
        `local_hour8:elev` = 10000,
        `local_hour9:elev` = 10000,
        `local_hour10:elev` = 10000,
        `local_hour11:elev` = 10000,
        `local_hour12:elev` = 10000,
        `local_hour13:elev` = 10000,
        `local_hour14:elev` = 10000,
        `local_hour15:elev` = 10000,
        `local_hour16:elev` = 10000,
        `local_hour17:elev` = 10000,
        `local_hour18:elev` = 10000,
        `local_hour19:elev` = 10000,
        `local_hour20:elev` = 10000,
        `local_hour21:elev` = 10000,
        `local_hour22:elev` = 10000,
        `local_hour23:elev` = 10000,
        `local_hour0:fch` = 100,
        `local_hour1:fch` = 100,
        `local_hour2:fch` = 100,
        `local_hour3:fch` = 100,
        `local_hour4:fch` = 100,
        `local_hour5:fch` = 100,
        `local_hour6:fch` = 100,
        `local_hour7:fch` = 100,
        `local_hour8:fch` = 100,
        `local_hour9:fch` = 100,
        `local_hour10:fch` = 100,
        `local_hour11:fch` = 100,
        `local_hour12:fch` = 100,
        `local_hour13:fch` = 100,
        `local_hour14:fch` = 100,
        `local_hour15:fch` = 100,
        `local_hour16:fch` = 100,
        `local_hour17:fch` = 100,
        `local_hour18:fch` = 100,
        `local_hour19:fch` = 100,
        `local_hour20:fch` = 100,
        `local_hour21:fch` = 100,
        `local_hour22:fch` = 100,
        `local_hour23:fch` = 100,
        `local_hour0:imp` = 200,
        `local_hour1:imp` = 200,
        `local_hour2:imp` = 200,
        `local_hour3:imp` = 200,
        `local_hour4:imp` = 200,
        `local_hour5:imp` = 200,
        `local_hour6:imp` = 200,
        `local_hour7:imp` = 200,
        `local_hour8:imp` = 200,
        `local_hour9:imp` = 200,
        `local_hour10:imp` = 200,
        `local_hour11:imp` = 200,
        `local_hour12:imp` = 200,
        `local_hour13:imp` = 200,
        `local_hour14:imp` = 200,
        `local_hour15:imp` = 200,
        `local_hour16:imp` = 200,
        `local_hour17:imp` = 200,
        `local_hour18:imp` = 200,
        `local_hour19:imp` = 200,
        `local_hour20:imp` = 200,
        `local_hour21:imp` = 200,
        `local_hour22:imp` = 200,
        `local_hour23:imp` = 200
      ),
      expand.factor.strategy = "inla"
    ),
    control.compute = list(config = TRUE),
    control.inla = cinla,
    verbose = verbose,
    debug = debug
  )
  cat("mod model done...")
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
      mean = list(
        int = 0,
        era5_t2m = 1,
        era5_rh = 0,
        era5_tcc = 0,
        `local_hour0:elev` = -0.006,
        `local_hour1:elev` = -0.006,
        `local_hour2:elev` = -0.006,
        `local_hour3:elev` = -0.006,
        `local_hour4:elev` = -0.006,
        `local_hour5:elev` = -0.006,
        `local_hour6:elev` = -0.006,
        `local_hour7:elev` = -0.006,
        `local_hour8:elev` = -0.006,
        `local_hour9:elev` = -0.006,
        `local_hour10:elev` = -0.006,
        `local_hour11:elev` = -0.006,
        `local_hour12:elev` = -0.006,
        `local_hour13:elev` = -0.006,
        `local_hour14:elev` = -0.006,
        `local_hour15:elev` = -0.006,
        `local_hour16:elev` = -0.006,
        `local_hour17:elev` = -0.006,
        `local_hour18:elev` = -0.006,
        `local_hour19:elev` = -0.006,
        `local_hour20:elev` = -0.006,
        `local_hour21:elev` = -0.006,
        `local_hour22:elev` = -0.006,
        `local_hour23:elev` = -0.006,
        `local_hour0:fch` = 0,
        `local_hour1:fch` = 0,
        `local_hour2:fch` = 0,
        `local_hour3:fch` = 0,
        `local_hour4:fch` = 0,
        `local_hour5:fch` = 0,
        `local_hour6:fch` = 0,
        `local_hour7:fch` = 0,
        `local_hour8:fch` = 0,
        `local_hour9:fch` = 0,
        `local_hour10:fch` = 0,
        `local_hour11:fch` = 0,
        `local_hour12:fch` = 0,
        `local_hour13:fch` = 0,
        `local_hour14:fch` = 0,
        `local_hour15:fch` = 0,
        `local_hour16:fch` = 0,
        `local_hour17:fch` = 0,
        `local_hour18:fch` = 0,
        `local_hour19:fch` = 0,
        `local_hour20:fch` = 0,
        `local_hour21:fch` = 0,
        `local_hour22:fch` = 0,
        `local_hour23:fch` = 0,
        `local_hour0:imp` = 0,
        `local_hour1:imp` = 0,
        `local_hour2:imp` = 0,
        `local_hour3:imp` = 0,
        `local_hour4:imp` = 0,
        `local_hour5:imp` = 0,
        `local_hour6:imp` = 0,
        `local_hour7:imp` = 0,
        `local_hour8:imp` = 0,
        `local_hour9:imp` = 0,
        `local_hour10:imp` = 0,
        `local_hour11:imp` = 0,
        `local_hour12:imp` = 0,
        `local_hour13:imp` = 0,
        `local_hour14:imp` = 0,
        `local_hour15:imp` = 0,
        `local_hour16:imp` = 0,
        `local_hour17:imp` = 0,
        `local_hour18:imp` = 0,
        `local_hour19:imp` = 0,
        `local_hour20:imp` = 0,
        `local_hour21:imp` = 0,
        `local_hour22:imp` = 0,
        `local_hour23:imp` = 0
      ),
      prec = list(
        int = 10,
        era5_t2m = 10,
        era5_rh = 0.02,
        era5_tcc = 0.02,
        `local_hour0:elev` = 10000,
        `local_hour1:elev` = 10000,
        `local_hour2:elev` = 10000,
        `local_hour3:elev` = 10000,
        `local_hour4:elev` = 10000,
        `local_hour5:elev` = 10000,
        `local_hour6:elev` = 10000,
        `local_hour7:elev` = 10000,
        `local_hour8:elev` = 10000,
        `local_hour9:elev` = 10000,
        `local_hour10:elev` = 10000,
        `local_hour11:elev` = 10000,
        `local_hour12:elev` = 10000,
        `local_hour13:elev` = 10000,
        `local_hour14:elev` = 10000,
        `local_hour15:elev` = 10000,
        `local_hour16:elev` = 10000,
        `local_hour17:elev` = 10000,
        `local_hour18:elev` = 10000,
        `local_hour19:elev` = 10000,
        `local_hour20:elev` = 10000,
        `local_hour21:elev` = 10000,
        `local_hour22:elev` = 10000,
        `local_hour23:elev` = 10000,
        `local_hour0:fch` = 100,
        `local_hour1:fch` = 100,
        `local_hour2:fch` = 100,
        `local_hour3:fch` = 100,
        `local_hour4:fch` = 100,
        `local_hour5:fch` = 100,
        `local_hour6:fch` = 100,
        `local_hour7:fch` = 100,
        `local_hour8:fch` = 100,
        `local_hour9:fch` = 100,
        `local_hour10:fch` = 100,
        `local_hour11:fch` = 100,
        `local_hour12:fch` = 100,
        `local_hour13:fch` = 100,
        `local_hour14:fch` = 100,
        `local_hour15:fch` = 100,
        `local_hour16:fch` = 100,
        `local_hour17:fch` = 100,
        `local_hour18:fch` = 100,
        `local_hour19:fch` = 100,
        `local_hour20:fch` = 100,
        `local_hour21:fch` = 100,
        `local_hour22:fch` = 100,
        `local_hour23:fch` = 100,
        `local_hour0:imp` = 200,
        `local_hour1:imp` = 200,
        `local_hour2:imp` = 200,
        `local_hour3:imp` = 200,
        `local_hour4:imp` = 200,
        `local_hour5:imp` = 200,
        `local_hour6:imp` = 200,
        `local_hour7:imp` = 200,
        `local_hour8:imp` = 200,
        `local_hour9:imp` = 200,
        `local_hour10:imp` = 200,
        `local_hour11:imp` = 200,
        `local_hour12:imp` = 200,
        `local_hour13:imp` = 200,
        `local_hour14:imp` = 200,
        `local_hour15:imp` = 200,
        `local_hour16:imp` = 200,
        `local_hour17:imp` = 200,
        `local_hour18:imp` = 200,
        `local_hour19:imp` = 200,
        `local_hour20:imp` = 200,
        `local_hour21:imp` = 200,
        `local_hour22:imp` = 200,
        `local_hour23:imp` = 200
      ),
      expand.factor.strategy = "inla"
    ),
    control.compute = list(config = TRUE),
    control.mode = list(theta = mod_mode$mode$theta, restart = FALSE),
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
  list(pred = pred, mod = mod, info = info)
}
