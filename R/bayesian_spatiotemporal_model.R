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
inference <- function(data, pred, polygon, ts, te, verbose = FALSE) {
  stopifnot(
    "lon missing" = "lon" %in% colnames(data),
    "lat missing" = "lat" %in% colnames(data),
    "time missing" = "time" %in% colnames(data),
    "temp missing" = "temp" %in% colnames(data)
  )
  # check that polygon is a SpatVector
  stopifnot("polygon must be a SpatVector" = inherits(polygon, "SpatVector"))
  stopifnot(
    "lon missing" = "lon" %in% colnames(pred),
    "lat missing" = "lat" %in% colnames(pred),
    "time missing" = "time" %in% colnames(pred)
  )
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
  # spatial mesh
  domain <- polygon |>
    terra::project("epsg:4326") |>
    terra::as.points()
  mesh_s <- inla.mesh.2d(
    loc = cbind(data$lon, data$lat),
    max.edge = 0.1,
    cutoff = 0.005,
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
  # spde model: p(range < r0) = p_r0 and p(sigma > sd0) = p_sd0
  spde <- INLA::inla.spde2.pcmatern(mesh_s,
    prior.range = c(0.5, 0.99),
    prior.sigma = c(2, 0.01)
  )
  m <- spde$n.spde
  # projection matrices A and Ap (p for prediction)
  a_st <- INLA::inla.spde.make.A(
    mesh = mesh_s,
    loc = cbind(data$lon, data$lat),
    group = data$timeidx
  )
  ap_st <- INLA::inla.spde.make.A(
    mesh = mesh_s,
    loc = cbind(pred$lon, pred$lat),
    group = pred$timeidx
  )
  # space-time index
  s_idx <- INLA::inla.spde.make.index("s", n.spde = m, n.group = nrow(timedim))
  # data wrapper
  stk_data <- INLA::inla.stack(
    tag = "data",
    data = list(y = data$temp_cal),
    A = list(1, a_st),
    effects = list(
      data.frame(
        int = rep(1, nrow(data)),
        era5 = data$era5,
        elev = data$elev,
        tcc = data$tcc,
        imp = data$imp,
        fch = data$fch,
        bf = data$bf,
        frac_6_500m = data$frac_6_500m,
        frac_10_500m = data$frac_10_500m,
        frac_A_500m = data$frac_A_500m,
        frac_B_500m = data$frac_B_500m,
        frac_D_500m = data$frac_D_500m,
        frac_E_500m = data$frac_E_500m,
        frac_F_500m = data$frac_F_500m,
        frac_G_500m = data$frac_G_500m,
        timeidx = data$timeidx
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
        era5 = pred$era5,
        elev = pred$elev,
        tcc = pred$tcc,
        imp = pred$imp,
        fch = pred$fch,
        bf = pred$bf,
        frac_6_500m = pred$frac_6_500m,
        frac_10_500m = pred$frac_10_500m,
        frac_A_500m = pred$frac_A_500m,
        frac_B_500m = pred$frac_B_500m,
        frac_D_500m = pred$frac_D_500m,
        frac_E_500m = pred$frac_E_500m,
        frac_F_500m = pred$frac_F_500m,
        frac_G_500m = pred$frac_G_500m,
        timeidx = pred$timeidx
      ),
      s = s_idx
    )
  )
  stk_full <- INLA::inla.stack(stk_data, stk_pred)
  # model inference, rho = correlation with p(rho>0.5)=0.7 (prior for ar1)
  formula <- y ~ -1 + int + era5 + elev + imp + f(s,
    model = spde,
    group = s.group,
    control.group = list(
      model = "ar1",
      hyper = list(theta = list(prior = "pccor1", param = c(0.5, 0.7)))
    )
  )
  # -- to faster the algo
  cinla <- list(strategy = "adaptive", int.strategy = "eb")
  mod <- INLA::inla(
    formula = formula,
    data = INLA::inla.stack.data(stk_full),
    family = c("gaussian"),
    control.predictor = list(
      compute = TRUE,
      A = INLA::inla.stack.A(stk_full)
    ),
    control.inla = cinla,
    verbose = verbose
  )
  index <- INLA::inla.stack.index(stk_full, tag = "pred")$data
  pred$pred_mean <- mod$summary.fitted.values[index, "mean"]
  pred$pred_ll <- mod$summary.fitted.values[index, "0.025quant"]
  pred$pred_ul <- mod$summary.fitted.values[index, "0.975quant"]
  pred$pred_sd <- mod$summary.fitted.values[index, "sd"]
  return(list(pred = pred, mod = mod))
}
