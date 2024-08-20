#' Spatiotemporal model inference with INLA
#' @description
#' Perform inference on a spatiotemporal model using INLA.
#' @param data a dataframe with columns lon, lat, time, temp and all covariates.
#' Coordinates are in EPSG:4326.
#' @param pred a dataframe with columns lon, lat, time and all covariates.
#' Coordinates are in EPSG:4326.
#' @param polygon a terra::SpatVector with the study area
#' @param ts model start time
#' @param te model end time
#' @import INLA
#' @author Eva Marques
#' @export
inference <- function(data, pred, polygon, ts, te, verbose = FALSE) {
  stopifnot("lon missing" = "lon" %in% colnames(data),
            "lat missing" = "lat" %in% colnames(data),
            "time missing" = "time" %in% colnames(data),
            "temp missing" = "temp" %in% colnames(data))
  # check that polygon is a SpatVector
  stopifnot("polygon must be a SpatVector" = inherits(polygon, "SpatVector"))
  stopifnot("lon missing" = "lon" %in% colnames(pred),
            "lat missing" = "lat" %in% colnames(pred),
            "time missing" = "time" %in% colnames(pred))
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
    dplyr::rename(lon = x, lat = y)
  pred <- terra::vect(pred, geom = c("lon", "lat"), crs = "epsg:4326") |>
    terra::project(polygon) |>
    terra::intersect(polygon) |>
    terra::project("epsg:4326") |>
    as.data.frame(geom = "xy") |>
    dplyr::rename(lon = x, lat = y)
  # spatial mesh
  domain <- polygon |>
    terra::project("epsg:4326") |>
    terra::as.points()
  mesh_s <- inla.mesh.2d(
    loc = cbind(data$lon, data$lat),
    max.edge = 0.1,
    cutoff	= 0.005,
    loc.domain = cbind(terra::geom(domain)[, c("x")],
                       terra::geom(domain)[, c("y")]))
  # temporal mesh
  timedim <- list(time = sort(unique(data$time)),
                  timeidx = 1:length(unique(data$time))) |>
    as.data.frame()
  data <- merge(data, timedim, by = "time")
  pred <- merge(pred, timedim, by = "time")
  mesh_t <- inla.mesh.1d(loc = timedim$timeidx)
  # spde model
  spde <- inla.spde2.pcmatern(mesh_s,
                              # -- p(range < r0) = p_r0
                              prior.range = c(0.5, 0.99),
                              # -- p(sigma > sd0) = p_sd0
                              prior.sigma = c(2, 0.01))
  m <- spde$n.spde
  # projection matrices A and Ap (p for prediction)
  A_st <- inla.spde.make.A(mesh = mesh_s,
                           loc = cbind(data$lon, data$lat),
                           group = data$timeidx)
  Ap_st	<- inla.spde.make.A(mesh = mesh_s,
                            loc = cbind(pred$lon, pred$lat),
                            group = pred$timeidx)
  # space-time index
  s_idx <- inla.spde.make.index("s", n.spde = m, n.group = nrow(timedim))
  # data wrapper
  stk_data <- inla.stack(tag = "data",
                         data = list(y = data$temp_cal),
                         A = list(1, A_st),
                         effects = list(data.frame(
                           int = rep(1, nrow(data)),
                           era5 = data$era5,
                           elev = data$elev,
                           #slope = data$slope,
                           tcc = data$tcc,
                           imp = data$imp,
                           fch = data$fch,
                           bf = data$bf,
                           frac_6_500m = data$frac_6_500m,
                           frac_10_500m = data$frac_10_500m,
                           frac_A_500m = data$frac_A_500m,
                           frac_B_500m = data$frac_B_500m,
                           #frac_C_500m = data$frac_C_500m,
                           frac_D_500m = data$frac_D_500m,
                           frac_E_500m = data$frac_E_500m,
                           frac_F_500m = data$frac_F_500m,
                           frac_G_500m = data$frac_G_500m,
                           timeidx  = data$timeidx),
                           s = s_idx))
  stk_pred <- inla.stack(tag = "pred",
                         data = list(y = rep(NA, nrow(pred))),
                         A = list(1, Ap_st),
                         effects = list(data.frame(
                           int = rep(1, nrow(pred)),
                           era5 = pred$era5,
                           elev = pred$elev,
                           #slope = pred$slope,
                           tcc = pred$tcc,
                           imp = pred$imp,
                           fch = pred$fch,
                           bf = pred$bf,
                           frac_6_500m = pred$frac_6_500m,
                           frac_10_500m = pred$frac_10_500m,
                           frac_A_500m = pred$frac_A_500m,
                           frac_B_500m = pred$frac_B_500m,
                           #frac_C_500m = pred$frac_C_500m,
                           frac_D_500m = pred$frac_D_500m,
                           frac_E_500m = pred$frac_E_500m,
                           frac_F_500m = pred$frac_F_500m,
                           frac_G_500m = pred$frac_G_500m,
                           timeidx  = pred$timeidx),
                           s = s_idx))
  stk_full <- inla.stack(stk_data, stk_pred)
  # model inference
  # rho = correlation
  # -- p(rho>0.5)=0.7 (prior for ar1)
  rhoprior <- list(theta = list(prior = "pccor1", param = c(0.5, 0.7)))
  formula <- y ~ -1 + int + era5 + elev + imp + f(s,
                                                  model = spde,
                                                  group = s.group,
                                                  control.group = list(
                                                    model = "ar1",
                                                    hyper = rhoprior)
  )
  # -- to faster the algo
  cinla <- list(strategy = "adaptive", int.strategy = "eb")
  cres <- list(return.marginals.predictor = F, return.marginals.random = F)
  mod <- inla(formula	= formula,
              data = inla.stack.data(stk_full),
              family = c("gaussian"),
              #control.fixed	= list(
                #mean = list(
                  #int = ,
                  #era5 = ,
                  #default = 0),
                #prec = list(
                  #int = ,
                  #era5 = ,
                  #default = 0.001)),
              control.predictor = list(compute = TRUE,
                                       A = inla.stack.A(stk_full)),
              #control.results = cres,
              control.inla = cinla,
              verbose = verbose)
  index <- inla.stack.index(stk_full, tag = "pred")$data
  pred$pred_mean <- mod$summary.fitted.values[index, "mean"]
  pred$pred_ll <- mod$summary.fitted.values[index, "0.025quant"]
  pred$pred_ul <- mod$summary.fitted.values[index, "0.975quant"]
  pred$pred_sd <- mod$summary.fitted.values[index, "sd"]
  return(list(pred = pred, mod = mod))
}
