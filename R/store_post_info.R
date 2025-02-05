store_post_info <- function(mod, info) {
  stopifnot("info has more than 1 row" = nrow(info) == 1)

  # fixed effects
  list_marg <- mod$marginals.fixed
  marginals <- data.frame(do.call(rbind, list_marg))
  marginals$fixed.effects <- rep(names(list_marg),
                                 times = sapply(list_marg, nrow))
  marg_m <- unlist(lapply(X = list_marg,
                          FUN = INLA::inla.emarginal, # computes the expectation
                          fun = function(x) x))
  marg_mm <- unlist(lapply(X = list_marg,
                           FUN = INLA::inla.emarginal,
                           fun = function(x) x^2))
  # -- var[X] = E[X^2] − E[X]^2 = mm - m^2
  marg_sd <- sqrt(marg_mm - marg_m^2)
  marg_m_names <- unlist(lapply(names(list_marg), paste0, "_mean"))
  marg_sd_names <- unlist(lapply(names(list_marg), paste0, "_sd"))
  mod_param <- data.frame(t(c(marg_m, marg_sd)))
  colnames(mod_param) <- c(marg_m_names, marg_sd_names)
  info <- cbind(info, mod_param)

  # hyperparameters
  list_marg <- mod$marginals.hyperpar
  names(list_marg) <- c("prec_gaussian_obs", "range_gaussian_obs", "stdev_s", "grouprho_s")
  marginals <- data.frame(do.call(rbind, list_marg))
  marginals$fixed.effects <- rep(names(list_marg),
                                 times = sapply(list_marg, nrow))
  marg_m <- unlist(lapply(X = list_marg,
                          FUN = INLA::inla.emarginal, # computes the expectation
                          fun = function(x) x))
  marg_mm <- unlist(lapply(X = list_marg,
                           FUN = INLA::inla.emarginal,
                           fun = function(x) x^2))
  # -- var[X] = E[X^2] − E[X]^2 = mm - m^2
  marg_sd <- sqrt(marg_mm - marg_m^2)
  marg_m_names <- unlist(lapply(names(list_marg), paste0, "_mean"))
  marg_sd_names <- unlist(lapply(names(list_marg), paste0, "_sd"))
  mod_param <- data.frame(t(c(marg_m, marg_sd)))
  colnames(mod_param) <- c(marg_m_names, marg_sd_names)
  info <- cbind(info, mod_param)

  return(info)
}
