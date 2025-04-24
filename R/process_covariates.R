#' Process elevation raster
#' @description Process elevation raster: select extent, set NA values and
#' calculate slope, aspect, flow direction
#' @param elev RasterLayer: elevation raster
#' @param polygon SpatVector polygon to crop raster
#' @return RasterLayer: processed elevation raster
#' @importFrom terra crop terrain
#' @export
process_elev <- function(elev, polygon) {
  # check if crs are the same and update polygon's crs if not
  if (!terra::same.crs(elev, polygon)) {
    polygon <- terra::project(polygon, elev)
  }
  elev <- terra::crop(elev, polygon, mask = TRUE)
  names(elev) <- "dem"
  elev$slope <- terra::terrain(elev$dem, "slope")
  # aspect is in degrees, clockwise from North
  # if no slope: 90
  elev$aspect <- terra::terrain(elev$dem, "aspect")
  # Roughness is the difference between the maximum and the
  # minimum value of a cell and its 8 surrounding cells. It is a discrete value
  # so it is not added here.
  elev$flowdir <- terra::terrain(elev$dem, "flowdir")
  elev
}

#' Process imperviousness raster
#' @description Process imperviousness raster: select extent,
#' and set NA values
#' @param imp RasterLayer: imperviousness raster
#' @param polygon SpatVector polygon to crop raster
#' @return RasterLayer: processed imperviousness raster
#' @importFrom terra crop
#' @export
process_imp <- function(imp, polygon) {
  # check if crs are the same and update polygon's crs if not
  if (!terra::same.crs(imp, polygon)) {
    polygon <- terra::project(polygon, imp)
  }
  imp <- terra::crop(imp, polygon, mask = TRUE)
  imp[imp$Layer_1 == 127] <- NA
  imp
}

#' Process building footprint raster
#' @description Process building footprint raster: select extent,
#' and set NA values
#' @param bf RasterLayer: building footprint raster
#' @param polygon SpatVector polygon to crop raster
#' @return RasterLayer: processed building footprint raster
#' @importFrom terra crop
#' @export
process_bf <- function(bf, polygon) {
  # check if crs are the same and update polygon's crs if not
  if (!terra::same.crs(bf, polygon)) {
    polygon <- terra::project(polygon, bf)
  }
  bf <- terra::crop(bf, polygon, mask = TRUE)
  bf[bf$Layer_1 > 900] <- NA
  bf
}

#' Process tree canopy cover raster
#' @description Process tree canopy cover raster: select extent,
#' and set NA values
#' @param tcc RasterLayer: tree canopy cover raster
#' @param polygon SpatVector polygon to crop raster
#' @return RasterLayer: processed tree canopy cover raster
#' @importFrom terra crop
#' @export
process_tcc <- function(tcc, polygon) {
  # check if crs are the same and update polygon's crs if not
  if (!terra::same.crs(tcc, polygon)) {
    polygon <- terra::project(polygon, tcc)
  }
  tcc <- terra::crop(tcc, polygon, mask = TRUE)
  tcc[tcc$Layer_1 == 0] <- NA
  tcc[tcc$Layer_1 >  100] <- NA
  tcc
}

#' Process local climate zone raster
#' @description Process local climate zone raster: select extent,
#' and set NA values
#' @param lcz RasterLayer: local climate zone raster
#' @param polygon SpatVector polygon to crop raster
#' @return RasterLayer: processed local climate zone raster
#' @importFrom terra crop
#' @export
process_lcz <- function(lcz, polygon) {
  # check if crs are the same and update polygon's crs if not
  if (!terra::same.crs(lcz, polygon)) {
    polygon <- terra::project(polygon, lcz)
  }
  lcz <- terra::crop(lcz, polygon, mask = TRUE)
  lcz[lcz$lcz_conus_demuzere_2020 == 0] <- NA
  lcz
}

#' Process forest canopy height raster
#' @description Process forest canopy height raster: select extent,
#' and set NA values
#' @param fch RasterLayer: forest canopy height raster
#' @param polygon SpatVector polygon to crop raster
#' @return RasterLayer: processed forest canopy height raster
#' @importFrom terra crop
#' @export
process_fch <- function(fch, polygon) {
  # check if crs are the same and update polygon's crs if not
  if (!terra::same.crs(fch, polygon)) {
    polygon <- terra::project(polygon, fch)
  }
  fch <- terra::crop(fch, polygon, mask = TRUE)
  # fch is in meters, values from 0-60m
  # fch 101: water
  # fch 102: snow/ice
  # fch 103: no data
  fch[fch$Layer_1 > 100] <- NA
  fch
}
