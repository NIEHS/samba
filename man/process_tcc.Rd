% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_covariates.R
\name{process_tcc}
\alias{process_tcc}
\title{Process tree canopy cover raster}
\usage{
process_tcc(tcc, polygon)
}
\arguments{
\item{tcc}{RasterLayer: tree canopy cover raster}

\item{polygon}{SpatVector polygon to crop raster}
}
\value{
RasterLayer: processed tree canopy cover raster
}
\description{
Process tree canopy cover raster: select extent,
and set NA values
}
