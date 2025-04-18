% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_covariates.R
\name{extract_imperviousness}
\alias{extract_imperviousness}
\title{Extracts imperviousness at spatial points}
\usage{
extract_imperviousness(pts, imp, buf_radius = 500)
}
\arguments{
\item{pts}{a SpatVector or sf (should not be spatiotemporal)}

\item{imp}{a SpatRaster with imperviousness data.}

\item{buf_radius}{a numeric with the radius of the buffer around each point.}
}
\description{
Extracts imperviousness at spatial points
}
\author{
Eva Marques
}
