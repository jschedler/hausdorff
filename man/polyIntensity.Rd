% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polyIntensity.R
\name{polyIntensity}
\alias{polyIntensity}
\title{Generate a Pixel intensity over a union of polygons}
\usage{
polyIntensity(shp, n.sim, beta = 0, surf = NULL, b1 = NULL,
  b2 = NULL, scale.factor = 1)
}
\arguments{
\item{shp}{a SpatialPolygons object.}

\item{n.sim}{the approximate number of points to sample to determine the dimensions of the pixel image}

\item{beta}{the ``true" value of the coefficient for the covariate which determines the intensity}

\item{surf}{object of class im. optional continuous covariate to include in model for intensity.}
}
\value{

}
\description{
This function takes a SpatialPolygons object
}
