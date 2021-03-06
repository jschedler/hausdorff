% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hausMat.R
\name{hausMat}
\alias{hausMat}
\title{Creating a Hausdorff spatial weight matrix}
\usage{
hausMat(shp, f1, f2 = f1, fileout = FALSE, filename = NULL)
}
\arguments{
\item{shp}{a SpatialPolygons object.}

\item{f1}{The percentage (as a decimal) of region i to retain when calculating the directional Hausdorff distance from region i to region j.}

\item{f2}{The percentage (as a decimal) of region j to retain when calculating the directional Hausdorff distance from region j to i. Defaults to the value of f1. Note that specifying a different value will result in a non-symmetric matrix.}

\item{fileout}{Should the resulting weight matrix be written to file? Defaults to FALSE}

\item{filename}{If \code{fileout} is TRUE, the name for the file to be outputted.}
}
\value{
an nxn matrix of requested distances.
}
\description{
This function takes a SpatialPolygons object
}
