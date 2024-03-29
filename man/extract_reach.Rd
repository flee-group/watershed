% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topology.r
\name{extract_reach}
\alias{extract_reach}
\title{Find pixel IDs matching reach i}
\usage{
extract_reach(i, x, Tp, sorted = FALSE)
}
\arguments{
\item{i}{Reach number to extract}

\item{x}{Raster stream stack}

\item{Tp}{optional Topology, only needed for sorting}

\item{sorted}{Logical, should the ids be returned sorted from upstream to down?}
}
\value{
A vector of pixel ids
#' @examples
\donttest{
    data(kamp_dem)
    kamp = delineate(kamp_dem, outlet = NA)
    Tp = pixel_topology(kamp)
    extract_reach(5, kamp$stream, Tp)
}
}
\description{
Find pixel IDs matching reach i
}
