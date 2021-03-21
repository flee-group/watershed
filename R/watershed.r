#' watershed: Tools for Watershed Delineation
#'
#' This is a simplified set of tools for deriving a river network from digital elevation data.
#' The package relies on GRASS GIS (v. 7.4 or 7.6) to do the heavy lifting. An installation of
#' GRASS and the rgrass7 package is required for watershed to function. Note that this is meant
#' to be a simplified workflow for watershed delineation using only R code; for more options
#' users can use rgrass7 to access GRASS GIS functions directly.
#'
#' @section Key functions:
#'
#' * [foo()] A function for doing foo
#'
#' @section Datasets:
#' * [kamp_dem] An example digital elevation model for the Kamp river in Austria
#' * [kamp_q] Measured discharge in the Kamp river
#'
#' @docType package
#' @name watershed_package
NULL


#' Digital elevation model for the Kamp River catchment in Austria
#'
#' @format A [raster::raster()]
#' \describe{
#'   \item{value}{Elevation of each pixel, in m}
#' }
"kamp_dem"


#' Measured discharge in the Kamp river
#'
#' @format A simple features (`sf`) point dataset
#' \describe{
#'   \item{discharge}{Discharge in m^3/s}
#' }
"kamp_q"
