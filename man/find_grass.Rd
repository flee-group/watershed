% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grass.r
\name{find_grass}
\alias{find_grass}
\title{Try to locate a user's GRASS GIS installation}
\usage{
find_grass()
}
\value{
The path to the user's grass location, or NULL if not found
}
\description{
Locates a grass installation in common locations on Mac, Windows, and Linux. This is normally
run automatically when the package is loaded. If multiple installations are present, then
we preferably take whichever version is called "grass" and is in the users PATH.
If that doesn't work, we look for grass80, grass78, grass76, and grass74 in the path, in that
order.
}
\details{
In some (many?) cases, this function will fail to find a grass installation, or users may wish
to specify a different version than what is detected automatically. In these cases, it is
possible to manually specify the grass location using \code{options(gisBase = "path/to/grass")}.
}
