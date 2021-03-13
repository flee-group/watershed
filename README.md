# watershed: An R-package for delineating watersheds using a digital elevation model

This is a simplified set of tools for deriving a river network from digital elevation data. 
The package relies on GRASS GIS (v7.8, 7.6, or 7.4) to do the heavy lifting, thus an installation of `GRASS GIS` and the `rgrass7` package is required for `watershed` to function. 
Note that this is meant to be a simplified workflow for watershed delineation using only R code; for more options users can use rgrass7 to access GRASS GIS functions directly.

## Installation

Before installing this package, you should install [GRASS GIS](https://grass.osgeo.org). The latest tested version is 7.8; versions 7.6 and 7.4 also work, earlier or later versions are unknown (but please send us feedback if you try this!).

**Windows**: The preferred distribution is the OSGeo4W distribution; `watershed` should be able to auto-detect your grass installation if you install this version.

**Mac**: `watershed` can auto-detect [grass binary](http://grassmac.wikidot.com) installations (which are usually installed in `/Applications/`). Command-line installations may also work; try following the linux instructions.

**Linux/Unix**: Install grass however you like. For auto-detection to work, make sure the grass binary (`grass74`, `grass76`, `grass78`) is in your `$PATH` and visible from within R; try `system2('grass76', args = c("--config", "path"))` to see if this is working, it should output the location of your grass installation. If you want a different version of grass, you can alias `grass78` to whatever binary you like, or see **Changing the default grass* below.

## Installing `watershed`.

The `watershed` package can be easily installed using the `remotes` package within R as follows:

    remotes::install_github("flee-group/watershed", dependencies = TRUE)

## Changing the default grass

If auto-detection fails, or if you want to use a grass version other than the default, you can do so easily:

    options(gisBase = "/path/to/grass_directory")
    
For example, if the grass executable is located at `/usr/local/bin/grass/grass79/grass79`, then you would use `/usr/local/bin/grass/grass79` as the gisBase.

## Testing

We recommend following the first two steps in the [vignette](https://flee-group.github.io/watershed/watershed) to see if the package is working.

Please contact us, or raise an issue on github if you encounter any problems!
