#' Start grass in a simple way
#'
#' @param layer A [SpatRaster] or [RasterLayer] object to write to the grass session
#' @param layer_name Name of the layer in the grass environment
#' @param gisBase character; the location of the GRASS installation
#' @param home Location to write GRASS settings
#' @param gisDbase Location to write GRASS GIS datasets
#' @param location Grass location name
#' @param mapset Grass mapset name
#' @keywords internal
.start_grass = function(layer, layer_name, gisBase, home = tempdir(), gisDbase = home,
						location = 'watershed', mapset = 'PERMANENT') {
	if(missing(gisBase))
		gisBase = getOption("gisBase")
	rgrass::initGRASS(gisBase, home=home, SG = layer, gisDbase = gisDbase, location = location,
					   mapset = mapset, override = TRUE)
	.add_raster(layer, layer_name)
}

#' Add a raster to grass
#' @param x A [terra::SpatRaster]
#' @param name The name of the layer in grass
#' @param flags Any flags to pass to [rgrass::write_RAST]
#' @param overwrite Should the file be overwritten if it exists
#' @keywords internal
.add_raster = function(x, name, flags, overwrite = TRUE) {
	if(! "SpatRaster" %in% class(x))
		stop("x must be of class terra::SpatRaster")
	if(missing(flags))
		flags = list()
	if(overwrite)
		flags = c(flags, "overwrite")
	flags = c(flags, 'o') # rgrass keeps dropping datum info, resulting in clashes with grass location proj
	rgrass::write_RAST(x, name, flags = unlist(flags), verbose = FALSE)
	ws_env$rasters = c(ws_env$rasters, name)
}


#' @name read_rasters
#' @rdname read_rasters
#' @title Read files from grass
#' Read and format raster and vector layers from a grass session
#' @param layers A vector of names of rasters to read
#' @param file The file name to save the raster
#' @param x A single vector layer name to read
#' @keywords internal
.read_rasters = function(layers, file) {
	ras = sapply(layers, function(l) rgrass::read_RAST(l))
		# suppressWarnings(rgrass::read_RAST(l)) # proj warnings suppressed
	if(length(layers) > 1) {
		ras = terra::rast(ras)
		names(ras) = layers
	} else {
		ras = ras[[1]]
	}
	if(!missing(file) && !is.na(file))
		ras = terra::writeRaster(ras, file)
	ras
}

#' @rdname read_rasters
#' @keywords internal
.read_vector = function(x) {
	v <- rgrass::read_VECT(x)
	# d <- capture.output(v <- rgrass::read_VECT(x)) ## this function is quite noisy
	sf::st_as_sf(v)
}

#' Clean up grass files
#' @param raster Raster layers to remove, if missing removes all, if NA removes none
#' @param vector Vector layers to remove, if missing removes all, if NA removes none
#' @details The default mode is to clean everything, removing all layers from the grass session
#' @keywords internal
.clean_grass = function(raster, vector) {
	if(missing(raster))
		raster = ws_env$rasters
	if(missing(vector))
		vector = ws_env$vectors

	# sometimes layers get added twice
	raster = unique(raster)
	vector = unique(vector)

	if(!is.na(raster) && length(raster) > 0) {
		sapply(raster, function(r) rgrass::execGRASS("g.remove", flags = c("f", "quiet"), 
			type="raster", name=r))
		if(length(raster) == length(ws_env$rasters)) {
			ws_env$rasters = list()
		} else {
			ws_env$rasters = ws_env$rasters[-which(ws_env$rasters %in% raster)]
		}
	}

	if(!is.na(vector) && length(vector) > 0) {
		sapply(vector, function(v) rgrass::execGRASS("g.remove", flags = c("f", "quiet"), 
			type="vector", name=v))
		if(length(vector) == length(ws_env$vectors)) {
			ws_env$vectors = list()
		} else {
			ws_env$vectors = ws_env$vectors[-which(ws_env$vectors %in% vector)]
		}
	}
}



#' Try to locate a user's GRASS GIS installation
#'
#' Locates a grass installation in common locations on Mac, Windows, and Linux. This is normally
#' run automatically when the package is loaded. If multiple installations are present, then
#' we preferably take whichever version is called "grass" and is in the users PATH.
#' If that doesn't work, we look for grass80, grass78, grass76, and grass74 in the path, in that
#' order.
#' 
#' In some (many?) cases, this function will fail to find a grass installation, or users may wish
#' to specify a different version than what is detected automatically. In these cases, it is 
#' possible to manually specify the grass location using `options(gisBase = "path/to/grass")`.
#'
#' @return The path to the user's grass location, or NULL if not found
#' @export
find_grass = function() {
	gisBase = NA

	if(.Platform$OS.type == "unix") {
		grass_names = c("grass", "grass80", "grass78", "grass76", "grass74")
		paths = c("", "/usr/local/bin/", "/opt/local/bin/")

		gb = unlist(lapply(paths, function(p) {
			lapply(grass_names, function(gr) {
				tryCatch(system2(paste0(p, gr), args = c("--config", "path"), 
					stdout = TRUE, stderr = TRUE), error = function(e) NA)
			})
		}))
		if(!all(is.na(gb)))
			gisBase = gb[!is.na(gb)][1]
	} else {
		## windows
		appPath = c(file.path("C:", "OSGEO4W64", "apps", "grass"),
			file.path("C:", "Program Files", "GRASS GIS 8.0"),
			file.path("C:", "Program Files", "GRASS GIS 7.8"),
			file.path("C:", "Program Files", "GRASS GIS 7.6"))

		foundGrass = sapply(appPath, file.exists)
		if(any(foundGrass))
			gisBase = appPath[foundGrass][1]
	}
	gisBase
}

