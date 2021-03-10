.onLoad = function(libname, pkgname) {
	Sys.setenv("GRASS_VERBOSE"=0)
	rgrass7::use_sp()
	
	## set of an environment to hold package globals
	assign("ws_env", new.env(), envir = parent.env(environment()))
	ws_env$rasters = list()
	ws_env$vectors = list()

	## Try to figure out where GRASS is located, warn the user if not found
	gisBase = find_grass()
	if(is.null(gisBase) | is.na(gisBase)) {
		warning("Could not automatically detect GRASS, see ?find_grass")
	} else {
		options(gisBase = gisBase)
	}
}

