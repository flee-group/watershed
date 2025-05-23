.onLoad = function(libname, pkgname) {
	Sys.setenv("GRASS_VERBOSE"=0)

	if(is.null(getOption("mc.cores")) && !grepl("[Ww]indows", Sys.info()['sysname'])) {
		message("For faster performance of some functions on a machine with lots of RAM, you can set")
		message("options(mc.cores = parallel::detectCores())")
	}

	## set of an environment to hold package globals
	assign("ws_env", new.env(), envir = parent.env(environment()))
	ws_env$rasters = list()
	ws_env$vectors = list()

	## Try to figure out where GRASS is located, warn the user if not found
	gisBase = find_grass()
	if(is.null(gisBase) | is.na(gisBase)) {
		warning("Could not automatically detect GRASS, see ?find_grass")
	} else {
		message("Using grass: ", gisBase)
		message("see ?find_grass to override this default")
		options(gisBase = gisBase)
	}
}

