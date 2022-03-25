#' @name catchment
#' @rdname catchment
#' @title Catchment
#' Compute catchment area or catchment spatial objects from delineated streams
#' @param x A [raster::stack], such as one created by [delineate()].
#' @param type The scale at which to compute catchments, see 'details'
#' @param y Matrix, 2-columns, giving coordinates at which to compute the catchments
#' @param area boolean, if `TRUE` returns the catchment area, if `FALSE` returns a raster
#' @param Tp Optional topology, used if type != "points", will be computed if not provided
#' @details The type parameter controls how many catchments/catchment areas are computed 
#'		(and how long the function will take).
#'
#'    * `'outlet'`: Compute only for the outlet of the entire network
#'    * `'reach'`: Compute one catchment area per reach, computed at the bottom of the reach
#'    * `'points'`: Compute catchment for user-specified points, given by the `y` parameter
#'    * `'pixel'`: Compute catchment area for each pixel; this can take a very long time
#' @return
#' @examples
#' \donttest{
#'     data(kamp_dem)
#'     x = delineate(kamp_dem)
#'     catchment(x)
#' }
#' @export
catchment = function(x, type=c("outlet", "reach",  "points", "pixel"), y, area = TRUE, Tp) {
	type = match.arg(type)

	if(missing(Tp) && type != "points")
		Tp = pixel_topology(x)

	# if unix, use multiple cores if mc.cores is specified
	cores = ifelse(.Platform$OS.type == "unix", getOption("mc.cores", 1L), 1L)

	## do nothing if type is points; user must specify y themselves
	if(type == "outlet") {
		y = stream_coordinates(x)[.outlet(Tp),,drop=FALSE]
	} else if(type == "reach") {
		## find the bottom of each reach
		ids = raster::unique(x[['stream']])
		pids = parallel::mclapply(ids, extract_reach, x = x, Tp = Tp, 
			mc.cores = cores)
		Tp_r = lapply(pids, function(x) Tp[x,x,drop=FALSE])
		out_r = lapply(Tp_r, .outlet)
		out_r = mapply(function(x,y) x[y], pids, out_r)
		y = stream_coordinates(x)[out_r,,drop=FALSE]
	} else if(type == "pixel") {
		y = stream_coordinates(x)
	} else {
		if(!is.matrix(y))
			y = matrix(y, ncol=2)
	}

	if(is.matrix(y))
		y = as.list(as.data.frame(t(y)))

	drainage = "drainage"
	catch_names = paste("catchment", seq_along(y), sep='_')
	.start_grass(x[['drainage']], drainage)

	if(area && .Platform$OS.type != "windows") {
		res = mapply(.catchment_area, y = y, out_name = catch_names, 
				MoreArgs = list(drain_name = drainage),
				USE.NAMES = FALSE, SIMPLIFY = TRUE)
	} else {
		res = mapply(.catchment, y = y, out_name = catch_names, 
			MoreArgs = list(drain_name = drainage), SIMPLIFY = FALSE)
		if(area) {
			res = sapply(res, function(x) 
				sum(raster::values(x), na.rm = TRUE) * prod(raster::res(x)))
		} else if(length(res) == 1) {
			res = res[[1]]
			names(res) = "catchment"
		} else {
			res = raster::stack(res)
		}
	}
	.clean_grass()
	res
}

#' @name catchment_area
#' @rdname catchment_area
#' @title Catchment worker functions
#' Worker functions for catchments and catchment areas on single pixels
#' @param y Coordinates, where to compute the catchment
#' @param drain_name Name of the (already present) drainage direction raster
#' @param out_name Name of the output raster in grass
#' @keywords internal
#' @details
#' * `.do_catchment` builds the catchment raster in GRASS
#' * `.catchment_area` runs `.do_catchment` and then comptues and returns the area as a number
#' * `.catchment` runs `.do_catchment` and then extracts and returns the raster from GRASS
.catchment_area = function(y, out_name, drain_name) {
	.do_catchment(y, out_name, drain_name)
	res = capture.output(rgrass7::execGRASS("r.stats", flags=c("overwrite", "quiet", "a"), 
		input = out_name))
	.clean_grass(raster = out_name, vector = NA)
	pat = "^1 ([0-9//.]+)"
	res = res[grepl(pat, res)]
	as.numeric(sub(pat, "\\1", res))
}

#' @rdname catchment_area
#' @keywords internal
.catchment = function(y, out_name, drain_name) {
	.do_catchment(y, out_name, drain_name)
	res = .read_rasters(out_name)
	.clean_grass(raster = out_name, vector = NA)
	res[res == 0] = NA
	res
}

#' @rdname catchment_area
#' @keywords internal
.do_catchment = function(y, out_name, drain_name) {
	rgrass7::execGRASS("r.water.outlet", flags=c("overwrite", "quiet"), input=drain_name, 
		output = out_name, coordinates = y)
	if(!(out_name %in% ws_env$rasters))
		ws_env$rasters = c(ws_env$rasters, out_name)
}

