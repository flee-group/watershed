#' Delineate streams from an elevation model
#' @param dem A [raster::raster]; a digital elevation model.
#' @param threshold The minimum size of a delineated catchment; units are in dem_units^2 (e.g., m^2); default 10 km^2.
#' @param pretty Should flat areas be made prettier? Corresponds to r.watershed -b flag
#' @param file The file name of the raster to be returned, see `details`.
#' @param outlet The location of the outlet of the target stream, see `details`.
#' @details This is a wrapper for [r.watershed](https://grass.osgeo.org/grass76/manuals/r.watershed.html).
#'
#' The threshold parameter controls the level of detail in the delineated streams. Higher thresholds result in faster
#' computation and fewer streams in the output. For finer control, see [extract_stream()].
#'
#' If the `outlet` parameter is missing (the default), then all streams in the DEM will be returned. If `outlet` is `NA`,
#' then the largest stream in the area will be set as the outlet, and only streams in that watershed will be used.
#' If a smaller stream is the focus, then outlet can be a pair of x-y coordinates which will determine the farthest
#' downstream point to use.
#'
#' Streams can be converted to a vector file, see [stream_vector()].
#'
#' It is recommended to specify the `file` parameter (including the extension to specify
#' file format; e.g., .tif, .grd). If not specified, a temp file will be created and will be
#' lost at the end of the R session.
#' @return A [raster::stack()], containing the drainage map, the flow accumulation map, and the delineated streams.
#' @export
delineate = function(dem, threshold = 1e6, pretty = FALSE, file, outlet) {
	cell_area = prod(raster::res(dem))
	threshold_cell = floor(threshold/cell_area)

	if(threshold_cell / raster::ncell(dem) < 0.0001)
		warning("Small threshold; excessive computation time and memory usage are possible if threshold not increased")

	flags = c("overwrite", "quiet")
	if(pretty)
		flags = c(flags, "b")

	elevation = "dem"
	accum = "accum"
	drainage = "drainage"
	stream= "stream"

	## launch grass and copy data
	.start_grass(dem, elevation)

	## perform computation
	rgrass7::execGRASS("r.watershed", flags=flags, elevation=elevation, threshold = threshold_cell,
					   accumulation = accum, drainage = drainage, stream = stream)
	# make sure to add the names of created rasters to the list of layers
	ws_env$rasters = c(ws_env$rasters, accum, drainage, stream)

	## gather data
	res = .read_rasters(c(accum, drainage, stream), file)

	## Clip to a single network if desired
	if(!missing(outlet)) {
		if(length(outlet) == 1 && is.na(outlet))
			outlet = raster::coordinates(res)[which.max(abs(raster::values(res$accum))),]

		rgrass7::execGRASS("r.water.outlet", flags=c("overwrite", "quiet"), input=drainage, output = "catchment",
						   coordinates = outlet)
		ws_env$rasters = c(ws_env$rasters, "catchment")
		catchment = .read_rasters("catchment")
		catchment[catchment == 0] = NA
		stream_clip = raster::mask(res$stream, catchment)
		res = raster::dropLayer(res, which(names(res) == stream))
		res = raster::addLayer(res, stream = stream_clip, catchment = catchment)
		if(!missing(file))
			res = raster::writeRaster(res, filname = file)
	}

	## clean up files
	.clean_grass()

	res
}
