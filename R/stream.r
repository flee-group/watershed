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
	
	## renumber reaches to go from 1:nreaches
	res[['stream']] = raster::match(res[['stream']], 
					raster::unique(res[['stream']]))

	## clean up files
	.clean_grass()

	res
}

#' @name vectorise_stream
#' @rdname vectorise_stream
#' @title Vectorize a stream layer
#' Produces a vector layer (in `sf` format) from a raster stream map as created by [delineate()].
#' @param x A [raster] stream map, such as one created by [delineate()].
#' @return An `sf` stream layer
#' @examples
#' \donttest{
#'     data(kamp_dem)
#'     x = delineate(kamp_dem)
#'     vect = vectorise_stream(x)
#' }
#' @export
vectorise_stream = function(x) {
	inpname = "stream"
	outname = "stream_thin"
	vectname = "stream_vect"
	
	.start_grass(x, inpname)

	flags = c("overwrite", "quiet")

	rgrass7::execGRASS("r.thin", flags=flags, input=inpname, output = outname)
	ws_env$rasters = c(ws_env$rasters, outname)
	
	rgrass7::execGRASS("r.to.vect", flags=flags, input=outname, output = vectname,
					   type="line", column="reach_id")
	ws_env$vectors = c(ws_env$vectors, vectname)
	vect = .read_vector(vectname)
	
	## todo: v.clean
	.clean_grass()
	vect	
}

#' Resize reaches from a delineated stream
#' @param x A [raster] stream map, such as one created by [delineate()].
#' @param len The target length of resized reaches (in map units)
#' @param min_len How small is the smallest acceptable reach?
#' @param trim Logical; remove headwater reaches smaller than min_length?
#' @param Tp Optional; a pixel topology; will be computed if missing
#' @param Tr Optional; a reach topology; will be computed if missing
#' @return A raster similar to `x` with reaches resized
#' @examples
#' \donttest{
#'     data(kamp_dem)
#'     x = delineate(kamp_dem)
#'     x[['stream']] = resize_reaches(x[['stream']], 1000, 600)
#' }
#' @export
resize_reaches = function(x, len, min_len = 0.5 * len, trim = TRUE, Tp, Tr) {
	
}

# split a single reach
# returns a list of vectors
# each vector is a new reach
# pixels in the old reach that are not in the new reach should be set to NA by the calling function
.resize_r = function(Tp) {
	
	## only trim headwater reaches
	trim = trim && .is_headwater(Tr, i)
	
	## extract the IDs of the reach of interest
	## then find the reach outlet
	pids = which(raster::values(stream) == i)
	Tp_r = Tp[pids, pids, drop=FALSE]
	bottom = pids[.outlet(Tp_r)]
	top = pids[.headwater(Tp_r)]
	
	pix_lens = Matrix::rowSums(Tp)
	
	## number of reaches
	rlen = sum(pix_lens[pids])
	nr = ceiling(rlen/len)
	
	## if not trimming, decide on new reach length
	## try the computed number of reaches +/- 1
	## pick the one that gets us closest to the target length
	if(!trim) {
		cand = rlen / (nr + -1:1)
		ldiff = abs(len - cand)
		len = cand[which.min(ldiff)]
		nr = nr + (-1:1)[which.min(ldiff)]
	}
	
	## biggest possible reach, in terms of the number of pixels
	maxpix = ceiling(len / min(Tp@x))
	
	
	## traverse, accumulating each reach as we go upstream
	## start with memory pre-allocated, will drop excess 0s later
	reaches = lapply(rep(maxpix, nr), numeric) 
	cpix = bottom
	r = 1
	while(cpix != top) {
		p = 1
		rl = 0
		while(rl < len) {
			rl = rl + pix_lens[cpix]
			reaches[[r]][p] = cpix
			p = p+1
			cpix = .upstream(Tp, cpix)
			if(cpix == top)
				break()
		}
		r = r+1
	}
	
	## drop 0s
	reaches = lapply(reaches, function(r) r[r>0])
	if(length(reaches[[nr]]) == 0) {
		reaches[[nr]] = NULL
		nr = nr - 1
	}
	rlens = lapply(reaches, function(x) sum(pix_lens[x]))
	
	## trim the top if it is a headwater
	if(trim && rlens[nr] < min_len)
		reaches[[nr]] = NULL
	
	reaches
}

