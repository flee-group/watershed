#' Delineate streams from an elevation model
#' @param dem A [terra::SpatRaster]; a digital elevation model.
#' @param threshold The minimum size of a delineated catchment; units are in dem_units^2 
#'		(e.g., m^2); default 10 km^2.
#' @param pretty Should flat areas be made prettier? Corresponds to r.watershed -b flag
#' @param file The file name of the raster to be returned, see `details`.
#' @param outlet The location of the outlet of the target stream, see `details`.
#' @param reach_len Optional; if provided, reaches will be resized to meet this target length
#' @param ... If `reach_len` is specified, additional parameters to be passed to [resize_reaches()]
#' @details This is a wrapper for 
#'		[r.watershed](https://grass.osgeo.org/grass76/manuals/r.watershed.html).
#'
#' The threshold parameter controls the level of detail in the delineated streams. Higher 
#'		thresholds result in faster computation and fewer streams in the output. For finer control, 
#'		see [extract_stream()].
#'
#' If `outlet` is `NA` (the default), then the largest stream in the area will be set as the outlet,
#' 		and only streams in that watershed will be used. If a smaller stream is the focus, then 
#'		outlet can be a pair of x-y coordinates which will determine the farthest downstream point 
#'		to use.
#'
#' Streams can be converted to a vector file, see [stream_vector()].
#'
#' It is recommended to specify the `file` parameter (including the extension to specify
#' file format; e.g., .tif, .grd). If not specified, a temp file will be created and will be
#' lost at the end of the R session.
#' @return A [terra::SpatRaster], containing the drainage map, the flow accumulation map, and the 
#"		delineated streams.
#' @examples
#' \donttest{
#'     data(kamp_dem)
#'     x = delineate(kamp_dem)
#' }
#' @export
delineate = function(dem, threshold = 1e6, pretty = FALSE, file, outlet = NA, reach_len, ...) {
	cell_area = prod(terra::res(dem))
	threshold_cell = floor(threshold/cell_area)

	if(threshold_cell < 1)
		stop("Threshold too small, resulting in zero pixels chosen (threshold must be >", 
			" prod(terra::res(dem)")

	if(threshold_cell / terra::ncell(dem) < 0.0001)
		warning("Small threshold; excessive computation time and memory usage are possible if ",
			"threshold not increased")

	flags = c("overwrite", "quiet")
	if(pretty)
		flags = c(flags, "b")

	elevation = "dem"
	accum = "accum"
	drainage = "drainage"
	stream = "stream"

	## launch grass and copy data
	.start_grass(dem, elevation)

	## perform computation
	rgrass::execGRASS("r.watershed", flags=flags, elevation=elevation, 
		threshold = threshold_cell, accumulation = accum, drainage = drainage, stream = stream)

	# make sure to add the names of created rasters to the list of layers
	ws_env$rasters = c(ws_env$rasters, accum, drainage, stream)

	## gather data
	res = .read_rasters(c(accum, drainage, stream))

	## Clip to a single network
	if(length(outlet) == 1 && is.na(outlet)) {
		res_df = as.data.frame(res, xy=TRUE)
		res_df = res_df[complete.cases(res_df) & accum > 0,]
		outlet = as.matrix(res_df[which.max(res_df$accum), c('x', 'y')])
	}
	catch = suppressWarnings(catchment(res, type = "points", y = outlet, area = FALSE))

	catch = terra::trim(catch)
	res = terra::crop(res, catch)
	stream_clip = terra::mask(res$stream, catch)
	id = stream_clip
	i = which(!is.na(terra::values(id)))
	id[i] = 1:length(i)
	
	# renumber streams to go from 1:nreaches
	vv = terra::values(stream_clip)[i]
	vv = as.integer(as.factor(vv))
	stream_clip[i] = vv
	
	res = res[[-(which(names(res) == stream))]]
	res = c(res, stream_clip, catch, id)
	names(res) = c("accum", "drainage", "stream", "catchment", "id")
	storage.mode(res[['stream']][]) = 'integer'
	storage.mode(res[['id']][]) = 'integer'


	if(!missing(reach_len)) {
		Tp = pixel_topology(res)
		res[['stream']] = resize_reaches(res, Tp, len = reach_len, ...)
		# some pixels are trimmed, make sure to trim them from id as well
		j = which(is.na(terra::values(res[['stream']])))
		res[['id']][j] = NA
		i = which(!is.na(terra::values(res[['id']])))
		res[['id']][i] = 1:length(i)
	}

	## clean up files
	.clean_grass()

	if(!missing(file))
		res = terra::writeRaster(res, filname = file)

	res
}


#' @name vectorise_stream
#' @rdname vectorise_stream
#' @title Vectorize a stream layer
#' Produces a vector layer (in `sf` format) from a raster stream map as created by [delineate()].
#' @param x A [terra::SpatRaster], such as one created by [delineate()].
#' @param Tp A pixel topology
#' @return An `sf` stream layer
#' @examples
#' \donttest{
#'     data(kamp_dem)
#'     x = delineate(kamp_dem)
#'     vect = vectorise_stream(x$stream, pixel_topology(x))
#' }
#' @export
vectorise_stream = function(x, Tp = pixel_topology(x)) {

	rids = terra::unique(x$stream)[,1]
	vals = as.data.frame(x, xy = TRUE)
	vals = subset(vals, !is.na(id))
	sf_pts = sf::st_as_sf(vals, coords = c('x', 'y'), crs=sf::st_crs(x))

	# if unix, use multiple cores if mc.cores is specified
	cores = ifelse(.Platform$OS.type == "unix", getOption("mc.cores", 1L), 1L)

	# lines = list()
	# nr = length(rids)
	# for(i in 1:nr) {
	# 	lines[i] = .pts_to_line(rids[i], x = x, pts = sf_pts, Tp = Tp)
	# 	cat(paste0(Sys.time(), "  ", i, "/", nr, " (", round(100 * i/nr, 0), "%)", "\r"))
	# }

	lines = parallel::mclapply(rids, .pts_to_line, x = x, pts = sf_pts, Tp = Tp, mc.cores = cores)
	do.call(rbind, lines)
}

.pts_to_line = function(i, x, pts, Tp) {
	pids = extract_reach(i, x, Tp, sorted = TRUE)
	pids = c(pids, .downstream(Tp, pids[length(pids)]))

	# in rare cases, end up with a single-pixel reach
	# if so, we take out upstream pixel to connect it to
	if(length(pids) == 1) 
		pids = c(pids, .upstream(Tp, pids)[1])

	xy = sf::st_cast(sf::st_geometry(pts[pids,]), "POINT")
	# convert each pair of points into a linestring
	lines = sapply(1:(length(pids)-1), function(j)
		sf::st_cast(sf::st_combine(xy[j:(j+1)]), "LINESTRING"))
	# convert the whole thing to a multilinestring
	mline = sf::st_sf(sf::st_geometry(sf::st_multilinestring(lines)))
	sf::st_crs(mline) = sf::st_crs(pts)
	mline$reach_id = i
	mline
}



#' Resize reaches from a delineated stream
#' @param x A [terra::SpatRaster] stream stack, such as one created by [delineate()].
#' @param len The target length of resized reaches (in map units)
#' @param min_len How small is the smallest acceptable reach?
#' @param trim Logical; remove headwater reaches smaller than min_length?
#' @param Tp A pixel topology
#' @return A raster similar to `x` with reaches resized
#' @examples
#' \donttest{
#'     data(kamp_dem)
#'     x = delineate(kamp_dem)
#'     Tp = pixel_topology(x)
#'     x[['stream']] = resize_reaches(x, Tp, 1000, 600)
#' }
#' @export
resize_reaches = function(x, Tp, len, min_len = 0.5 * len, trim = TRUE) {

	# if unix, use multiple cores if mc.cores is specified
	cores = ifelse(.Platform$OS.type == "unix", getOption("mc.cores", 1L), 1L)

	stream = x[['stream']]
	ids = terra::unique(stream)[,1]

	pix_ids = terra::values(x[['id']])

	pids = parallel::mclapply(ids, extract_reach, x=x, Tp = Tp, sorted = TRUE, mc.cores = cores)

	new_reaches = unlist(parallel::mclapply(pids, .resize_r, Tp = Tp, trim = trim, len = len,
			min_len = min_len, mc.cores = cores), recursive = FALSE)
	stream = stream * NA
	for(i in seq_along(new_reaches)) {
		resized_ids = new_reaches[[i]]
		j = match(resized_ids, pix_ids)
		stream[j] = i
	}

	storage.mode(stream[]) = 'integer'
	stream
}

#' Split a single reach
#' @param ids A vector of pixel ids in the reach, sorted from upstream to downstream
#' @param Tp A pixel topology
#' @param trim Logical, should we trim small headwater reaches?
#' @param len Target reach length
#' @param min_len Minimum acceptable reach length, only used if `trim` is `TRUE`
#' @return A list of vectors, each vector is a single new reach.
#' @keywords internal
.resize_r = function(ids, Tp, trim, len, min_len) {

	if(length(ids) == 1)
		return(list(ids))

	## only trim headwater reaches
	trim = trim && .is_headwater(Tp)[ids[1]]

	# we will resize from bottom to top, then reverse again at the end
	ids = rev(ids)

	pix_lens = Matrix::rowSums(Tp[ids,])

	## compute target number of reaches
	rlen = sum(pix_lens)
	nr = ceiling(rlen/len)

	## if not trimming, decide on new reach length to get reaches mostly even lengths
	## try the computed number of reaches +/- 1
	## pick the one that gets us closest to the target length
	if(!trim) {
		cand = rlen / (nr + -1:1)
		ldiff = abs(len - cand)
		len = cand[which.min(ldiff)]
		nr = nr + (-1:1)[which.min(ldiff)]
	}

	new_rids = ceiling(cumsum(pix_lens) / len)

	## outlet of the whole river network will end up with an id of 0; fix it
	new_rids[new_rids == 0] = 1

	# any pixels given a new id > nr are short headwater pixels that should be dropped (if trimming)
	# or should be added to the last reach (if not trimming)
	if(any(new_rids > nr)) {
		if(trim) {
			new_rids = new_rids[new_rids <= nr]
		}
		else {
			new_rids[new_rids > nr] = nr
		}
	}
	
	new_reaches = lapply(unique(new_rids), function(i) ids[new_rids == i])

	## if we are trimming, drop the headwater if it is too short
	if(trim) {
		r = new_reaches[[length(new_reaches)]]
		hwlen = sum(pix_lens[match(r, ids)])
		if(hwlen < min_len)
			new_reaches[[length(new_reaches)]] = NULL
	}

	# reorder the list so that the reaches are sorted from headwater to outlet
	rev(new_reaches)
}

