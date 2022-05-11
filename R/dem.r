#' Produce a DEM with basins filled in
#'
#' @param dem A [raster::raster]; a digital elevation model.
#' @param file The file name of the raster to be returned, see `details`.
#' @details This is a wrapper for [r.fill.dir](https://grass.osgeo.org/grass76/manuals/r.fill.dir.html)
#'
#' It is recommended to specify the `file` parameter (including the extension to specify
#' file format; e.g., .tif, .grd). If not specified, a temp file will be created and will be
#' lost at the end of the R session.
#' @return A filled DEM raster
#' @examples
#' \donttest{
#'     data(kamp_dem)
#'     kamp_fill = fill_dem(kamp_dem)
#' }
#' @export
fill_dem = function(dem, file = NA) {
	inp_name = "dem"
	out_name = "dem_filled"
	dir_name = "flow_direction"
	problem_name= "problems"

	## launch grass and copy data
	.start_grass(dem, inp_name)

	## perform computation
	rgrass7::execGRASS("r.fill.dir", flags=c("overwrite", "quiet"), input=inp_name,
				output = out_name, direction = dir_name, areas = problem_name)
	# make sure to add the names of created rasters to the list of layers
	ws_env$rasters = c(ws_env$rasters, out_name, dir_name, problem_name)

	## gather data
	res = .read_rasters(out_name, file)

	## clean up files
	.clean_grass()

	res
}


#' Compute the slope along the river channel
#' 
#' Computes the change in elevation from one reach to the next.
#' @param x A river vector layer in sf format, e.g., output from [vectorise_stream()]
#' @param dem A digital elevation model, preferably a [terra::SpatRaster]
#' @return Vector of slopes, one per reach, map units (e.g., meters of drop per meter of stream)
#' @examples
#' \donttest{
#'     data(kamp_dem)
#'     x = delineate(kamp_dem)
#'     vect = vectorise_stream(x$stream, pixel_topology(x))
#'	   vect$slope = river_slope(vect, kamp_dem)
#' }
#' @export
river_slope = function(x, dem) {
	if(!is(dem, "SpatRaster"))
		dem = terra::rast(dem)
	slopes = units::drop_units(sf::st_length(x))
	for(i in 1:nrow(x)) {
		coords = sf::st_coordinates(x[i,])[,1:2]
		elev = terra::extract(dem, coords)
		slopes[i] = diff(range(elev)) / slopes[i]
	}
	slopes
}