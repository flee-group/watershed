#' Intersect a river feature with a polygon feature
#'
#' w_intersect handles the use case where we want to summarize areal features, 
#' such as land use, soils, etc, across reaches of a river, while taking into account drainage area, 
#' flow, etc.
#' 
#' There are three output types:
#' * `riparian`: The layers in `areas` are summarized within a riparian buffer around each reach
#' * `riparian_upstream`: The layers in `areas` are summarized within a riparian buffer for each 
#' 		focal reach and all upstream reaches
#' * `catchment`: The layers in `areas` are summarized within the entire catchment for each reach
#'
#' @param riv An sf LINESTRING layer, the river channel
#' @param areas Areal features to summarize; see 'details.'
#' @param area_id Optional character vector giving field/column names that should be summarised;
#' see 'details.'
#' @param drainage Drainage direction raster for calculating catchment
#' @param pts An `sf` point layer, one point per reach; the locations to compute the catchment of
#' each reach. If missing, the last point in each reach in `riv` is used.
#' @param buff Buffer width
#' @param rid The column name in 'riv' containing reach ID numbers
#'
#' @details `areas` must be in one of the following formats: `terra::SpatRaster`, `raster::raster`, 
#' `sp::SpatialPolygons`, or `sf::sf`. Multiple features are allowed, in which case provide a 
#' raster stack/multi band raster, or a list of polygon features.
#'
#' If polygon features are specified in `areas`, you must also specify which attributes should be
#' summarised in `area_id`. Currently a single attribute per feature is supported, and it must
#' be specified by name, not column index. Partial matching with regular expressions is allowed, but 
#' each item in `area_id` must identify a single column in `areas`. Ignored if `areas` is a 
#' raster.
#'
#' @import data.table
#' @return A data.table summarising the polygons in each layer in areas along the river provided in x.
#' @export
w_intersect = function(riv, areas, area_id = NULL, drainage, pts, buff = 50, rid = "reachID") {
	if(!requireNamespace("data.table", quietly = TRUE))
		stop("package 'data.table' is required for this functionality")
	if(!requireNamespace("terra", quietly = TRUE))
		stop("package 'terra' is required for this functionality")
	if(!requireNamespace("fasterize", quietly = TRUE))
		stop("package 'fasterize' is required for this functionality")

	if(missing(pts)) {
		pts = do.call(rbind, lapply(1:nrow(riv), \(i) {
			x = suppressWarnings(st_cast(riv[i,], "POINT"))
			x[nrow(x),]
		}))
	}

	if(methods::is(drainage, "SpatRaster"))
		drainage = raster::raster(drainage)

	if(class(areas) %in% c("RasterStack", "SpatRaster", "RasterLayer")) {
		areas = .process_area(areas)
	} else {
		areas = .process_area(areas, area_id, drainage)
	}
	

	# create buffers
	riv_buff = sf::st_sf(sf::st_union(sf::st_buffer(riv, buff)))
	riv_buff =  writeRaster(fasterize::fasterize(riv_buff, raster = drainage), 
		paste0(tempfile(), ".tif"))
	riv_buff = terra::rast(riv_buff)
	riv_buff = terra::trim(riv_buff)

	rid = riv[[rid]]

	# start grass, write drainage direction raster
	dname = "drainage"
	.start_grass(drainage, dname)

	res = rbindlist(lapply(1:nrow(riv), \(i) {
		# .do_intersect(r = riv[i,], x = pts[i,], y = areas, width = buff, 
		# 	riv_buff = riv_buff, rid = rid[i], dname = dname)

		tryCatch(.do_intersect(r = riv[i,], x = pts[i,], y = areas, width = buff, 
			riv_buff = riv_buff, rid = rid[i], dname = dname), error = \(e) {
			data.table(method = "NA", layer = "NA", value = NA, area = NA, proportion = NA, 
				reachID = rid[i])})
	}))

	.clean_grass()

	res$category = ""
	for(lyr in unique(res$layer)) {
		l = terra::levels(areas[[lyr]])[[1]]
		i = which(res$layer == lyr)
		res$category[i] = l[res$value[i] + 1]
	}
	res$value = NULL
	res
}



#' Helper function to perform the watershed intersection on a single reach
#' @param r A single reach, an sf linestring
#' @param x A single point, the outlet of r
#' @param y A raster stack to intersect
#' @param width Buffer width
#' @param riv_buff A buffer layer for the entire river
#' @param rid The id number of the reach
#' @param dname The name of the drainage direction raster in grass
.do_intersect = function(r, x, y, width, riv_buff, rid, dname) {

	# 1. create a (rasterized) buffer around the vector layer for a single reach
	rb = sf::st_buffer(r, width)
	ras = raster::raster(ext=raster::extent(as(rb, "Spatial")), res=terra::res(y))
	rb = fasterize::fasterize(rb, ras)
	rb = rast(rb)
	crs(rb) = crs(y)
	yy = terra::crop(y, rb)
	rb = terra::resample(rb, yy)

	# 2. intersect y with the rasterized layer
	riparian = rb * yy
	names(riparian) = names(yy)

	# 3. compute the areal catchment at the bottom of the reach in question
	cname = paste0("catchment_", rid)
	cment = .catchment(st_coordinates(x), cname, dname)
	# .clean_grass(raster = cname)
	cment = rast(cment)
	cment = terra::crop(cment, y)

	# 4. intersect the catchment with the area feature
	cment_int = y * cment
	names(cment_int) = names(y)
	cment_int = terra::crop(cment_int, riv_buff)

	# 5. intersect the result of #4 with the whole-river buffer
	riparian_upstream = cment_int * riv_buff
	names(riparian_upstream) = names(cment_int)

	# 6. generate a summary table
	.wsi_summary(list(riparian = riparian, riparian_upstream=riparian_upstream, 
		catchment = cment_int), reachID = rid)
}

#' Summarise multiple raster layers by area
#' @name wsi_summary
#' @param ras A named list of raster stacks. The names of this list will be used for the 'method' column.
#' Layer names in each raster stack will give the layer name
#' @param ... Additional (single-valued) attributes to add to the output table
#' @import data.table
#' 
#' @return A data.table with the following columns:
#'  * method: The method used, either 'riparian', 'riparian_upstream', or 'catchment'
#'  * layer: The layer of origin for each row
#'  * value: The numeric value from the raster layer of each row
#'  * area: The area occupied by the category
#'  * proportion: The proportion of area (within the method and layer)
#' @keywords internal
.wsi_summary = function(ras, ...) {
	more_attr = list(...)
	tab = lapply(ras, .wsi_summary_single)
	tab = rbindlist(tab, idcol='method')

 	for(nm in names(more_attr)) {
 		tab[[nm]] = more_attr[[nm]]
 	}
	return(tab)
}

#' @rdname wsi_summary
#' @param ras A raster stack
#' @import data.table
#' @keywords internal
.wsi_summary_single = function(ras) {
	tab = lapply(1:terra::nlyr(ras), function(i) table(terra::values(ras[[i]])))
	area = lapply(tab, function(x) x * prod(terra::res(ras)))
	prop = lapply(area, function(x) x / sum(x))
	rbindlist(mapply(function(a, p, lyr) data.table(layer = lyr, value = as.numeric(names(a)), 
			area = as.vector(a), proportion = as.vector(p)), area, prop, names(ras), SIMPLIFY=FALSE))
}






#' Take whatever format of areal layer, process into a raster for ws_intersect
#' @param x The areal layer to process
#' @param id The id column in x, needed if x is a vector GIS layer
#' @param ras The raster template to use to create a raster from x
#' @param return A [terra::SpatRaster()], with each layer classified, optionally preserving names
#' from vector layers
#' @keywords internal
setGeneric(".process_area", function(x, ...) {
	standardGeneric(".process_area")
})

setMethod(".process_area", c(x = "list"),
	function(x, id, ras) {
		if(all(sapply(x, \(y) methods::is(y, "SpatRaster")))) {
			x = lapply(x, raster::raster)
		}
		if(all(sapply(x, \(y) methods::is(y, "RasterLayer")))) {
			x = raster::stack(x)
			.process_area(x)
		} else {
			x = mapply(\(xx, id, ras) callGeneric(xx, id, ras)[[1]], 
				xx = x, id = id, MoreArgs = list(ras = ras), SIMPLIFY = FALSE)
			terra::rast(x)
		}
	}
)

setMethod(".process_area", c(x = "RasterLayer"),
	function(x) {
		callGeneric(terra::rast(x))
	}
)

setMethod(".process_area", c(x = "RasterStack"),
	function(x) {
		callGeneric(terra::rast(x))
	}
)

setMethod(".process_area", c(x = "SpatRaster"),
	function(x, levels) {
		ii = which(!terra::is.factor(x))
		for(i in ii) {
			vals = terra::values(x[[i]])
			ll = terra::unique(x[[i]])[,1]
			if(missing(levels)) {
				levs = ll
			} else {
				levs = levels[[i]]
			}
			vals = match(vals, ll) - 1
			terra::values(x[[i]]) = vals
			levels(x[[i]]) = levs
		}
		x
	}
)

setMethod(".process_area", c(x = "SpatialPolygons"),
	function(x, id, ras) {
		callGeneric(sf::st_as_sf(x), id, ras)
	}
)

setMethod(".process_area", c(x = "sf"),
	function(x, id, ras) {
		if(methods::is(ras, "SpatRaster"))
			ras = raster::raster(ras)
		fieldname = 'ws_category'
		x[[fieldname]] = factor(x[[id]])
		tab = levels(x[[fieldname]])
		x = terra::rast(fasterize::fasterize(x, raster = ras, field = fieldname))
		callGeneric(x, tab)
	}
)

