#' @name topologies
#' @rdname topologies
#' @title Build topologies
#' Build pixel and reach topologies for delineated streams
#' @param x A [raster::stack], such as one created by [delineate()], or specify layers separately with `drain` and `stream`.
#' @param drainage Optional, ignored if `x` is provided; a drainage direction raster
#' @param stream Optional, ignored if `x` is provided; a delineated stream raster, all non-stream cells must be `NA`
#' @param Tp Topology for pixels in the network, e.g., the output from [pixel_topology()].
#' @details The topology is a square matrix showing immediate adjacency between pixels/reaches. Each row/column
#' in the topology is a single pixel/reach. Zero entries indicate no adjacency between two nodes. A non-zero entry
#' for row `i` column `j` indicates that `i` is upstream of `j`. The value in the entry is the reaction length of the
#' upstream pixel/reach; this is the midpoint to midpoint distance from i to j. The actual length of each node is stored in the
#' `length` attribute; thus `Tp[i,j] == sum(attr(Tp, "length")[c(i,j)])/2`
#' @return A [Matrix::sparseMatrix] giving the pixel or reach topology
#' @examples
#' \donttest{
#'     data(kamp_dem)
#'     kamp = delineate(kamp_dem, outlet = NA)
#'     Tp = pixel_topology(kamp)
#'     Tr = reach_topology(kamp, Tp)
#'
#' }
NULL

#' @rdname topologies
#' @export
pixel_topology = function(x, drainage, stream) {
	if(!requireNamespace("Matrix"))
		stop("This functionality requires the Matrix package; please install it with install.packages('Matrix') and try again.")

	if(! missing(x)) {
		stream = x[['stream']]
		drainage = x[['drainage']]
	}

	nr = raster::nrow(drainage)
	nc = raster::ncol(drainage)
	ncl = raster::ncell(drainage)
	vals = raster::values(raster::stack(list(
		x = raster::raster(matrix(1:nc, nrow=nr, ncol=nc, byrow=TRUE), template = drainage),
		y = raster::raster(matrix(1:nr, nrow=nr, ncol=nc), template = drainage),
		id = raster::raster(matrix(1:ncl, nrow=nr, ncol=nc, byrow=TRUE), template = drainage),
		drainage = drainage,
		stream = stream)))
	vals = vals[complete.cases(vals),]

	## compute the length of each pixel
	r = raster::res(stream)
	vals = cbind(vals, len = ifelse(vals[, 'drainage'] %in% c(1,3,5,7), sqrt(r[1]^2 + r[2]^2),
									ifelse(vals[, 'drainage'] %in% c(2, 6), r[2], r[1])))
	xy = .flowto(vals, xmax = nc, ymax = nr)

	## this is the reaction length of each connection, the midpoint-midpoint distance
	p_len = (vals[match(xy[,'from_id'], vals[,'id']), 'len'] + vals[match(xy[,'to_id'], vals[,'id']), 'len'])/2

	res = Matrix::sparseMatrix(i = xy[,'from_id'], j = xy[,'to_id'], dims=rep(raster::ncell(drainage), 2), x = p_len)
	attr(res, "length") = vals[,'len']
	.check_topology(res, warn = TRUE)
	res
}

#' @rdname topologies
#' @export
reach_topology = function(x, Tp, stream) {
	if(missing(Tp))
		stop("A topology is required for this function")

	if(!requireNamespace("Matrix"))
		stop("This functionality requires the Matrix package; please install it with install.packages('Matrix') and try again.")

	if(missing(stream))
		stream = x[['stream']]

	if(!is.null(getOption("mc.cores")) && getOption("mc.cores") > 1) {
		lapplfun = parallel::mclapply
	} else {
		lapplfun = lapply
	}

	rids = raster::unique(stream)

	if(!all(rids == 1:length(rids)))
		stop("Reach IDs not in strict ascending order, they must be renumbered")

	adj = lapplfun(rids, .upstream_r, stream = stream, Tx = Tp)
	adj = do.call(rbind, adj)

	## compute the length of each reach as the sum of all pixels
	pids = lapplfun(rids, extract_reach, stream=stream, Tp = Tp)
	lens = unlist(lapplfun(pids, function(x) sum(Tp[x,,drop=FALSE])))

	## the reaction length is the midpoint to midpoint distance for each reach
	## actual reach lengths are saved in an attribute
	r_len = (lens[adj[,1]] + lens[adj[,2]])/2

	res = Matrix::sparseMatrix(adj[,'from'], adj[,'to'], dims=rep(max(rids), 2),
									dimnames = list(rids, rids), x = r_len)
	attr(res, "length") = lens
	res
}



#' Find pixel IDs matching reach i
#' @param i Reach number to extract
#' @param stream Raster stream layer
#' @param Tp optional Topology, only needed for sorting
#' @param sorted Logical, should the ids be returned sorted from upstream to down?
#' @return A vector of pixel ids
#' #' @examples
#' \donttest{
#'     data(kamp_dem)
#'     kamp = delineate(kamp_dem, outlet = NA)
#'     Tp = pixel_topology(kamp)
#'     extract_reach(5, kamp$stream, Tp)
#' }
#' @export
extract_reach = function(i, stream, Tp, sorted = FALSE) {
	pids = which(raster::values(stream) == i)
	if(sorted && length(pids) > 1) {
		pid_s = numeric(length(pids))
		Tp_r = Tp[pids, pids, drop=FALSE]
		pid_s[1] = .headwater(Tp_r)
		for(i in 2:length(pid_s))
			pid_s[i] = .downstream(Tp_r, pid_s[i-1])
		pids = pids[pid_s]
	}
	pids
}



#' @name upstream
#' @rdname upstream
#' @title Simple topology analysis
#' Simple functions for extracting information from river topologies
#' @param Tx A (pixel or reach) topology
#' @param i,j Focal node
#' @param stream A stream raster (as produced by [delineate()]) with reachIDs as the values
#' @details upstream/downstream take a single node as input, and return the up- or downstream node.
#' Outlets/headwaters analyzes the entire toplogy and finds nodes that have no downstream or upstream neighbours.
#' @return The id (corresponding to dims of Tx) of the desired node(s)
NULL


#' @rdname upstream
#' @keywords internal
.upstream = function(Tx, j) {
	which(Tx[,j] != 0)
}

#' @rdname upstream
#' @keywords internal
.downstream = function(Tx, i) {
	which(Tx[i,] != 0)
}

#' @rdname upstream
#' @keywords internal
.headwater = function(Tx) {
	which(.is_headwater(Tx))
}

#' @rdname upstream
#' @keywords internal
.outlet = function(Tx) {
	which(.is_outlet(Tx))
}

#' @rdname upstream
#' @keywords internal
.is_headwater = function(Tx) {
	## single-member topologies are always both headwaters and outlets
	if(all(dim(Tx) == c(1,1))) {
		return(TRUE)
	} else {
		## check for rowsums as well because we exclude nodes that are connected to nothing
		(Matrix::colSums(Tx) == 0 & Matrix::rowSums(Tx) != 0)
	}
}

#' @rdname upstream
#' @keywords internal
.is_outlet = function(Tx) {
	## single-member topologies are always both headwaters and outlets
	if(all(dim(Tx) == c(1,1))) {
		return(TRUE)
	} else {
		## check for colsums as well because we exclude nodes that are connected to nothing
		(Matrix::colSums(Tx) != 0 & Matrix::rowSums(Tx) == 0)
	}
}


#' @rdname upstream
#' @return For `.upstream_r`, A named2-column matrix, 'to' is the downstream reach (i.e., i) and
#' 'from' contains ids of the reach(es) upstream of i; or NULL if there are none
#' @keywords internal
.upstream_r = function(i, stream, Tx) {
	pids = extract_reach(i, stream)
	Tp_r = Tx[pids, pids, drop=FALSE]
	r_top = pids[.headwater(Tp_r)]
	r_nb = .upstream(Tx, r_top)
	if(length(r_nb) > 0) {
		from_ids = stream[r_nb]
		res = cbind(to = i, from = from_ids)
	} else {
		res = NULL
	}
	res
}






#' Compute which pixels flow into which other pixels
#' @param mat A matrix with 4 named columns; 'x', 'y', 'drainage', and 'id'
#' @return A matrix of IDs, the first column the upstream pixel, the second column downstream pixel
#' @keywords internal
.flowto = function(mat, xmax, ymax) {
	mat = mat[mat[, 'drainage'] > 0,] ## can only interpret positive drainages
	xoffset = c(1, 0, -1, -1, -1, 0, 1, 1)
	yoffset = c(-1, -1, -1, 0, 1, 1, 1, 0)
	newx = mat[,'x'] + xoffset[mat[,'drainage']]
	newy = mat[,'y'] + yoffset[mat[,'drainage']]

	res_mat = cbind(mat, newx, newy)
	res_mat = merge(res_mat[,c('newx', 'newy', 'id', 'drainage')], res_mat[,c('x', 'y', 'id')], by = c(1,2), all.x = TRUE)
	res_mat = res_mat[,c('newx', 'newy', 'drainage', 'id.x', 'id.y')]
	colnames(res_mat)[4:5] = c('from_id', 'to_id')

	# res_mat = .fix_topology(res_mat, mat)
	res_mat = res_mat[order(res_mat[,'from_id']),]
	res_mat = res_mat[complete.cases(res_mat),]
	return(res_mat)
}



#' Verify that the topology is working as expected
#' @param Tp A topology matrix
#' @param warn if TRUE, warnings are raised instead of errors
#' @return NULL, raises errors/warnings when invalid topologies detected
#' @keywords internal
.check_topology = function(Tp, warn = FALSE) {
	if(warn) {
		fun = warning
	} else {
		fun = stop
	}

	rs = Matrix::rowSums(Tp != 0)
	cs = Matrix::colSums(Tp != 0)
	if(any(rs > 1))
		fun("Invalid topology; ", sum(rs > 1), " nodes are upstream of more than one node.")

	if(sum(rs == 0 && cs != 0) > 1)
		fun("Invalid topology; ", sum(rs == 0), " outlets found.")

	if(any(cs > 2))
		fun("Invalid topology; ", sum(cs > 2), " nodes are downstream of more than two nodes.")
}
