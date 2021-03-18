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
#' for row `i` column `j` indicates that `i` is upstream of `j`. The value in the entry is the length of the
#' upstream pixel/reach.
#' @return A [Matrix::sparseMatrix] giving the pixel or reach topology
#' @examples
#' \donttest{
#'     data(ybbs_dem)
#'     Tp = pixel_topology(ybbs_dem)
#'     Tr = reach_topology(ybbs_dem, Tp)
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
	row_ras = raster::raster(matrix(1:nr, nrow=nr, ncol=nc), template = drainage)
	col_ras = raster::raster(matrix(1:nc, nrow=nr, ncol=nc, byrow=TRUE), template = drainage)
	id_ras = raster::raster(matrix(1:ncl, nrow=nr, ncol=nc, byrow=TRUE), template = drainage)
	coords = raster::stack(list(x = col_ras, y = row_ras, drainage = drainage, id = id_ras))
	coords = raster::mask(coords, stream)

	vals = raster::values(coords)
	vals = vals[!is.na(vals[,1]), ]
	xy = .flowto(vals, xmax = nc, ymax = nr)
	# res = xy[,c('from_id', 'drainage')]
	# colnames(res)[1] = 'id'
	r = raster::res(x)
	len = ifelse(xy[, 'drainage'] %in% c(1,3,5,7), sqrt(r[1]^2 + r[2]^2),
				 ifelse(xy[, 'drainage'] %in% c(2, 6), r[2], r[1]))
	res = Matrix::sparseMatrix(i = xy[,'from_id'], j = xy[,'to_id'], dims=rep(raster::ncell(drainage), 2), x = len)
	.check_topology(res, warn = TRUE)
	res
}

#' @rdname topologies
#' @export
reach_topology = function(x, Tp, stream) {
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
	Matrix::sparseMatrix(adj[,2], adj[,1], dims=rep(max(rids), 2),
									dimnames = list(rids, rids))
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
	## check for rowsums as well because we exclude nodes that are connected to nothing
	which(Matrix::colSums(Tx) == 0 & Matrix::rowSums(Tx) != 0)
}

#' @rdname upstream
#' @return For `.upstream_r`, A named2-column matrix, 'to' is the downstream reach (i.e., i) and
#' 'from' contains ids of the reach(es) upstream of i; or NULL if there are none
#' @keywords internal
.upstream_r = function(i, stream, Tx) {
	pids = which(raster::values(stream) == i)
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


#' Check and fix problems with drainage direction
#' @param conn A preliminary topology matrix
#' @param drain Drainage direction matrix
#' @param prev_probs Previous number of problems, to allow stopping if no improvement on
#' 		subsequent calls
#' @details In some cases, drainage direction rasters don't agree with flow accumulation, resulting
#' 		in a delineated stream that doesn't have the right drainage direction. This function
#' 		attempts to detect and fix this in the adjacency matrix and the drainage layer.
#'
#' 		Not used in the current version; can be reactivated if there are problems
#' @keywords internal
#' @return A corrected topology matrix
.fix_topology = function(conn, drain, prev_probs = NA) {
	probs = which(is.na(conn[,'to_id']) & conn[,'drainage'] > 0)
	if(length(probs) == 0 | (!is.na(prev_probs) & length(probs) == prev_probs))
		return(conn)

	c_fixed = do.call(rbind, lapply(conn[probs,'from_id'], .fix_drainage, drain = drain, conn = conn))
	conn = conn[-probs,]
	conn <- rbind(conn, c_fixed)
	.fix_topology(conn, drain, prev_probs = length(probs))
}


#' Fix drainage direction for a single pixel
#' @param id ID of the problematic pixel
#' @param connMat preliminary connectivity matrix
#' @param drainMat Drainage direction matrix
#' @keywords internal
#' @return corrected row from the connectivity matrix
.fix_drainage = function(id, drain, conn) {
	i = which(drain[,'id'] == id) # problem cell index
	j = which(conn[,'to_id'] == id) # upstream of problem cell
	up_id = conn[j,'from_id']
	x = drain[i,'x']
	y = drain[i,'y']
	down_ind = which(drain[,'x'] >= x-1 & drain[,'x'] <= x+1 & drain[,'y'] >= y-1 & drain[,'y'] <= y+1 & !(drain[,'id'] %in% c(id, up_id)))
	out = conn[conn[,'from_id'] == id,]
	if(length(down_ind) == 1) {
		out[,'newx'] = drain[down_ind,'x']
		out[,'newy'] = drain[down_ind,'y']
		out[,'to_id'] = drain[down_ind,'id']
		xo = out[,'newx'] - drain[i,'x']
		yo = out[,'newy'] - drain[i,'y']
		xoffset = c(1, 0, -1, -1, -1, 0, 1, 1)
		yoffset = c(-1, -1, -1, 0, 1, 1, 1, 0)
		out[,'drainage'] = which(xoffset == xo & yoffset == yo)
	}
	out
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
