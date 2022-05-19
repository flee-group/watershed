#' Compute Strahler stream order
#' @param Tx a (pixel or reach) topology
#' @return A vector of stream orders
#' @import Matrix
#' @export
strahler = function(Tx) {
	st_order = rep(as.integer(NA), nrow(Tx))
	hws = .headwater(Tx)
	st_order[hws] = 1
	stuck = FALSE
	while(!stuck & any(is.na(st_order))) {
		reaches = which(is.na(st_order))
		st_new = sapply(reaches, \(i) {
			ord = st_order[.upstream(Tx, i)]
			if(any(is.na(ord))) {
				NA
			} else if(length(ord) == 1) {
				ord
			} else if(all(ord == max(ord))) {
				max(ord) + 1
			} else {
				max(ord)
			}
		})
		if(all(is.na(st_new)))
			stuck = TRUE
		st_order[reaches] = st_new
	}
	if(stuck)
		stop("No progress being made; check topological correctness")
	st_order
}


