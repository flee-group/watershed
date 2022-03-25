test_that("toplogy creation", {
	skip_on_cran()
	suppressWarnings(require(raster))
	kamp_sm = readRDS(system.file("testdata/kamp_sm.rds", package="watershed"))
	expect_error(Tp <- pixel_topology(kamp_sm), regex=NA)
	
	## Todo - write working test cases for different scenarios
	
	## working case
	expect_error(Tr <- reach_topology(kamp_sm, Tp), regex=NA)

	## failure on bad reach numbers
	kmp_test = kamp
	strm_test = kmp_test[['stream']]
	strm_test[strm_test == raster::maxValue(strm_test)] = raster::maxValue(strm_test)+1
	kmp_test[["stream"]] = strm_test
	expect_error(reach_topology(Tp = Tp, x = kmp_test), regex="ascending order")

	## Todo - test correctness of topology against a known case
	
})

