test_that("r.watershed Works", {
	skip_on_cran()
	require(raster)
	kamp_dem_sm = readRDS(system.file("testdata/kamp_dem_sm.rds", package="watershed"))
	expect_error(kamp_sm <- delineate(kamp_dem_sm), regex=NA)
	## kamp_sm = readAll(kamp_sm)
	## saveRDS(kamp_sm, "inst/testdata/kamp_sm.rds")
	## 
	
	## todo - test conditions
	## todo - test correctness of results
	## 
	
	expect_error(vectorize_stream(kamp_sm[['stream']]))
})
