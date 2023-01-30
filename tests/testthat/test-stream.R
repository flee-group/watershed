library(terra)
library(sf)

test_that("r.watershed", {
	skip_on_cran()
	kamp_dem_sm = rast(readRDS(system.file("testdata/kamp_dem_sm.rds", package="watershed")))
	expect_error(kamp_sm <- delineate(kamp_dem_sm), regex=NA)
	expect_identical(crs(kamp_dem_sm), crs(kamp_sm))
	expect_error(kamp_sm2 <- delineate(kamp_dem_sm, reach_len = 200), regex=NA)
	expect_lt(length(terra::unique(kamp_sm$stream)[,1]), length(terra::unique(kamp_sm2$stream)[,1]))
	expect_error(kv <- vectorise_stream(kamp_sm, pixel_topology(kamp_sm)), regex=NA)
	expect_true(all(kv$reach_id %in% terra::unique(kamp_sm$stream)[,1]))

	## reach resizing
	expect_error(kamp_stream200 <- resize_reaches(kamp_sm, pixel_topology(kamp_sm), len = 200), 
		regex=NA)
	expect_equal(kamp_stream200[], kamp_sm2$stream[])
	
	## vectorizing
	
})

