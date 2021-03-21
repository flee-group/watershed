kamp_dem_sm = readRDS(system.file("testdata/kamp_dem_sm.rds", package="watershed"))

test_that("Fill DEM (raster object)", {
	skip_on_cran()
	expect_error(filled <- fill_dem(kamp_dem_sm), regex=NA)
	expect_gt(sum(filled[]), sum(kamp_dem_sm[]))
})
