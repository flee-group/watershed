ybbs_dem_sm = readRDS(system.file("testdata/ybbs_dem_sm.rds", package="watershed"))

test_that("Fill DEM (raster object)", {
	skip_on_cran()
	expect_error(filled <- fill_dem(ybbs_dem_sm), regex=NA)
})
