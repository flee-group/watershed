ybbs_dem_sm = readRDS(system.file("testdata/ybbs_dem_sm.rds", package="watershed"))
test_that("r.watershed Works", {
	expect_error(ybbs <- delineate(ybbs_dem_sm), regex=NA)
})
