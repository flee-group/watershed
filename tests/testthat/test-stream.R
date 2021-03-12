kamp_dem_sm = readRDS(system.file("testdata/kamp_dem_sm.rds", package="watershed"))
test_that("r.watershed Works", {
	skip_on_cran()
	expect_error(kamp <- delineate(kamp_dem_sm), regex=NA)
})
