test_that("toplogy creation", {
	skip_on_cran()
	require(raster)
	kamp_sm = readRDS(system.file("testdata/kamp_sm.rds", package="watershed"))
	expect_error(kamp_T <- pixel_topology(kamp_sm), regex=NA)
})

