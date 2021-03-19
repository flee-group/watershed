test_that("Catchment area working", {
	skip_on_cran()

	## todo: fix smaller test dataset, it fails here because the catchment is not complete
	data(kamp_dem, package="watershed")
	kamp = delineate(kamp_dem, outlet=NA)
	expect_error(ca <- catchment(kamp), regex=NA)
	expect_equal(ca, sum(values(kamp$catchment), na.rm=TRUE) * prod(res(kamp$catchment)))
})
