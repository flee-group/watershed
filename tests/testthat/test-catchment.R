library(raster)
test_that("Catchment area", {
	skip_on_cran()

	kamp_dem = readRDS(system.file("testdata/kamp_dem_sm.rds", package="watershed"))
	kamp = delineate(kamp_dem, outlet=NA)
	expect_error(ca <- catchment(kamp), regex=NA)
	expect_equal(ca, sum(values(kamp$catchment), na.rm=TRUE) * prod(res(kamp$catchment)))

	## try for reaches
	Tp = pixel_topology(kamp)
	expect_error(ca_r <- catchment(kamp, type='reach', Tp = Tp), regex=NA)

	## outlet has largest catchment area, and is equal to the CA for the whole network
	Tr = reach_topology(kamp, Tp)
	out_r = .outlet(Tr)
	expect_equal(ca_r[out_r], max(ca_r))
	expect_equal(ca_r[out_r], ca)

	## smallest catchment area is one of the headwaters
	hw_r = .headwater(Tr)
	expect_true(min(ca_r) %in% ca_r[hw_r])

	## try for all pixels -- TOO SLOW

	## try for specific pixels
	px = coordinates(kamp)[.outlet(Tp),]
	expect_error(ca_p <- catchment(kamp, type='points', y = px, Tp = Tp), regex=NA)
	ca_p_map <- expect_error(catchment(kamp, type='points', y = px, area = FALSE, Tp = Tp), regex=NA)
	## we used the outlet, so the area and the map should be identical to the whole river
	expect_equal(ca_p, ca)
	expect_true(compareRaster(ca_p_map, kamp$catchment, values=TRUE))

})
