test_that("Hydrology", {
	library(sf)
	library(units)
	kamp_q = readRDS(system.file("testdata/kamp_q_sm.rds", package="watershed"))
	kamp_vect = readRDS(system.file("testdata/kv_sm.rds", package="watershed"))

	## no units set
	expect_warning(hydraulic_geometry(kamp_q$ca, kamp_q$discharge, kamp_vect$ca), regex = "units")

	## no warning if units are set
	ca = set_units(kamp_vect$ca, "m^2")
	expect_error(hyd1 <- hydraulic_geometry(kamp_q$ca, kamp_q$discharge, ca), regex = NA)

	## should work fine if units are changed
	expect_error(hyd2 <- hydraulic_geometry(kamp_q$ca, kamp_q$discharge, set_units(ca, "km^2")), regex = NA)
	expect_equal(hyd1, hyd2, tolerance = 1e-2) ## quite a bit of error due to the bayesian regression

	## should get a different calibration if we screw up the units
	hyd3 <- hydraulic_geometry(kamp_q$ca, kamp_q$discharge, set_units(kamp_vect$ca, "km^2"))
	expect_true(all(hyd3$Q > hyd1$Q))

})
