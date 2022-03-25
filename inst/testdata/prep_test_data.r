library(raster)
library(watershed)
library(sf)
library(units)

data("kamp_dem")
kamp = delineate(kamp_dem)
kv = vectorise_stream(kamp)
par(mar=c(4,4,4,4))

xl = c(4679000, 4689000) + 5000 * c(-1, 1)
yl = c(2835000, 2846000) + 5000 * c(-1, 1)
plot(kamp_dem, col=terrain.colors(20), axes = TRUE, xlim=xl, ylim=yl)
plot(st_geometry(kv), col='blue', add = TRUE)

xl = 4675000 - 5000
yl = 2835000 - 5000
xu = 4689700 + 5000
yu = 2846300 + 5000
rect(xl, yl, xu, yu, border='red')

kamp_dem_sm = crop(kamp_dem, raster::extent(c(xl, xu, yl, yu)))
saveRDS(kamp_dem_sm, "inst/testdata/kamp_dem_sm.rds")

kamp_sm = delineate(kamp_dem_sm)
## warning - a topological error. we can kill this

kv_sm = vectorise_stream(kamp_sm)

quartz()
plot(kamp_dem_sm, col=terrain.colors(20), axes = FALSE)
plot(st_geometry(kv_sm), col='blue', add = TRUE)


## prep some data for hydrology test
ca = catchment(kamp_sm, type='reach')
kv_sm$ca = ca[kv_sm$reach_id]

data(kamp_q)
kamp_q2 = st_snap(kamp_q, kv, tolerance=100)
plot(kamp_dem_sm, col=terrain.colors(20), axes = FALSE)
plot(st_geometry(kv_sm), col='blue', add = TRUE)
plot(kamp_q, col='cyan', add=TRUE)
plot(kamp_q2, col='red', add=TRUE)

## save the other intermediate calculations
kamp_sm = readAll(kamp_sm)
saveRDS(kamp_sm, "inst/testdata/kamp_sm.rds")
saveRDS(kv_sm, "inst/testdata/kv_sm.rds")


## some points get duplicated, add a point ID to eliminate them
kamp_q2$id = 1:nrow(kamp_q2)
kamp_q2 = st_intersection(kamp_q2, kv_sm)
kamp_q2$id
kamp_q2 = kamp_q2[!duplicated(kamp_q2$id),]
units(kamp_q2$ca) = "m^2"
saveRDS(kamp_q2, "inst/testdata/kamp_q_sm.rds")
