library(raster)
library(watershed)

data("kamp_dem")
kamp = delineate(kamp_dem)
kv = vectorise_stream(kamp[["stream"]])
par(mar=c(4,4,4,4))

xl = c(4679000, 4689000) + 5000 * c(-1, 1)
yl = c(2835000, 2846000) + 5000 * c(-1, 1)
plot(kamp_dem, col=terrain.colors(20), axes = TRUE, xlim=xl, ylim=yl)
plot(st_geometry(kv), col='blue', add = TRUE)
rect(4675000, 2835000, 4689700, 2846300, border='red')

kamp_dem_sm = crop(kamp_dem, raster::extent(c(4675000, 4689700, 2835000, 2846300)))
saveRDS(kamp_dem_sm, "inst/testdata/kamp_dem_sm.rds")


kamp_sm = delineate(kamp_dem_sm)
kv_sm = vectorise_stream(kamp_sm[["stream"]])
plot(kamp_dem_sm, col=terrain.colors(20), axes = FALSE)
plot(st_geometry(kv_sm), col='blue', add = TRUE)
