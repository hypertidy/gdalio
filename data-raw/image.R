
f <- system.file("extdata/sst.tif", package = "vapour", mustWork = TRUE)
r <- raster::raster(f)
pal <- palr::sst_pal(palette = TRUE)
vr <- range(raster::values(r), na.rm = TRUE)


rgba <- palr::image_raster(r, col = pal$cols, breaks = seq(vr[1], vr[2], length.out = length(pal$cols) + 1))
#  workaround https://github.com/AustralianAntarcticDivision/palr/issues/10
rgba <- raster::setValues(raster::brick(replicate(4L, rgba[[1]], simplify = FALSE)),
                          cbind(raster::values(rgba), rep(seq(64, 200, by = 8), each = ncol(rgba))))
#plot(as.raster(scales::rescale(as.array(rgba))))


writeRaster(rgba, "inst/extdata/sst_rgba.tif")
