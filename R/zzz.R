.onLoad = function(libname, pkgname) {
  gdalio_set_default_grid()
  gdalio_sources <<- memoise::memoize(gdalio_sources, cache = cachem::cache_mem(max_age = 24 * 3600))

}
