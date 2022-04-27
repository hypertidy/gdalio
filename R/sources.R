#' Data sources for reading
#'
#' Provides a data frame of online-accessible data sources. This is very incomplete and has no
#' guarantee of success. It's your responsibility to check usage terms.
#'
#' There is a table of sources 'provider', 'name', 'source' - source is a DSN in GDAL terms, you
#' can query and read from it with GDAL.
#' @return
#' @export
#'
#' @examples
#' srcs <- gdalio_sources()
#' ttsa <- subset(srcs, provider == "tasmap" & name == "TTSA")$source
#' ve <- subset(srcs, provider == "gdaltms" & name == "wms_virtualearth_street")$source
#' gdalio_set_default_grid(list(extent = c(-1, 1, -1, 1) * 800,
#'                              dimension = rep(min(dev.size("px")), 2L),
#'                              projection = "+proj=laea +lat_0=-42.8826 +lon_0=147.3257"))
#' source(gdalio_format_source())
#' tas_street <- gdalio_terra(ttsa, bands = 1:3, resample = "cubic")
#' ve_street <- gdalio_terra(ve, bands = 1:3, resample = "cubic")
#' par(mfrow = c(1, 2))
#' terra::plotRGB(tas_street)
#' terra::plotRGB(ve_street)
gdalio_sources <- function() {
  read.csv(url("https://raw.githubusercontent.com/hypertidy/gdalwebsrv/master/inst/bundle/gdalwebsrv.csv"),
           colClasses = c("character", "character"), check.names = FALSE)
}

