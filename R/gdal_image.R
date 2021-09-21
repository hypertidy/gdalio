# gdal_raster()  ## get elevation
#
# gdal_image()   ## get aerial/satellite
#
# gdal_map()     ## get street map

# default grid is 360,180 longlat



scl <- function(x) {
  rg <- range(x, na.rm = TRUE)

  (x - rg[1L])/diff(rg)
}

eqc <- function(x) {
  x * pi/180 * 6378137
}
#' Title
#'
#' @param dsn
#'
#' @return
#' @export
#' @examples
#' gdalio_set_default_grid(list(extent = c(-1, 1, -1, 1) * 500,
#' dimension = dev.size("px") * c(1, 1)/2,
#' projection = "+proj=laea +lon_0=153.02 +lat_0=-27.47"))
#' op <- par(mfrow = c(2, 2), mar = rep(0, 4))
#' plot(gdal_map())
#' plot(gdal_raster())
#' plot(gdal_image())
#' plot(gdal_map(VirtualEarthStreet))
#' par(op)
#' #gdal_raster_data()
gdal_image <- function(dsn = VirtualEarthSatellite) {
  g <- gdalio_get_default_grid()
  as.raster(matrix(gdalio_data_hex(dsn, bands = 1:3, resample = "cubic"), g$dimension[2L], byrow = TRUE))
}
#' @name gdal_image
#' @export
gdal_raster <- function(dsn = AWSElevation) {
  g <- gdalio_get_default_grid()
  dd <- gdalio_data(dsn, resample = "cubic")[[1L]]
  dd[dd < 0] <- 0
  as.raster(scl(matrix(dd, g$dimension[2L], byrow = TRUE)))
}
#' @name gdal_image
#' @export
gdal_map <- function(dsn = OSMMap) {
  g <- gdalio_get_default_grid()
  as.raster(matrix(gdalio_data_hex(dsn, bands = 1:3, resample = "cubic"), g$dimension[2L], byrow = TRUE))
}
#' @name gdal_image
#' @export
gdal_raster_data <- function(dsn = AWSElevation) {
  g <- gdalio_get_default_grid()
  matrix(gdalio_data(dsn, resample = "cubic")[[1L]], g$dimension[2L], byrow = TRUE)
}




