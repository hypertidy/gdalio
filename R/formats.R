#' @name gdalio_data
#' @export
gdalio_graphics <- function(dsn, ..., bands = 1:3) {
  hex <- gdalio_data_hex(dsn, bands = bands, ...)
  g <- gdalio_get_default_grid(dsn)
  grDevices::as.raster(t(matrix(hex, g$dimension[1])))
}
#' @name gdalio_data
#' @export
gdalio_matrix <- function(dsn, ...) {
  v <- gdalio_data(dsn, ...)
  g <- gdalio_get_default_grid(dsn)

  matrix(v[[1]], g$dimension[1])[,g$dimension[2]:1, drop = FALSE]
}
#' @name gdalio_data
#' @export
gdalio_array <- function(dsn, ...) {
  v <- gdalio_data(dsn, ...)
  g <- gdalio_get_default_grid(dsn)

  array(v[[1]], c(g$dimension, length(v)))[,g$dimension[2]:1, , drop = FALSE]
}

#' Example data files
#'
#' Return a path to a data raster file, for easy access to examples.
#'
#' This raster has sea surface temperature values in Celsius, with no colour palette defined.
#' @export
#' @name gdalio-example-files
#' @aliases gdalio_eg_image
#' @examples
#' gdalio_eg_raster()
#' gdalio_eg_image()
gdalio_eg_raster <- function() {
  system.file("extdata", "sst.tif", package = "vapour", mustWork = TRUE)
}
#' @export
#' @name gdalio-example-files
gdalio_eg_image <- function() {
  system.file("extdata", "sst_rgba.tif", package = "gdalio", mustWork = TRUE)
}

#' Print the code to source format-specific functions
#'
#' You can run the code displayed by this function to define package-specific formats for the gdalio data.
#'
#' Currently running the code displayed by this function will load functions for terra, stars, raster, and spatstat.
#' @export
#' @examples
#' gdalio_format_source()
gdalio_format_source <- function() {
  'source(system.file("raster_format/raster_format.codeR", package = "gdalio", mustWork = TRUE))'
}
