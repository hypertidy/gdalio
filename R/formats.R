#' @name gdalio_data
#' @export
gdalio_graphics <- function(dsn, ..., bands = 1:3, extent = NULL, dimension = NULL, projection = NULL) {
  hex <- gdalio_data_hex(dsn, bands = bands, ...)
  g <- .handle_args(gdalio_get_default_grid(dsn), extent, dimension, projection)
  grDevices::as.raster(t(matrix(hex, g$dimension[1])))
}
#' @name gdalio_data
#' @export
gdalio_matrix <- function(dsn, ..., bands = 1L, extent = NULL, dimension = NULL, projection = NULL) {
  v <- gdalio_data(dsn, ...)
  g <- .handle_args(gdalio_get_default_grid(dsn), extent, dimension, projection)
  matrix(v[[bands[1L]]], g$dimension[1])[,g$dimension[2]:1, drop = FALSE]
}
#' @name gdalio_data
#' @export
gdalio_array <- function(dsn, ..., extent = NULL, dimension = NULL, projection = NULL) {
  v <- gdalio_data(dsn, ...)
  g <- .handle_args(gdalio_get_default_grid(dsn), extent, dimension, projection)
  array(v[[1]], c(g$dimension, length(v)))[,g$dimension[2]:1, , drop = FALSE]
}

#' @name gdalio_data
#' @export
gdalio_image <- function(dsn, ..., extent = NULL, dimension = NULL, projection = NULL) {
  out <- .gdalio_grid_coords_cent_xy()
  out$y <- rev(out$y)
  out$z <- gdalio_matrix(dsn, ..., extent, dimension, projection)
  out
}
.gdalio_grid_coords_cent_xy <- function() {
   g <- gdalio_get_default_grid()
   res <- c(diff(g$extent[1:2])/g$dimension[1L], diff(g$extent[3:4])/g$dimension[2L])
   xs <- seq(g$extent[1L] + res[1L]/2, g$extent[2L] - res[1L]/2, length.out = g$dimension[1L])
   ## top to bottom
   ys <- seq(g$extent[4L] - res[2L]/2, g$extent[3L] + res[2L]/2, length.out = g$dimension[2L])
  list(x = xs, y = ys)
}
.gdalio_grid_coords_cent <- function() {
  xy <- .gdalio_grid_coords_cent_xy()
  xs <- xy$x
  cbind(x = xs, y = rep(ys, each = length(xs)))
}
#' @name gdalio_xyz
#' @export
gdalio_xyz <- function(dsn, ...) {
  v <- gdalio_data(dsn, ...)
  cbind(.gdalio_grid_coords_cent(), do.call(cbind, v))
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
#' You can 'source()' the file path displayed by this function to define package-specific formats for the gdalio data.
#'
#' Running the code in the file path displayed by this function will load functions for terra, stars, raster, and spatstat.
#' @export
#' @examples
#' \dontrun{
#' source(gdalio_format_source())
#' }
gdalio_format_source <- function() {
  system.file("raster_format/raster_format.codeR", package = "gdalio", mustWork = TRUE)
}
