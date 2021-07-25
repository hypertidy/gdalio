
#' Read GDAL raster numeric data
#'
#' Data may be one band (the default, first band) or many.
#'
#' [gdalio_data()] returns a list of vectors, [gdalio_matrix()] and [gdalio_array()] and
#' [gdalio_graphics()] return
#' a matrix, array, matrix of the necessary shape, as used by [image()] and [plot()].
#'
#' The matrix of hex values returned by [gdalio_graphics()] cannot really be placed on a spatial
#' plot window without a lot of extra work, but it's good for fast visuals to 'plot()' the output.
#' We can write helpers to plot this thing better but WIP atm.
#'
#' @param dsn character string, raster source understood by GDAL
#' @param ... arguments passed to 'vapour::vapour_warp_raster'
#' @param bands default 1L, but can be more, duplicated in different order, or 'NULL' for all
#' @return list of numeric vectors
#' @export
#' @examples
#' \dontrun{
#'  f <- system.file("extdata/sst.tif", package = "vapour", mustWork = TRUE)
#'
#'  g <- list(extent = c(130, 160, -60, -30), dimension = c(180, 100),
#'     projection = "+proj=longlat")
#'  gdalio_set_default_grid(g)
#'  v <- gdalio_data(f)
#'  image(seq(130, 160, length.out = 181), seq(-60, -30, length.out = 101),
#'     matrix(v[[1]], g$dimension[1])[,g$dimension[2]:1], asp = 1.5)
#' }
gdalio_data <- function(dsn, ..., bands = 1L) {
 UseMethod("gdalio_data")
}
#' @export
gdalio_data.vrt_simple <- function(dsn, ..., bands = 1L) {
  ## these have to be NULL, or 4 numbers and a string
  src_extent <- .vrt_extent(dsn)
  if (is.null(src_extent)) src_extent <- 0  ## that's what vapour expects, not NULL
  src_proj <- .vrt_projection(dsn)
  gdalio_data(unclass(dsn), source_extent = src_extent, source_wkt = src_proj, bands = bands, ...)
}
#' @export
#' @importFrom vapour vapour_warp_raster
gdalio_data.default <- function(dsn, ..., bands = 1L) {
  g <- gdalio_get_default_grid()
  vapour::vapour_warp_raster(dsn, extent = g$extent, dimension = g$dimension, wkt = g$projection, bands = bands,  ...)
 }

#' Read GDAL raster data as RGB triples or hex colours
#'
#' `gdalio_data_hex` and `gdalio_data_rgb` are a little strange in that they return a
#' vector of character strings and a list of numeric values respectively.
#'
#' @param dsn character string, raster source understood by GDAL
#' @param ... arguments passed to 'vapour::vapour_warp_raster'
#' @param bands bands to read, assumes 1:3 (can be 1:4 or any ordering)
#' @param max_col_value max value for colour range, usually it's Byte values 255 (but might be 0,1 as in R's graphics)
#'
#' @return 'gdalio_data_rgb()' a list of numeric vectors, 'gdalio_data_hex()' a character vector of "#" colours
#' @export
#' @name gdalio_data_rgb
#' @export
gdalio_data_rgb <- function(dsn, ..., bands = 1:3) {
 gdalio_data(dsn, bands = bands, ...)
}

#' @name gdalio_data_rgb
#' @importFrom grDevices rgb
#' @export
gdalio_data_hex <- function(dsn, bands = 1:3, max_col_value = 255, ...) {
  v <- gdalio_data(dsn, bands = bands, ...)
  if (length(v) < 3 ) stop("did not obtain 3 bands from data source")
  if (length(v) > 4 ) {
    message("obtained more than 4 bands from data source, ignoring all but first 4")
    v <- v[1:4]
  }
  .convert_list_bands_hex(v, max_col_value = max_col_value)
}


#' @name gdalio_data
#' @export
gdalio_graphics <- function(dsn, ..., bands = 1:3) {
  hex <- gdalio_data_hex(dsn, bands = bands, ...)
  g <- gdalio_get_default_grid()
  grDevices::as.raster(t(matrix(hex, g$dimension[1])))
}
#' @name gdalio_data
#' @export
gdalio_matrix <- function(dsn, ...) {
  v <- gdalio_data(dsn, ...)
  g <- gdalio_get_default_grid()

  matrix(v[[1]], g$dimension[1])[,g$dimension[2]:1, drop = FALSE]
}
#' @name gdalio_data
#' @export
gdalio_array <- function(dsn, ...) {
  v <- gdalio_data(dsn, ...)
  g <- gdalio_get_default_grid()

  array(v[[1]], c(g$dimension, length(v)))[,g$dimension[2]:1, , drop = FALSE]
}



.convert_list_bands_hex <- function(v, max_col_value = 255) {
  if (length(v) == 3) {
    out <- grDevices::rgb(v[[1]], v[[2]], v[[3]], maxColorValue = max_col_value)
  } else {
    out <- grDevices::rgb(v[[1]], v[[2]], v[[3]], v[[4]], maxColorValue = max_col_value)
  }
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
