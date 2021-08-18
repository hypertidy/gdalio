
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
  if (.is_native(g)) {
    g <- gdalio_get_default_grid(dsn)
  }
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
#' @param col optional colour values to set for a data raster (like 'image()' see [palr::image_pal()])
#' @param breaks optional interval values to set for a data raster (like 'image()' see 'col')
#' @return 'gdalio_data_rgb()' a list of numeric vectors, 'gdalio_data_hex()' a character vector of "#" colours
#' @export
#' @name gdalio_data_rgb
#' @export
gdalio_data_rgb <- function(dsn, ..., bands = 1:3) {
 gdalio_data(dsn, bands = bands, ...)
}

#'
#' @name gdalio_data_rgb
#' @importFrom grDevices rgb
#' @export
gdalio_data_hex <- function(dsn, bands = 1:3, max_col_value = 255, ..., col = NULL, breaks = NULL) {
  v <- gdalio_data(dsn, bands = bands, ...)

  if (length(v) < 3 ) {
    if (length(v) == 1L) {
      ## assume we use col,breaks, or just give grey
      ## case col,breaks both NULL or only breaks given
      if (is.null(col)) out <- .convert_band_grey(v[[1]], breaks = breaks)
      ## case breaks NULL, col present
      if (!is.null(col) && is.null(breaks)) out <- .convert_band_cols(v[[1]], col = col)
      if (!is.null(col) && !is.null(breaks)) out <- .convert_band_colbreaks(v[[1]], col = col, breaks = breaks)
      return(out)
    }
    #stop("did not obtain 3 bands from data source")
  }
  if (length(v) > 4 ) {
    message("obtained more than 4 bands from data source, ignoring all but first 4")
    v <- v[1:4]
  }
  .convert_list_bands_hex(v, max_col_value = max_col_value)
}

#' @importFrom grDevices grey.colors
#' @importFrom palr image_pal
.convert_band_grey <- function(v, breaks = NULL) {
  cols <- if (!is.null(breaks)) grey.colors(length(breaks)  - 1) else grey.colors(64)
  palr::image_pal(v, col = cols, breaks = breaks)
}
.convert_band_cols <- function(v, col) {
  palr::image_pal(v, col = col)
}
.convert_band_colbreaks <- function(v, col, breaks) {
  palr::image_pal(v, col = col, breaks = breaks)
}
.convert_list_bands_hex <- function(v, max_col_value = 255) {
  if (length(v) == 3) {
    out <- grDevices::rgb(v[[1]], v[[2]], v[[3]], maxColorValue = max_col_value)
  } else {
    out <- grDevices::rgb(v[[1]], v[[2]], v[[3]], v[[4]], maxColorValue = max_col_value)
  }
}

