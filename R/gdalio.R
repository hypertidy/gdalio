

.gdalio_default_grid <- function() {
  list(extent = c(-180, 180, -90, 90),
       dimension = c(180, 90),
       projection = "+proj=longlat +datum=WGS84")
}

.valid_check_grid <- function(x) {
  has_extent <- is.numeric(x[["extent"]]) && length(x[["extent"]] == 4) && all(!is.na(x[["extent"]])) &&
    diff(x[["extent"]][1:2]) > 0 && diff(x[["extent"]][3:4]) > 0
  if (!has_extent) stop("invalid extent")
  has_dim <- is.numeric(x[["dimension"]]) && length(x[["dimension"]] == 2) && all(!is.na(x[["dimension"]])) && all(x[["dim"]] > 0)
  if (!has_dim) stop("invalid dimension")
  has_proj <- is.character(x[["projection"]]) && length(x[["projection"]] == 1) && !is.na(x[["projection"]])
  if (!has_proj) stop("invalid projection")
  TRUE
}

#' Title
#'
#' @param x optional grid specification
#'
#' @return nothing, used to set a default grid available globally
#' @export
#'
#' @examples
#' gdalio_set_default_grid(list(extent = c(-1000, 1000, -2000, 2000),
#'    dimension = c(100, 200), projection = "+proj=longlat"))
#' gdalio_set_default_grid()
gdalio_set_default_grid <- function(x) {
 if (missing(x)) {
   x <- .gdalio_default_grid()
 } else {
    .valid_check_grid(x)
 }
 options(gdalio.default.grid = x)
}

#' Title
#'
#' @return grid specification (list of extent, dimension, projection)
#' @export
#'
#' @examples
#' gdalio_get_default_grid()
#' gdalio_set_default_grid(list(extent = c(-1000, 1000, -2000, 2000),
#'  dimension = c(100, 200), projection = "+proj=longlat"))
#' gdalio_get_default_grid()
#'
#' gdalio_set_default_grid()
gdalio_get_default_grid <- function() {
  getOption("gdalio.default.grid")
}

#' Read GDAL raster numeric data
#'
#' Data may be one band (the default, first band) or many.
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

.convert_list_bands_hex <- function(v, max_col_value = 255) {
  if (length(v) == 3) {
    out <- grDevices::rgb(v[[1]], v[[2]], v[[3]], maxColorValue = max_col_value)
  } else {
    out <- grDevices::rgb(v[[1]], v[[2]], v[[3]], v[[4]], maxColorValue = max_col_value)
  }
}
