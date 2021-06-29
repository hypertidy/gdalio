

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

#' Title
#'
#' @param dsn character string, raster source understood by GDAL
#' @param ... arguments passed to 'vapour::vapour_warp_raster'
#'
#' @return list of numeric vectors
#' @export
#'
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
gdalio_data <- function(dsn, ...) {
 UseMethod("gdalio_data")
}
#' @export
gdalio_data.vrt_simple <- function(dsn, ...) {
  ## these have to be NULL, or 4 numbers and a string
  src_extent <- .vrt_extent(dsn)
  if (is.null(src_extent)) src_extent <- 0  ## that's what vapour expects, not NULL
  src_proj <- .vrt_projection(dsn)
  g <- gdalio_get_default_grid()
  vapour::vapour_warp_raster(dsn, extent = g$extent, dimension = g$dimension, wkt = g$projection, source_extent = src_extent, source_wkt = src_proj, ...)
}
#' @export
gdalio_data.default <- function(dsn, ...) {
  g <- gdalio_get_default_grid()
  if (is.null(list(...)$bands)) {
    bands <- NULL
  }
  vapour::vapour_warp_raster(dsn, extent = g$extent, dimension = g$dimension, wkt = g$projection, bands = bands,  ...)
 }


