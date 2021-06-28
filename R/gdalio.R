

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
.set_default_grid <- function(x) {
 if (missing(x)) {
   x <- .gdalio_default_grid()
 } else {
    .valid_check_grid(x)
 }
 options(gdalio.default.grid = x)
}
.get_default_grid <- function() {
  getOption("gdalio.default.grid")
}
 gdalio_data <- function(dsn, ...) {
  g <- .get_default_grid()
  vapour::vapour_warp_raster(dsn, extent = g$extent, dimension = g$dimension, wkt = g$projection, ...)
 }
