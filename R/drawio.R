
#' Read GDAL drawing geometry and fields data
#'
#' @param dsn character string, drawing data source understood by GDAL
#' @param ... arguments passed to 'vapour::vapour_read_attributes()'
#' @return list of numeric vectors
#' @export
#' @exampls
drawio_data <- function(dsn, ...) {
  UseMethod("gdalio_data")
}
#' @export
drawio_data.vrt_simple <- function(dsn, ...) {
  ## these have to be NULL, or 4 numbers and a string
  src_extent <- .vrt_extent(dsn)
  if (is.null(src_extent)) src_extent <- 0  ## that's what vapour expects, not NULL
  src_proj <- .vrt_projection(dsn)
  drawio_data(unclass(dsn), source_extent = src_extent, source_wkt = src_proj, ...)
}
#' @export
#' @importFrom vapour vapour_warp_raster
drawio_data.default <- function(dsn, ...) {
  g <- gdalio_get_default_grid()

  extents <- do.call(rbind, vapour::vapour_read_extent(dsn))
  x <- as.vector(rbind(extents[,1], extents[,1], extents[,2], extents[,2], extents[,1], NA))
  y <- as.vector(rbind(extents[,3], extents[,4], extents[,4], extents[,3], extents[,3], NA))
  ## this isnt' right still but it'll do
  xx <- approxfun(seq_along(x), x, na.rm = FALSE)(seq(1, length(x), length.out = 24 * length(x)))
  yy <- approxfun(seq_along(y), y, na.rm = FALSE)(seq(1, length(y), length.out = 24 * length(y)))
  #plot(xx, yy, type = "b")
  ## https://stat.ethz.ch/pipermail/r-help/2010-April/237031.html
  splitna <- function( x ){
    x[!is.finite(x)] <- NA
        idx <- 1 + cumsum( is.na( x ) )
        not.na <- ! is.na( x )
        split( x[not.na], idx[not.na] )
  }
  polys <- reproj::reproj(cbind(xx, yy), "+proj=laea", source = "<get source projection>")
  xrange <- do.call(rbind, lapply(splitna(polys[,1]), range))
  yrange <- do.call(rbind, lapply(splitna(polys[,2]), range))

  ## now we find which xrange+yrange falls in our default extent

  x_in <- (xrange[,1] >= g$extent[1]  & xrange[,1] <= g$extent[2]) |
          (xrange[,2] >= g$extent[1]  & xrange[,2] <= g$extent[2])
  y_in <- (yrange[,1] >= g$extent[3]  & yrange[,1] <= g$extent[4]) |
    (yrange[,2] >= g$extent[3]  & yrange[,2] <= g$extent[4])

  if (any(x_in & y_in)) {
    message("we have work to do")
  } else {
    message("nothing to do")
  }
}
