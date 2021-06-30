
.make_world_shape <- function() {
  system.file("shapes/world.gpkg", package = "spData", mustWork = TRUE)
}

#' Read GDAL drawing geometry and fields data
#'
#' @param dsn character string, drawing data source understood by GDAL
#' @param ... arguments passed to 'vapour::vapour_read_attributes()'
#' @return list of blob geometries
#' @export
#' @examples
#' library(gdalio)
#' shp <- system.file("shapes/world.gpkg", package = "spData", mustWork = TRUE)
#' ## set a local context, a bit around Tasmania
#' gdalio_set_default_grid(gdalio_local_grid(147, -42, buffer  = 2e6))
#
#' ## specify a GDAL tile server, we'll warp direct to our grid above
#' virtualearth_imagery <- tempfile(fileext = ".xml")
#' writeLines('<GDAL_WMS>
#' <Service name="VirtualEarth">
#' <ServerUrl>http://a${server_num}.ortho.tiles.virtualearth.net/tiles/a${quadkey}.jpeg?g=90</ServerUrl>
#' </Service>
#' <MaxConnections>4</MaxConnections>
#' <Cache/>
#' </GDAL_WMS>', virtualearth_imagery)
#
# ## get the image, then get the vector data that intersects this box
#' img <- gdalio_raster(virtualearth_imagery, bands = 1:3)
#' plot( raster::extent(img) + 5e5, asp = 1, col = "white")
#' raster::plotRGB(img, add = TRUE)
#' axis(1)
#' axis(2)
# ## this generates a bbox index, projects that and sweeps out only WKB that are overlap our gdalio grid extent
#' bin <- drawio_data(shp)
drawio_data <- function(dsn, ...) {
  UseMethod("drawio_data")
}
# drawio_data.vrt_simple <- function(dsn, ...) {
# not sensible for vector, very unlikely needed but more general VRT would be

#' @export
#' @importFrom vapour vapour_read_extent vapour_layer_info
drawio_data.default <- function(dsn, ...) {
  g <- gdalio_get_default_grid()
  extents <- do.call(rbind, vapour::vapour_read_extent(dsn, ...))
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
  ## we probably need vapour_layer_info to ignore the dots that aren't its
  polys <- reproj::reproj(cbind(xx, yy), g$projection, source = vapour::vapour_layer_info(dsn, ...)$projection$Proj4)
  xrange <- do.call(rbind, lapply(splitna(polys[,1]), range))
  yrange <- do.call(rbind, lapply(splitna(polys[,2]), range))
  ## now we find which xrange+yrange falls in our default extent

  x_in <- (xrange[,1] >= g$extent[1]  & xrange[,1] <= g$extent[2]) |
          (xrange[,2] >= g$extent[1]  & xrange[,2] <= g$extent[2])
  y_in <- (yrange[,1] >= g$extent[3]  & yrange[,1] <= g$extent[4]) |
    (yrange[,2] >= g$extent[3]  & yrange[,2] <= g$extent[4])

  if (any(x_in & y_in)) {

    idx <- which(x_in & y_in)
    geom <- vapour:::vapour_read_geometry_ia(dsn, layer = 0L, sql = "", ex = 0, format = "wkb", ia = idx-1)
  } else {
    message("no features found")
    return(NULL)
  }
  geom
}
