# gdal_raster()  ## get elevation
#
# gdal_image()   ## get aerial/satellite
#
# gdal_map()     ## get street map

# default grid is 360,180 longlat



read_coll <- function(x) {
  paste0(readLines(x), collapse = "")
}
dsn <- tibble::tribble(
  ~name, ~bands, ~datatype, ~dsn,
  "VirtualEarthSatellite", 3L, "Byte", read_coll("../gdalwebsrv/data-raw/frmt_wms_virtualearth.xml"),
  "VirtualEarthStreet", 3L, "Byte",    gsub("a\\$", "r\\$", read_coll("../gdalwebsrv/data-raw/frmt_wms_virtualearth.xml")),
  "AWSElevation", 1L, "Float32",     "<GDAL_WMS>  <Service name=\"TMS\">    <ServerUrl>https://s3.amazonaws.com/elevation-tiles-prod/geotiff/${z}/${x}/${y}.tif</ServerUrl>  </Service>  <DataWindow>    <UpperLeftX>-20037508.34</UpperLeftX>    <UpperLeftY>20037508.34</UpperLeftY>    <LowerRightX>20037508.34</LowerRightX>    <LowerRightY>-20037508.34</LowerRightY>    <TileLevel>14</TileLevel>    <TileCountX>1</TileCountX>    <TileCountY>1</TileCountY>    <YOrigin>top</YOrigin>  </DataWindow>  <Projection>EPSG:3857</Projection>  <BlockSizeX>512</BlockSizeX>  <BlockSizeY>512</BlockSizeY>  <BandsCount>1</BandsCount>  <DataType>Int16</DataType>  <ZeroBlockHttpCodes>403,404</ZeroBlockHttpCodes>  <DataValues>    <NoData>-32768</NoData>  </DataValues>  <Cache/></GDAL_WMS>",
  "OSMMap", 3L, "Byte", read_coll("../gdalwebsrv/data-raw/frmt_wms_openstreetmap_tms.xml")
)

purrr::walk(purrr::transpose(dsn), ~assign(.x$name, .x$dsn, , envir = .GlobalEnv))
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
#'
#' @examples
#' gdalio_set_default_grid(list(extent = c(-1, 1, -1, 1) * 500,
#' dimension = dev.size("px") * c(1, 1)/2,
#' projection = "+proj=laea +lon_0=153.02 +lat_0=-27.47"))
#' par(mfrow = c(2, 2))
#' plot(gdal_map())
#' plot(gdal_raster())
#' plot(gdal_image())
#' plot(gdal_map(VirtualEarthStreet))
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




