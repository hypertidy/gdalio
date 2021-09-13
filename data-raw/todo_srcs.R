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

usethis::use_data(VirtualEarthSatellite)
usethis::use_data(VirtualEarthStreet)
usethis::use_data(AWSElevation)
usethis::use_data(OSMMap)


