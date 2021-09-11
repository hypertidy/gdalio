#reprex::reprex({
  ve_earth <- function(type = c("satellite", "map")) {
  type <- c(satellite = "a", map = "r")[match.arg(type)]
  server_num <- "{server_num}"
  quadkey <- "{quadkey}"
  glue::glue_data(list(type = type), '<GDAL_WMS>
  <Service name="VirtualEarth">
    <ServerUrl>http://{type}${server_num}.ortho.tiles.virtualearth.net/tiles/{type}${quadkey}.png?g=90</ServerUrl>
  </Service>
  <MaxConnections>4</MaxConnections>
  <Cache/>
</GDAL_WMS>')
}

aws_elev <- function() {
  '<GDAL_WMS>
    <Service name="TMS">
      <ServerUrl>https://s3.amazonaws.com/elevation-tiles-prod/geotiff/${z}/${x}/${y}.tif</ServerUrl>
      </Service>
      <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
      <TileLevel>14</TileLevel>
      <TileCountX>1</TileCountX>
      <TileCountY>1</TileCountY>
      <YOrigin>top</YOrigin>
      </DataWindow>
      <Projection>EPSG:3857</Projection>
      <BlockSizeX>512</BlockSizeX>
      <BlockSizeY>512</BlockSizeY>
      <BandsCount>1</BandsCount>
      <DataType>Int16</DataType>
      <ZeroBlockHttpCodes>403,404</ZeroBlockHttpCodes>
      <DataValues>
      <NoData>-32768</NoData>
      </DataValues>
      <Cache/>
      </GDAL_WMS>'
}

osm_map <- function() {
  '<GDAL_WMS>
    <Service name="TMS">
      <ServerUrl>https://tile.openstreetmap.org/${z}/${x}/${y}.png</ServerUrl>
      </Service>
      <DataWindow>
      <UpperLeftX>-20037508.34</UpperLeftX>
      <UpperLeftY>20037508.34</UpperLeftY>
      <LowerRightX>20037508.34</LowerRightX>
      <LowerRightY>-20037508.34</LowerRightY>
      <TileLevel>18</TileLevel>
      <TileCountX>1</TileCountX>
      <TileCountY>1</TileCountY>
      <YOrigin>top</YOrigin>
      </DataWindow>
      <Projection>EPSG:3857</Projection>
      <BlockSizeX>256</BlockSizeX>
      <BlockSizeY>256</BlockSizeY>
      <BandsCount>3</BandsCount>
      <!--
      <UserAgent>Please add a specific user agent text, to avoid the default one being used, and potentially blocked by OSM servers in case a too big usage of it would be seen</UserAgent>
      -->
      <Cache />
      </GDAL_WMS>'
}

#remotes::install_github(c("hypertidy/vapour"))
#remotes::install_github(c("hypertidy/gdalio"))

library(gdalio)
## create a grid in any projection anywhere
g <- list(extent = c(-1, 1, -1, 1) * 350000,
          dimension = c(256, 256),
          projection = "+proj=ortho +lon_0=145 +lat_0=-37 +datum=WGS84")
gdalio_set_default_grid(g)
#g <- gdalio_get_default_grid()
vei <- gdalio_graphics(ve_earth("satellite"))
vem <- gdalio_graphics(ve_earth("map"), resample = "cubic")
ele <- gdalio_matrix(aws_elev())
ele1 <- gdalio_data(aws_elev())[[1]]

osm <- gdalio_graphics(osm_map(), resample = "cubic")
par(mar = rep(0, 4), mfrow = c(2, 2))
plot(as.raster(matrix(osm, g$dimension[2], g$dimension[1], byrow = TRUE)))
plot(as.raster(matrix(vei, g$dimension[2], g$dimension[1], byrow = TRUE)))
ele[ele < 0] <- 0
#ele1[ele1 < 0] <- 0
#plot(as.raster(t(matrix(scales::rescale(ele1), g$dimension[1]))), asp = 1, col = grey.colors(24))
image(matrix(ele, g$dimension[1], g$dimension[2], byrow = F), asp = 1, col = grey.colors(240, start = 0, end = 1))
plot(g$extent[1:2], g$extent[3:4], xaxs = "i", yaxs = "i", type = "n")
rasterImage(g$extent[1], g$extent[3],
            g$extent[2], g$extent[4], as.raster(matrix(scales::rescale(ele), g$dimension[2], g$dimension[1], byrow = TRUE)))

#})































mars <- "/vsicurl/https://planetarymaps.usgs.gov/mosaic/Mars/HRSC_MOLA_Blend/Mars_HRSC_MOLA_BlendDEM_Global_200mp_v2.tif"
#geblocal <- topofile("gebco_19")
library(gdalio)
## create a grid in any projection anywhere
g <- list(extent = c(-1, 1, -1, 1) * 350000,
          dimension = c(1024, 1024),
          projection = "+proj=ortho +lon_0=145 +lat_0=-37 +R=3389500")
gdalio_set_default_grid(g)
ele <- gdalio_data(mars)[[1]]
#ele[ele<0] <- NA
plot(g$extent[1:2], g$extent[3:4], xaxs = "i", yaxs = "i", type = "n", asp = 1)
rasterImage(as.raster(matrix(scales::rescale(ele), g$dimension[2], g$dimension[1], byrow = TRUE)),
           g$extent[1], g$extent[3],
            g$extent[2], g$extent[4])























