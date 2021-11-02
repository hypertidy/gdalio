# gdalio dev

* New format, matrix xyz with `gdalio_xyz()` the x, y coordinates in columns and then 1 or more bands. 

* Read functions `_graphics()` now request Byte type and `_data_rgb()` Int32
direct from GDAL. Other reader `_data()` gets what the native type has unless
`band_output_type` is provided (passed down to {vapour} to read from GDAL one of
Byte, Int32, or Float64 (the native type can be specified but is translated to
one of those 3 currently).


* New functions `gdalio_eg_raster()` and `gdalio_eg_image()` to give paths to a
single band data raster and a 4 band image raster respectively.
 
* Included `gdalio_matrix()`, `gdalio_array()`, and `gdalio_graphics()` as exported functions. 

* New function `gdalio_format_source()` prints the required code to run to load
functions for stars, terra, raster, spatstat objects.

* Added support for raster, terra, stars objects to `gdalio_set_default_grid()`. 

* New function `gdalio_local_grid()` to easily generate a local grid around a longitude,latitude. 

# gdalio 0.0.1

* New `vrt()` function to augment with extent and / or projection. 

* Basic infra working and examples. 
