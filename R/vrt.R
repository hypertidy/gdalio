## internal for now but we might extent later (methods for raster, stars, terra that are file-backed, and catalogue systems etc. )
as_vrt  <- function(x, source_extent = NULL, source_projection = NULL, ...) {
 UseMethod("as_vrt")
}

as_vrt.default <- function(x, source_extent = NULL, source_projection = NULL, ...) {
  if (!is.null(x) && !is.character(x)) stop("input must be a character vector")
  if (!is.null(source_projection)) {
    if (is.numeric(source_projection)) {
      message("converting numeric spec of source_projection to 'EPSG:<code>'")
      source_projection <- sprintf("EPSG:%i", source_projection)
    }
    if (!is.character(source_projection)) stop("'source_projection' must be a character vector")
  }
  structure(x, source_extent = source_extent, source_projection = source_projection, class = c("vrt_simple", "character"))
}

#' Fix missing raster metadata
#'
#' Simple metadata augmentation for raster sources.
#'
#' Simple function to add either or both of a raster source extent and projection string.
#'
#' The attributes from 'extent' as 'source_extent' and/or 'projection' as 'source_projection' are passed directly down to GDAL, via
#' the [gdalio_data()] function, which hands them on to [vapour::vapour_warp_raster()] arguments 'source_extent' and 'source_wkt'
#' respectively.
#'
#' @param x character string, file, url, GDAL dsn
#' @param extent numeric 'c(xmin, xmax, ymin, ymax)'
#' @param projection character wkt, proj, epsg code
#'
#' @return lightly classed character vector, with "vrt_simple", "character"
#' @export
#'
#' @examples
#' vrt("myfile.nc")
#' vrt("myfile.nc", extent = c(-180, 180, -90, 90))
#' str(vrt("myfile.nc", extent = c(-180, 180, -90, 90), projection = 4326))
vrt <- function(x, extent = NULL, projection = NULL) {
  UseMethod("vrt")
}
#' @name vrt
#' @export
vrt.default <- function(x, extent = NULL, projection = NULL) {
  as_vrt(x, source_extent = extent, source_projection = projection)
}
.vrt_extent <- function(x) {
  attr(x, "source_extent")
}
.vrt_projection <- function(x) {
  attr(x, "source_projection")
}
#' @export
print.vrt_simple <- function(x, ...) {
  ext <- .vrt_extent(x)
  src <- .vrt_projection(x)
  if (!is.null(ext) || !is.null(src)) {
    if (is.null(ext)) ext <- "<NA>"
    if (is.null(src)) src <- "<NA>"
    cat(sprintf("VRT augmented source:\n\n<%s>\n\nExtent    : %s\nProjection: '%s'",
                unclass(x),
                paste(paste0(ext, collapse = ", "), "(xmin, xmax, ymin, ymax)"),
                src))
  } else {
    print(unclass(x))
  }
  invisible(x)
}


#' @export
as.character.vrt_simple <- function(x, ...) {
  unclass(x)
}

#' @export
`[.vrt_simple` <- function(x, i, ...) {
  vrt(NextMethod())
}

#' @export
`[[.vrt_simple` <- function(x, i, ...) {
  vrt(NextMethod())
}


#' @importFrom methods setOldClass
setOldClass(c("vrt", "character"))


