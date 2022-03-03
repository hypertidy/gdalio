#' Generate a local grid
#'
#' Generate a default local grid to use for subsequent data reads.
#'
#' All arguments have default values.
#' @param x longitude
#' @param y latitude
#' @param buffer width either side of x, y
#' @param family projection family (as per PROJ strings)
#' @param dim size of grid nx, ny
#'
#' @return list appropriate for [gdalio_set_default_grid()]
#' @export
#' @importFrom grDevices dev.cur dev.size xy.coords
#' @examples
#' gdalio_local_grid()
#' gdalio_local_grid(family = "stere")
gdalio_local_grid <- function(x = 147, y = -42, buffer = 25e5, family = "laea", dim = if (dev.cur() == 1) {c(512, 512)} else {dev.size("px")}) {
  xy <- xy.coords(x, y)
  x0 <- mean(xy$x, na.rm = TRUE)
  y0 <- mean(xy$y, na.rm = TRUE)
  if (grepl("^]]+", family) || grepl("^\\[", family)) {
    projection <- family
  } else {
    projection <- sprintf("+proj=%s +lon_0=%f +lat_0=%f +datum=WGS84", family, x0, y0)
  }
  list(extent = c(-1, 1, -1, 1) * buffer/2,
       dimension = dim,
       projection = projection
  )
}


.gdalio_default_grid <- function() {
  list(extent = c(-180, 180, -90, 90),
       dimension = c(180, 90),
       projection = "+proj=longlat +datum=WGS84")
}

.valid_check_grid <- function(x) {
  if (isS4(x) && inherits(x, "BasicRaster")) {
    ## we have a {raster}
    x <- list(extent = c(x@extent@xmin, x@extent@xmax, x@extent@ymin, x@extent@ymax),
              dimension = c(x@ncols, x@nrows),
              projection = x@crs@projargs)
  }
  ## can we get away with this?
  if (isS4(x) && inherits(x, "SpatRaster")) {
      x <- try(list(extent = x@ptr$extent@.xData[["vector"]],
           dimension = c(x@ptr$ncol(), x@ptr$nrow()),
           projection = x@ptr$get_crs("wkt")), silent = TRUE)
      if (inherits(x, "try-error")) stop("cannot use terra object to set default grid")
  }

  if (inherits(x, "stars")) {
    d <- attr(x, "dimension")
    ## only simple cases will work
    ex <- c(c(d[[1]]$from -1, d[[1]]$to) * d[[1]]$delta + d[[1]]$offset,
            c(d[[2]]$from - 1, d[[2]]$to) * -d[[2]]$delta - d[[2]]$offset)
    dimension <- c(d[[1]]$to -  d[[1]]$from + 1, d[[2]]$to -  d[[2]]$from  + 1)
    crs <- d[[1]]$refsys[["wkt"]]
    x <- list(extent = ex, dimension = dimension, projection = crs)
  }
  if (inherits(x, "grd_rct")) {
    rct <- unclass(x$bbox[1])
    ex <- c(rct$xmin, rct$xmax, rct$ymin, rct$ymax)
    crs <- attr(rct, "crs")
    dimension <- dim(x$data)[1:2]
    x <- list(extent = ex, dimension= dimension, projection = crs)
  }

   if (is.character(x)) {
    tst <- try(vapour::vapour_raster_info(x))
    if (!inherits(tst, "try-error")) {
      tst$dimension <- tst$dimXY
      x <- tst[c("extent", "dimension", "projection")]
    }
  }

  has_extent <- is.numeric(x[["extent"]]) && length(x[["extent"]] == 4) && all(!is.na(x[["extent"]])) &&
    diff(x[["extent"]][1:2]) > 0 && diff(x[["extent"]][3:4]) > 0
  if (!has_extent) stop("invalid extent")
  if ("dimXY" %in% names(x)) {
    x[["dimension"]] <- x[["dimXY"]]
  }
  has_dim <- is.numeric(x[["dimension"]]) && length(x[["dimension"]]) ==2  && all(!is.na(x[["dimension"]])) && all(x[["dim"]] > 0)
  if (!has_dim) stop("invalid dimension")

  has_proj <- is.character(x[["projection"]]) && length(x[["projection"]]) == 1 && nchar(x[["projection"]]) > 0 && !is.na(x[["projection"]])
  if (!has_proj) stop("invalid projection")
  x[c("extent", "dimension", "projection")]
}

#' Title
#'
#' Input may be a list with `extent` `$dimension`, `$projection`, which is `c(xmin, xmax, ymin, ymax)`,
#' `c(ncol, nrow)`, and string (accepted by GDAL as a projection input). Alternatively, use
#' a raster, stars, or terra object. Only simple cases of stars will work (regular grids with positive x, negative y transforms).
#'
#'
#' @param x grid specification, a list with '$extent, $dimension, $projection` or a spatial grid object see Details
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
    x <- .valid_check_grid(x)
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
