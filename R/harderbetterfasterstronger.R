.fun <- function() {
 if (!missing(dimension))  g$dimension  <- rlang::eval_tidy(rlang::enquo(dimension), env = env)
  if (!missing(extent))     g$extent     <- rlang::eval_tidy(rlang::enquo(extent), env = env)
  if (!missing(projection)) g$projection <- rlang::eval_tidy(rlang::enquo(projection), env  = env)
}


finer <- function(factor = 2) {
  factor <- rep(factor, length.out = 2L)
  ceiling(dimension * factor)
}
coarser <- function(factor = 2) {
  factor <- rep(factor, length.out = 2L)
  ceiling(dimension / factor)
}
bigger <- function(factor = 2) {
  factor <- rep(factor, length.out = 2L)
  xlim <- extent[1:2]
  xlim <- xlim + (c(-1, 1) * diff(xlim)/2) * factor[1L]
  ylim <- extent[3:4]
  ylim <- ylim + (c(-1, 1) * diff(ylim)/2) * factor[1L]
  c(xlim, ylim)
}
wider <- function(factor = 2) {
  xlim <- extent[1:2]
  xlim <- xlim + (c(-1, 1) * diff(xlim)/2) * factor[1L]
  c(xlim, extent[3:4])
}
taller <- function(factor = 2) {
  ylim <- extent[3:4]
  ylim <- ylim + (c(-1, 1) * diff(ylim)/2) * factor[1L]
  c(extent[1:2], ylim)
}

smaller <- function(factor = 2) {
  bigger(factor = 1/factor)
}
thinner <- function(factor = 2) {
  wider(factor = 1/factor)
}
shorter <- function(factor = 2) {
  taller(factor = 1/factor)
}


