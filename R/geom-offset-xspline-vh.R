#' @rdname geom_offset_xspline
#' @export
geom_offset_xspline_h <- function(mapping = NULL,
                                  data = NULL,
                                  stat = "identity",
                                  position = "identity",
                                  ...,
                                  w = NULL,
                                  shape = 1,
                                  open = TRUE,
                                  repEnds = TRUE,
                                  lineend = "butt",
                                  mitrelimit = 4,
                                  d = NULL,
                                  wshape = -1,
                                  rep = FALSE,
                                  na.rm = FALSE,
                                  show.legend = NA,
                                  inherit.aes = TRUE) {
  structure(.Data = list(data = data,
                         mapping = mapping,
                         stat = stat,
                         geom = GeomOffsetXspline,
                         position = position,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         params = list(w = w,
                                       shape = shape,
                                       open = open,
                                       repEnds = repEnds,
                                       lineend = lineend,
                                       mitrelimit = mitrelimit,
                                       d = d,
                                       wshape = wshape,
                                       rep = rep,
                                       na.rm = na.rm,
                                       ...)
  ),
  class = "geom_offset_xspline_h")
}

#' @rdname geom_offset_xspline
#' @export
geom_offset_xspline_v <- function(mapping = NULL,
                                  data = NULL,
                                  stat = "identity",
                                  position = "identity",
                                  ...,
                                  w = NULL,
                                  shape = 1,
                                  open = TRUE,
                                  repEnds = TRUE,
                                  lineend = "butt",
                                  mitrelimit = 4,
                                  d = NULL,
                                  wshape = -1,
                                  rep = FALSE,
                                  na.rm = FALSE,
                                  show.legend = NA,
                                  inherit.aes = TRUE) {
  structure(.Data = list(data = data,
                         mapping = mapping,
                         stat = stat,
                         geom = GeomOffsetXspline,
                         position = position,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         params = list(w = w,
                                       shape = shape,
                                       open = open,
                                       repEnds = repEnds,
                                       lineend = lineend,
                                       mitrelimit = mitrelimit,
                                       d = d,
                                       wshape = wshape,
                                       rep = rep,
                                       na.rm = na.rm,
                                       ...)
  ),
  class = "geom_offset_xspline_v")
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add geom_offset_xspline_h
#' @export
ggplot_add.geom_offset_xspline_h <- function(object, plot, object_name) {
  xrng <- diff(xrange(plot))
  object <- unclass(object)
  object$geom <- GeomOffsetXspline
  object$params <- utils::modifyList(object$params, list(unit  = "npc",
                                                         scale = xrng))
  object <- do.call(layer, object)
  ggplot_add(object, plot, object_name)
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add geom_offset_xspline_v
#' @export
ggplot_add.geom_offset_xspline_v <- function(object, plot, object_name) {
  yrng <- diff(yrange(plot))
  object <- unclass(object)
  object$geom <- GeomOffsetXspline
  object$params <- utils::modifyList(object$params, list(unit  = "npc",
                                                         scale = yrng))
  object <- do.call(layer, object)
  ggplot_add(object, plot, object_name)
}

