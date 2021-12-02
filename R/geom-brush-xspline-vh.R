#' @rdname geom_brush_xspline
#' @export
geom_brush_xspline_h <- function(mapping = NULL,
                                 data = NULL,
                                 stat = "identity",
                                 position = "identity",
                                 ...,
                                 w = NULL,
                                 brush = verticalBrush,
                                 shape = 1,
                                 angle = "perp",
                                 open = TRUE,
                                 spacing = NULL,
                                 d = NULL,
                                 rep = FALSE,
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE) {
  structure(.Data = list(data = data,
                         mapping = mapping,
                         stat = stat,
                         position = position,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         params = list(w = w,
                                       brush = brush,
                                       shape = shape,
                                       angle = angle,
                                       open = open,
                                       spacing = spacing,
                                       d = d,
                                       rep = rep,
                                       na.rm = na.rm,
                                       ...)
  ),
  class = "geom_brush_xspline_h")
}

#' @rdname geom_brush_xspline
#' @export
geom_brush_xspline_v <- function(mapping = NULL,
                                 data = NULL,
                                 stat = "identity",
                                 position = "identity",
                                 ...,
                                 w = NULL,
                                 brush = verticalBrush,
                                 shape = 1,
                                 angle = "perp",
                                 open = TRUE,
                                 spacing = NULL,
                                 d = NULL,
                                 rep = FALSE,
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE) {
  structure(.Data = list(data = data,
                         mapping = mapping,
                         stat = stat,
                         position = position,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         params = list(w = w,
                                       brush = brush,
                                       shape = shape,
                                       angle = angle,
                                       open = open,
                                       spacing = spacing,
                                       d = d,
                                       rep = rep,
                                       na.rm = na.rm,
                                       ...)
  ),
  class = "geom_brush_xspline_v")
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add geom_brush_xspline_h
#' @export
ggplot_add.geom_brush_xspline_h <- function(object, plot, object_name) {
  xrng <- diff(xrange(plot))
  object <- unclass(object)
  object$geom <- GeomBrushXspline
  object$params <- utils::modifyList(object$params, list(unit  = "npc",
                                                         scale = xrng))
  object <- do.call(layer, object)
  ggplot_add(object, plot, object_name)
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add geom_brush_xspline_v
#' @export
ggplot_add.geom_brush_xspline_v <- function(object, plot, object_name) {
  yrng <- diff(yrange(plot))
  object <- unclass(object)
  object$geom <- GeomBrushXspline
  object$params <- utils::modifyList(object$params, list(unit  = "npc",
                                                         scale = yrng))
  object <- do.call(layer, object)
  ggplot_add(object, plot, object_name)
}

