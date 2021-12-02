#' @rdname geom_offset_bezier
#' @export
geom_offset_bezier_h <- function(mapping = NULL,
                                 data = NULL,
                                 stat = "identity",
                                 position = "identity",
                                 ...,
                                 w = NULL,
                                 stepFn = nSteps(100),
                                 open = TRUE,
                                 lineend = "butt",
                                 linejoin = "round",
                                 mitrelimit = 4,
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
                                       stepFn = stepFn,
                                       open = open,
                                       lineend = lineend,
                                       linejoin = linejoin,
                                       mitrelimit = mitrelimit,
                                       d = d,
                                       rep = rep,
                                       na.rm = na.rm,
                                       ...)
  ),
  class = "geom_offset_bezier_h")
}

#' @rdname geom_offset_bezier
#' @export
geom_offset_bezier_v <- function(mapping = NULL,
                                 data = NULL,
                                 stat = "identity",
                                 position = "identity",
                                 ...,
                                 w = NULL,
                                 stepFn = nSteps(100),
                                 open = TRUE,
                                 lineend = "butt",
                                 linejoin = "round",
                                 mitrelimit = 4,
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
                                       stepFn = stepFn,
                                       open = open,
                                       lineend = lineend,
                                       linejoin = linejoin,
                                       mitrelimit = mitrelimit,
                                       d = d,
                                       rep = rep,
                                       na.rm = na.rm,
                                       ...)
  ),
  class = "geom_offset_bezier_v")
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add geom_offset_bezier_h
#' @export
ggplot_add.geom_offset_bezier_h <- function(object, plot, object_name) {
  xrng <- diff(xrange(plot))
  object <- unclass(object)
  object$geom <- GeomOffsetBezier
  object$params <- utils::modifyList(object$params, list(unit  = "npc",
                                                         scale = xrng))
  object <- do.call(layer, object)
  ggplot_add(object, plot, object_name)
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add geom_offset_bezier_v
#' @export
ggplot_add.geom_offset_bezier_v <- function(object, plot, object_name) {
  yrng <- diff(yrange(plot))
  object <- unclass(object)
  object$geom <- GeomOffsetBezier
  object$params <- utils::modifyList(object$params, list(unit  = "npc",
                                                         scale = yrng))
  object <- do.call(layer, object)
  ggplot_add(object, plot, object_name)
}

