#' @rdname geom_vwxspline
#' @export
geom_vwxspline_h <- function(mapping = NULL,
                             data = NULL,
                             stat = "identity",
                             position = "identity",
                             ...,
                             shape = 1,
                             open = TRUE,
                             repEnds = TRUE,
                             angle = "perp",
                             lineend = "butt",
                             mitrelimit = 4,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  structure(.Data = list(data = data,
                         mapping = mapping,
                         stat = stat,
                         geom = GeomVwXspline,
                         position = position,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         params = list(shape = shape,
                                       open = open,
                                       repEnds = repEnds,
                                       angle = angle,
                                       lineend = lineend,
                                       mitrelimit = mitrelimit,
                                       na.rm = na.rm,
                                       ...)
  ),
  class = "geom_vwxspline_h")
}

#' @rdname geom_vwxspline
#' @export
geom_vwxspline_v <- function(mapping = NULL,
                             data = NULL,
                             stat = "identity",
                             position = "identity",
                             ...,
                             shape = 1,
                             open = TRUE,
                             repEnds = TRUE,
                             angle = "perp",
                             lineend = "butt",
                             mitrelimit = 4,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  structure(.Data = list(data = data,
                         mapping = mapping,
                         stat = stat,
                         geom = GeomVwXspline,
                         position = position,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         params = list(shape = shape,
                                       open = open,
                                       repEnds = repEnds,
                                       angle = angle,
                                       lineend = lineend,
                                       mitrelimit = mitrelimit,
                                       na.rm = na.rm,
                                       ...)
  ),
  class = "geom_vwxspline_v")
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add geom_vwxspline_h
#' @export
ggplot_add.geom_vwxspline_h <- function(object, plot, object_name) {
  xrng <- diff(xrange(plot))
  object <- unclass(object)
  object$geom <- GeomVwXspline
  object$params <- utils::modifyList(object$params, list(unit  = "npc",
                                                         scale = xrng))
  object <- do.call(layer, object)
  ggplot_add(object, plot, object_name)
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add geom_vwxspline_v
#' @export
ggplot_add.geom_vwxspline_v <- function(object, plot, object_name) {
  yrng <- diff(yrange(plot))
  object <- unclass(object)
  object$geom <- GeomVwXspline
  object$params <- utils::modifyList(object$params, list(unit  = "npc",
                                                         scale = yrng))
  object <- do.call(layer, object)
  ggplot_add(object, plot, object_name)
}

