#' @rdname geom_vwline
#' @export
geom_vwline_h <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          w = NULL,
                          open = TRUE,
                          linejoin = "round",
                          lineend = "butt",
                          mitrelimit = 4,
                          stepWidth = FALSE,
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
                                       open = open,
                                       linejoin = linejoin,
                                       lineend = lineend,
                                       mitrelimit = mitrelimit,
                                       stepWidth = stepWidth,
                                       na.rm = na.rm,
                                       ...)
                         ),
            class = "geom_vwline_h")
}

#' @rdname geom_vwline
#' @export
geom_vwline_v <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          w = NULL,
                          open=TRUE,
                          linejoin = "round",
                          lineend = "butt",
                          mitrelimit = 4,
                          stepWidth = FALSE,
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
                                       open = open,
                                       linejoin = linejoin,
                                       lineend = lineend,
                                       mitrelimit = mitrelimit,
                                       stepWidth = stepWidth,
                                       na.rm = na.rm,
                                       ...)
                         ),
            class = "geom_vwline_v")
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add geom_vwline_h
#' @export
ggplot_add.geom_vwline_h <- function(object, plot, object_name) {
  xrng <- diff(xrange(plot))
  object <- unclass(object)
  object$geom <- GeomVwline
  object$params <- utils::modifyList(object$params, list(unit  = "npc",
                                                         scale = xrng))
  object <- do.call(layer, object)
  ggplot_add(object, plot, object_name)
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add geom_vwline_v
#' @export
ggplot_add.geom_vwline_v <- function(object, plot, object_name) {
  yrng <- diff(yrange(plot))
  object <- unclass(object)
  object$geom <- GeomVwline
  object$params <- utils::modifyList(object$params, list(unit  = "npc",
                                                         scale = yrng))
  object <- do.call(layer, object)
  ggplot_add(object, plot, object_name)
}

