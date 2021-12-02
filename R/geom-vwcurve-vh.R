#' @rdname geom_vwcurve
#' @export
geom_vwcurve_h <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          open = TRUE,
                          angle = "perp",
                          lineend = "butt",
                          mitrelimit = 4,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  structure(.Data = list(data = data,
                         mapping = mapping,
                         stat = stat,
                         position = position,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         params = list(open = open,
                                       angle = angle,
                                       lineend = lineend,
                                       mitrelimit = mitrelimit,
                                       na.rm = na.rm,
                                       ...)
  ),
  class = "geom_vwcurve_h")
}

#' @rdname geom_vwcurve
#' @export
geom_vwcurve_v <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          open = TRUE,
                          angle = "perp",
                          lineend = "butt",
                          mitrelimit = 4,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  structure(.Data = list(data = data,
                         mapping = mapping,
                         stat = stat,
                         position = position,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         params = list(open = open,
                                       angle = angle,
                                       lineend = lineend,
                                       mitrelimit = mitrelimit,
                                       na.rm = na.rm,
                                       ...)
  ),
  class = "geom_vwcurve_v")
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add geom_vwcurve_h
#' @export
ggplot_add.geom_vwcurve_h <- function(object, plot, object_name) {
  xrng <- diff(xrange(plot))
  object <- unclass(object)
  object$geom <- GeomVwcurve
  object$params <- utils::modifyList(object$params, list(unit  = "npc",
                                                         scale = xrng))
  object <- do.call(layer, object)
  ggplot_add(object, plot, object_name)
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add geom_vwcurve_v
#' @export
ggplot_add.geom_vwcurve_v <- function(object, plot, object_name) {
  yrng <- diff(yrange(plot))
  object <- unclass(object)
  object$geom <- GeomVwcurve
  object$params <- utils::modifyList(object$params, list(unit  = "npc",
                                                         scale = yrng))
  object <- do.call(layer, object)
  ggplot_add(object, plot, object_name)
}

