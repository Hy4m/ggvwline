#' @title  Draw acr variable-width curves
#' @description  This function can be used to draw arc variable-width curves.
#' @inheritParams ggforce::geom_arc
#' @importFrom ggforce StatArc
#' @rdname geom_vwarc
#' @export
geom_vwarc <- function(mapping = NULL,
                       data = NULL,
                       stat = "vwarc",
                       position = "identity",
                       n = 360,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       ...) {
  params <- list(...)
  if ("geom" %in% names(params)) {
    geom <- params$geom
    params <- params[setdiff(names(params), "geom")]
  } else {
    geom = GeomVwcurve
  }
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      na.rm = na.rm,
      ...)
  )
}
#' @rdname ggvwline-extensions
#' @format NULL
#' @usage NULL
#' @export
StatVwarc <- ggproto("StatVwarc", StatArc,
                   required_aes = c("x0", "y0", "r", "start", "end")
)
#' @rdname geom_vwarc
#' @export
stat_vwarc <- function(mapping = NULL,
                       data = NULL,
                       geom = "vwline",
                       position = "identity",
                       na.rm = FALSE,
                       show.legend = NA,
                       n = 360,
                       inherit.aes = TRUE,
                       ...) {
  layer(
    stat = StatVwarc,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      n = n,
      ...)
  )
}

