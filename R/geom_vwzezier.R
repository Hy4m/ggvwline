#' @title  Draw variable-width bezier curves
#' @description  This function can be used to draw variable-width bezier curves.
#' @inheritParams ggforce::geom_bezier
#' @importFrom ggforce StatBezier
#' @rdname geom_vwbezier
#' @export
geom_vwbezier <- function(mapping = NULL,
                          data = NULL,
                          stat = "vwbezier",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          n = 100,
                          ...) {
  params <- list(...)
  if ("geom" %in% names(params)) {
    geom <- params$geom
    params <- params[setdiff(names(params), "geom")]
  } else {
    geom <- GeomVwcurve
  }
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(
      list(
        na.rm = na.rm,
        n = n),
      params
    )
  )
}
#' @rdname ggvwline-extensions
#' @format NULL
#' @usage NULL
#' @export
StatVwbezier <- ggproto("StatVwbezier", StatBezier,
                      required_aes = c("x", "y"),
                      extra_params = c("na.rm", "n")
)
#' @rdname geom_vwbezier
#' @export
stat_bezier <- function(mapping = NULL,
                        data = NULL,
                        geom = "vwcurve",
                        position = "identity",
                        na.rm = FALSE,
                        show.legend = NA,
                        n = 100,
                        inherit.aes = TRUE,
                        ...) {
  layer(
    stat = StatVwbezier,
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
