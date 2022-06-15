#' @title  Variable-width horizontal diagonal curve
#' @description  A diagonal is a bezier curve where the control points are moved
#' perpendicularly towards the center in either the x or y direction a fixed
#' amount. And this function allows each curve to have a different width.
#' @inheritParams ggforce::geom_diagonal
#' @importFrom ggforce StatDiagonal
#' @rdname geom_vwdiagonal
#' @export
#' @examples
#' library(ggplot2)
#' dd <- data.frame(x = 1,
#'                  y = 1:10,
#'                  xend = 2:11,
#'                  yend = 3:12)
#' ggplot(dd, aes(x, y, xend = xend, yend = yend)) + geom_vwdiagonal()
geom_vwdiagonal <- function(mapping = NULL,
                            data = NULL,
                            stat = "vwdiagonal",
                            position = "identity",
                            n = 100,
                            na.rm = FALSE,
                            strength = 0.5,
                            show.legend = NA,
                            inherit.aes = TRUE,
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
    geom = GeomVwcurve,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(
      list(
        na.rm = na.rm,
        n = n,
        strength = strength),
      params
    )
  )
}

#' @rdname geom_vwdiagonal
#' @export
stat_vwdiagonal <- function(mapping = NULL,
                            data = NULL,
                            geom = "vwcurve",
                            position = "identity",
                            n = 100,
                            strength = 0.5,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            ...) {
  layer(
    stat = StatVwdiagonal,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      n = n,
      strength = strength,
      ...)
  )
}

#' @rdname ggvwline-extensions
#' @export
StatVwdiagonal <- ggproto("StatVwdiagonal", StatDiagonal,
                        required_aes = c("x", "y", "xend", "yend"),
                        extra_params = c("na.rm", "n", "strength")
)

