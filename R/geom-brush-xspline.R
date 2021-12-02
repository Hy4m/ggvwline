#' @title Brush X-spline Curve Layer
#' @description Layer function to sweep an X-spline curve with a brush.
#' @param w width of X-spline, should be created with \code{widthSpline} or
#' \code{BezierWidth()}.
#' @param brush a description of a brush shape.
#' @param shape a numeric value (or one per location) that controls the shape
#' of the X-spline curve relative to the locations.
#' @param angle either "perp" or a numeric value describing a fixed orientation
#' for the line width.
#' @param open a boolean indicating whether to connect the last location back to
#' the first location to produce a closed line.
#' @param spacing a numeric vector or unit describing where the brush will be
#' placed along the main curve.
#' @param d,rep passing to \code{widthSpline}.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#' \code{geom_brush_xspline()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{width}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'   }
#' @importFrom vwline brushXsplineGrob
#' @importFrom vwline verticalBrush
#' @rdname geom_brush_xspline
#' @export
geom_brush_xspline <- function(mapping = NULL,
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
  params <- list(...)
  if(length(params) > 0) {
    params <- params[setdiff(names(params), c("unit", "scale"))]
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBrushXspline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(list(
      w = w,
      brush = brush,
      shape = shape,
      angle = angle,
      open = open,
      spacing = spacing,
      d = d,
      rep = rep,
      na.rm = na.rm
    ), params)
  )
}

#' @rdname geom_brush_xspline
#' @format NULL
#' @usage NULL
#' @export
GeomBrushXspline <- ggproto(
  "GeomBrushXspline", Geom,
  default_aes = aes(width    = 5,
                    alpha    = NA,
                    colour   = "grey35",
                    fill     = "grey60",
                    linetype = 1,
                    size     = 0.5),
  required_aes = c("x", "y"),

  draw_panel = function(self, data, panel_params, coord, w = NULL,
                        brush = verticalBrush, shape = 1, angle = "perp",
                        open = TRUE, spacing = NULL, d = NULL, rep = FALSE,
                        unit = "mm", scale = 1, na.rm = FALSE) {
    if(empty(data) || nrow(data) < 2) {
      return(ggplot2::zeroGrob())
    }

    if(!is.null(w)) {
      if(!inherits(w, "widthSpline") && inherits(w, "BezierWidth")) {
        stop("'w' should be created with `widthSpline()` or `BezierWidth()`.", call. = FALSE)
      }
      width <- rescale_width(w, scale = scale)
    } else {
      width <- widthSpline(data$width / scale, unit, d = d, rep = rep)
    }

    coords <- coord$transform(data, panel_params)
    first_row <- coords[1, , drop = FALSE]

    ggname(
      "geom_brush_xspline",
      vwline::brushXsplineGrob(x = coords$x,
                               y = coords$y,
                               w = width,
                               default.units = "native",
                               brush = brush,
                               shape = shape,
                               angle = angle,
                               open = open,
                               spacing = spacing,
                               gp = gpar(col  = scales::alpha(first_row$colour,
                                                              first_row$alpha),
                                         fill = scales::alpha(first_row$fill,
                                                              first_row$alpha),
                                         lty  = first_row$linetype,
                                         size = first_row$size * ggplot2::.pt)
      )
    )
  },
  draw_key = draw_key_polygon
)
