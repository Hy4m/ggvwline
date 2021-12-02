#' @title X-spline Offset Curve Layer
#' @description Layer funtion to draw an X-spline offset curve.
#' @param w width of curve, should be created with \code{widthSpline()}.
#' @param shape a numeric value (or one per location) that controls the shape
#' of the X-spline curve relative to the locations.
#' @param open a boolean indicating whether to connect the last location back
#' to the first location to produce a closed line.
#' @param repEnds a logical indicating whether to replicate the first and last
#' control points (so that the X-spline starts and ends at the first and last
#' control points).
#' @param lineend the line ending style; one of "round", "mitre", "butt",
#' "square", or "extend".
#' @param mitrelimit a numeric that controls when a mitre join is converted to
#' a bevel join or a mitre ending is converted to a square ending.
#' @param d,wshape,rep passing to \code{widthSpline}.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#' \code{geom_offset_xspline()} understands the following aesthetics (required aesthetics are in bold):
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
#' @importFrom vwline offsetXsplineGrob
#' @importFrom vwline widthSpline
#' @rdname geom_offset_xspline
#' @export
geom_offset_xspline <- function(mapping = NULL,
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
  params <- list(...)
  if(length(params) > 0) {
    params <- params[setdiff(names(params), c("unit", "scale"))]
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomOffsetXspline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(list(
      w = w,
      shape = shape,
      open = open,
      repEnds = repEnds,
      lineend = lineend,
      mitrelimit = mitrelimit,
      d = d,
      wshape = wshape,
      rep = rep,
      na.rm = na.rm
    ), params)
  )
}

#' @rdname geom_offset_xspline
#' @format NULL
#' @usage NULL
#' @export
GeomOffsetXspline <- ggproto(
  "GeomOffsetXspline", Geom,
  default_aes = aes(width    = 5,
                    alpha    = NA,
                    colour   = "grey35",
                    fill     = "grey60",
                    linetype = 1,
                    size     = 0.5),
  required_aes = c("x", "y"),

  draw_panel = function(self, data, panel_params, coord, w = NULL, shape = 1,
                        open = TRUE, repEnds = TRUE, lineend = "butt", mitrelimit = 4,
                        unit = "mm", d = NULL, wshape = -1, rep = FALSE,
                        scale = 1, na.rm = FALSE) {
    if(empty(data) || nrow(data) < 2) {
      return(ggplot2::zeroGrob())
    }

    if(!is.null(w)) {
      if(!inherits(w, "widthSpline")) {
        stop("'w' should be created with `widthSpline()`.", call. = FALSE)
      }
      width <- rescale_width(w, scale = scale)
    } else {
      width <- widthSpline(data$width / scale, unit, d = d,
                           shape = wshape, rep = rep)
    }

    coords <- coord$transform(data, panel_params)
    first_row <- coords[1, , drop = FALSE]

    ggname(
      "geom_offset_xspline",
      vwline::offsetXsplineGrob(x = coords$x,
                                y = coords$y,
                                w = width,
                                default.units = "native",
                                shape = shape,
                                open = open,
                                repEnds = repEnds,
                                lineend = lineend,
                                mitrelimit = mitrelimit,
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
