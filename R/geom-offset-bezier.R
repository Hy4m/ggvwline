#' @title Bezier Offset Curve Layer
#' @description Layer function to draw a bezier offset curve.
#' @param w width of Bezier-offset curve, should be created with
#' \code{widthSpline} or \code{BezierWidth()}.
#' @param stepFn function called to generate steps in t when rendering.
#' @param open a boolean indicating whether to connect the last location back
#' to the first location to produce a closed line.
#' @param lineend the line ending style; one of "round", "mitre", "butt",
#' "square", or "extend".
#' @param linejoin the line join style; one of "round", "mitre", "bevel",
#' or "extend".
#' @param mitrelimit a numeric that controls when a mitre join is converted to
#' a bevel join or a mitre ending is converted to a square ending.
#' @param d,rep passing to \code{widthSpline}.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#' \code{geom_offset_bezier()} understands the following aesthetics (required aesthetics are in bold):
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
#' @importFrom vwline offsetBezierGrob
#' @importFrom vwline BezierWidth
#' @importFrom gridBezier nSteps
#' @rdname geom_offset_bezier
#' @export
geom_offset_bezier <- function(mapping = NULL,
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
  params <- list(...)
  if(length(params) > 0) {
    params <- params[setdiff(names(params), c("unit", "scale"))]
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomOffsetBezier,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(list(
      w = w,
      stepFn = stepFn,
      open = open,
      lineend = lineend,
      linejoin = linejoin,
      mitrelimit = mitrelimit,
      d = d,
      rep = rep,
      na.rm = na.rm
    ), params)
  )
}

#' @rdname geom_offset_bezier
#' @format NULL
#' @usage NULL
#' @export
GeomOffsetBezier <- ggproto(
  "GeomOffsetBezier", Geom,
  default_aes = aes(width    = 5,
                    alpha    = NA,
                    colour   = "grey35",
                    fill     = "grey60",
                    linetype = 1,
                    size     = 0.5),
  required_aes = c("x", "y"),

  draw_panel = function(self, data, panel_params, coord, w = NULL,
                        stepFn = nSteps(100), open = TRUE, lineend = "butt",
                        linejoin = "round", mitrelimit = 4, unit = "mm",
                        d = NULL, rep = FALSE, scale = 1, na.rm = FALSE) {
    if(empty(data) || nrow(data) < 4) {
      return(ggplot2::zeroGrob())
    }

    if(!is.null(w)) {
      if(!inherits(w, "widthSpline") || !inherits(w, "BezierWidth")) {
        stop("'w' should be created with `widthSpline()` or `BezierWidth()`.",
             call. = FALSE)
      }
      width <- rescale_width(w, scale = scale)
    } else {
      width <- widthSpline(data$width / scale, unit, d = d, rep = rep)
    }

    coords <- coord$transform(data, panel_params)
    first_row <- coords[1, , drop = FALSE]

    ggname(
      "geom_offset_bezier",
      vwline::offsetBezierGrob(x = coords$x,
                               y = coords$y,
                               w = width,
                               default.units = "native",
                               stepFn = stepFn,
                               open = open,
                               lineend = lineend,
                               linejoin = linejoin,
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
