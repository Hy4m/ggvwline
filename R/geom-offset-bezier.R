#' @title Bezier Offset Curve Layer
#' @description Layer function to draw a bezier offset curve.
#' @param width_units unit of width.
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
#' @param by_x logical, if TRUE will calculate the width based on x-axis.
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
#' @importFrom grid gList
#' @rdname geom_offset_bezier
#' @export
geom_offset_bezier <- function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               ...,
                               width_units = "mm",
                               stepFn = nSteps(100),
                               open = TRUE,
                               lineend = "butt",
                               linejoin = "round",
                               mitrelimit = 4,
                               d = NULL,
                               rep = FALSE,
                               by_x = FALSE,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  params <- list(...)
  if("width" %in% names(params)) {
    params <- rename(params, w = "width")
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
      width_units = width_units,
      stepFn = stepFn,
      open = open,
      lineend = lineend,
      linejoin = linejoin,
      mitrelimit = mitrelimit,
      d = d,
      rep = rep,
      by_x = by_x,
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

  draw_panel = function(self, data, panel_params, coord, w = NULL, shape = 1,
                        stepFn = nSteps(100), open = TRUE, lineend = "butt",
                        linejoin = "round", mitrelimit = 4, width_units = "mm",
                        d = NULL, rep = FALSE, by_x = FALSE, na.rm = FALSE) {
    groups <- split(data, factor(data$group))
    grobs <- lapply(groups, function(group) {
      self$draw_group(group, panel_params, coord, w = w, shape = shape,
                      stepFn = stepFn, open = open, lineend = lineend,
                      linejoin = linejoin, mitrelimit = mitrelimit,
                      width_units = width_units, d = d, rep = rep, by_x = by_x,
                      na.rm = na.rm)
    })

    ggname("geom_offset_bezier", grid::gTree(
      children = do.call("gList", grobs)
    ))
  },

  draw_group = function(self, data, panel_params, coord, w = NULL, shape = 1,
                        stepFn = nSteps(100), open = TRUE, lineend = "butt",
                        linejoin = "round", mitrelimit = 4, width_units = "mm",
                        d = NULL, rep = FALSE, by_x = FALSE, na.rm = FALSE) {
    if(empty(data) || nrow(data) < 4) {
      return(ggplot2::zeroGrob())
    }
    if (is.null(w)) {
      w <- widthSpline(data$width, default.units = width_units, shape = shape,
                       d = d, rep = rep)
    } else {
      if (is.numeric(w)) {
        w <- widthSpline(rep_len(w, nrow(data)), default.units = width_units,
                         shape = shape, d = d, rep = rep)
      }
    }
    if(!inherits(w, "widthSpline") && !inherits(w, "BezierWidth")) {
      stop("'width' should be created with `widthSpline()` or `BezierWidth()`.",
           call. = FALSE)
    }
    if (isTRUE(by_x)) {
      width <- reset_width(w, diff(panel_params$x.range))
    } else {
      width <- reset_width(w, diff(panel_params$y.range))
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
