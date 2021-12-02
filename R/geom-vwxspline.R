#' @title Variable-Width X-spline Curve Layer
#' @description Layer function to draw variable-width X-spline curve.
#' @param shape a numeric value (or one per location) that controls the shape
#' of the X-spline curve relative to the locations.
#' @param open a boolean indicating whether to connect the last location back
#' to the first location to produce a closed line.
#' @param repEnds a logical indicating whether to replicate the first and last
#' control points (so that the X-spline starts and ends at the first and last
#' control points).
#' @param angle either "perp" or a numeric value describing a fixed orientation
#' for the line width.
#' @param lineend the line ending style; one of "round", "mitre", "butt",
#' "square", or "extend".
#' @param mitrelimit a numeric that controls when a mitre join is converted to
#' a bevel join or a mitre ending is converted to a square ending.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#' \code{geom_vwxspline()} understands the following aesthetics (required aesthetics are in bold):
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
#' @importFrom vwline vwXsplineGrob
#' @rdname geom_vwxspline
#' @export
geom_vwxspline <- function(mapping = NULL,
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
  params <- list(...)
  if(length(params) > 0) {
    params <- params[setdiff(names(params), c("unit", "scale"))]
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomVwXspline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(list(
      shape = shape,
      open = open,
      repEnds = repEnds,
      angle = angle,
      lineend = lineend,
      mitrelimit = mitrelimit,
      na.rm = na.rm
    ), params)
  )
}

#' @rdname geom_vwxspline
#' @format NULL
#' @usage NULL
#' @export
GeomVwXspline <- ggproto(
  "GeomVwXspline", Geom,
  default_aes = aes(width    = 5,
                    alpha    = NA,
                    colour   = "grey35",
                    fill     = "grey60",
                    linetype = 1,
                    size     = 0.5),
  required_aes = c("x", "y"),

  draw_panel = function(self, data, panel_params, coord, shape = 1, open = TRUE,
                        repEnds = TRUE, angle = "perp", lineend = "butt",
                        mitrelimit = 4, unit = "mm",  scale = 1, na.rm = FALSE) {
    if(empty(data) || nrow(data) < 3) {
      return(ggplot2::zeroGrob())
    }

    width <- unit(data$width / scale, unit)

    coords <- coord$transform(data, panel_params)
    first_row <- coords[1, , drop = FALSE]

    ggname(
      "geom_vwxspline",
      vwline::vwXsplineGrob(x = coords$x,
                            y = coords$y,
                            w = width,
                            default.units = "native",
                            shape = shape,
                            open = open,
                            angle = angle,
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
