#' @title Variable-Width Curve Layer
#' @description Layer function draw a variable-width line.
#' @param open a boolean indicating whether to connect the last location back
#' to the first location to produce a closed line.
#' @param angle either "perp" or a numeric value describing a fixed orientation
#' for the line width.
#' @param lineend the line ending style; one of "round", "mitre", "butt",
#' "square", or "extend".
#' @param mitrelimit a numeric that controls when a mitre join is converted to
#' a bevel join or a mitre ending is converted to a square ending.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#' \code{geom_vwcurve()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{width}
#'       \item \code{left}
#'       \item \code{right}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'   }
#' @importFrom vwline vwcurveGrob
#' @importFrom grid unit
#' @rdname geom_vwcurve
#' @export
geom_vwcurve <- function(mapping = NULL,
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
  params <- list(...)
  if(length(params) > 0) {
    params <- params[setdiff(names(params), c("unit", "scale"))]
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomVwcurve,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(list(
      open = open,
      angle = angle,
      lineend = lineend,
      mitrelimit = mitrelimit,
      na.rm = na.rm
    ), params)
  )
}

#' @rdname geom_vwcurve
#' @format NULL
#' @usage NULL
#' @export
GeomVwcurve <- ggproto(
  "GeomVwcurve", Geom,
  default_aes = aes(width    = 5,
                    alpha    = NA,
                    colour   = "grey35",
                    fill     = "grey60",
                    linetype = 1,
                    size     = 0.5),
  required_aes = c("x", "y"),

  draw_panel = function(self, data, panel_params, coord, open = TRUE,
                        angle = "perp", lineend = "butt", mitrelimit = 4,
                        unit = "mm", scale = 1, na.rm = FALSE) {
    if(empty(data) || nrow(data) < 2) {
      return(ggplot2::zeroGrob())
    }

    width <- unit(data$width / scale, units = unit)

    coords <- coord$transform(data, panel_params)
    first_row <- coords[1, , drop = FALSE]

    ggname(
      "geom_vwcurve",
      vwline::vwcurveGrob(x            = coords$x,
                          y             = coords$y,
                          w             = width,
                          default.units = "native",
                          open          = open,
                          angle         = angle,
                          lineend       = lineend,
                          mitrelimit    = mitrelimit,
                          gp            = gpar(col  = scales::alpha(first_row$colour,
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
