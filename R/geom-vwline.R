#' @title Variable-Width Line Layer
#' @description Layer function to draw variable-width line.
#' @param w width of variable-width line, should be created with \code{widthSpec}.
#' @param open a boolean indicating whether to connect the last location back
#' to the first location to produce a closed line.
#' @param linejoin the line join style; one of "round", "mitre", or "bevel"..
#' @param lineend the line ending style; one of "round", "mitre", "butt",
#' "square", or "extend".
#' @param mitrelimit a numeric that controls when a mitre join is converted to
#' a bevel join or a mitre ending is converted to a square ending.
#' @param stepWidth a logical indicating whether widths are fixed along the
#' length of a segment.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#' \code{geom_vwline()} understands the following aesthetics (required aesthetics are in bold):
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
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom grid gpar
#' @importFrom vwline vwlineGrob
#' @importFrom vwline widthSpec
#' @rdname geom_vwline
#' @export
geom_vwline <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        position = "identity",
                        ...,
                        w = NULL,
                        open = TRUE,
                        linejoin = "round",
                        lineend = "butt",
                        mitrelimit = 4,
                        stepWidth = FALSE,
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
    geom = GeomVwline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(list(
      w = w,
      open = open,
      linejoin = linejoin,
      lineend = lineend,
      mitrelimit = mitrelimit,
      stepWidth = stepWidth,
      na.rm = na.rm
    ), params)
  )
}

#' @rdname geom_vwline
#' @format NULL
#' @usage NULL
#' @export
GeomVwline <- ggproto(
  "GeomVwline", Geom,
  default_aes = aes(width    = 5,
                    left     = NULL,
                    right    = NULL,
                    alpha    = NA,
                    colour   = "grey35",
                    fill     = "grey60",
                    linetype = 1,
                    size     = 0.5),
  required_aes = c("x", "y"),

  draw_panel = function(self, data, panel_params, coord, w = NULL, open = TRUE,
                        linejoin = "round", lineend = "butt", mitrelimit = 4,
                        stepWidth = FALSE, unit = "mm", scale = 1, na.rm = FALSE) {
    if(empty(data) || nrow(data) < 2) {
      return(ggplot2::zeroGrob())
    }

    if(!is.null(w)) {
      if(!inherits(w, "widthSpec")) {
        stop("'w' should be created with `widthSpec()`.", call. = FALSE)
      }
      width <- rescale_width(w, scale = scale)
    } else if(!is.null(data$left) && !is.null(data$right)) {
      width <- widthSpec(list(left = data$left / scale,
                              right = data$right / scale), unit)
    } else {
      width <- widthSpec(data$width / scale, unit)
    }

    coords <- coord$transform(data, panel_params)
    first_row <- coords[1, , drop = FALSE]

    ggname(
      "geom_vwline",
      vwline::vwlineGrob(x             = coords$x,
                         y             = coords$y,
                         w             = width,
                         default.units = "native",
                         open          = open,
                         linejoin      = linejoin,
                         lineend       = lineend,
                         mitrelimit    = mitrelimit,
                         stepWidth     = stepWidth,
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
