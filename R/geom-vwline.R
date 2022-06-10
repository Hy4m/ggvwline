#' @title Variable-Width Line Layer
#' @description Layer function to draw variable-width line.
#' @param width_units unit of width.
#' @param open a boolean indicating whether to connect the last location back
#' to the first location to produce a closed line.
#' @param linejoin the line join style; one of "round", "mitre", or "bevel"..
#' @param lineend the line ending style; one of "round", "mitre", "butt",
#' "square", or "extend".
#' @param mitrelimit a numeric that controls when a mitre join is converted to
#' a bevel join or a mitre ending is converted to a square ending.
#' @param stepWidth a logical indicating whether widths are fixed along the
#' length of a segment.
#' @param by_x logical, if TRUE will calculate the width based on x-axis.
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
                        width_units = "mm",
                        open = TRUE,
                        linejoin = "round",
                        lineend = "butt",
                        mitrelimit = 4,
                        stepWidth = FALSE,
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
    geom = GeomVwline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(list(
      width_units = width_units,
      open = open,
      linejoin = linejoin,
      lineend = lineend,
      mitrelimit = mitrelimit,
      stepWidth = stepWidth,
      by_x = by_x,
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
                    left     = NA,
                    right    = NA,
                    alpha    = NA,
                    colour   = "grey35",
                    fill     = "grey60",
                    linetype = 1,
                    size     = 0.5),
  required_aes = c("x", "y"),

  draw_panel = function(self, data, panel_params, coord, w = NULL, open = TRUE,
                        linejoin = "round", lineend = "butt", mitrelimit = 4,
                        stepWidth = FALSE, width_units = "mm", by_x = FALSE,
                        na.rm = FALSE) {
    if(empty(data) || nrow(data) < 2) {
      return(ggplot2::zeroGrob())
    }

    if (is.null(w)) {
      if(!all(is.na(data$left)) && !all(is.na(data$left))) {
        w <- widthSpec(x = list(left = unit(data$left, width_units),
                                right = unit(data$right, width_units)))
      } else {
        w <- widthSpec(x = unit(data$width, width_units))
      }
    }
    if(!inherits(w, "widthSpec")) {
      stop("'width' should be created with `widthSpec()`.", call. = FALSE)
    }
    if (isTRUE(by_x)) {
      width <- reset_width(w, diff(panel_params$x.range))
    } else {
      width <- reset_width(w, diff(panel_params$y.range))
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
