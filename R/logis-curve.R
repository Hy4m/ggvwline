#' @title Draw Logistic Curve
#' These functions create and draw logistic curve.
#' @param x a numeric vector specifying the x-location of the start point.
#' @param y a numeric vector specifying the y-location of the start point.
#' @param xend a numeric vector specifying the x-location of the end point.
#' @param yend a numeric vector specifying the y-location of the end point.
#' @param a a numeric vector specifying the center point.
#' @param b a numeric vector specifying the growth rate.
#' @param k a numeric vector specifying the initial value.
#' @param n positive integer value.
#' @param ... others parameters.
#' @importFrom grid linesGrob
#' @importFrom grid grid.draw
#' @importFrom tibble tibble
#' @rdname logisCurveGrob
#' @export
logisCurveGrob <- function(x = 0,
                           y = 0,
                           xend = 1,
                           yend = 1,
                           a = 0,
                           b = 0.05,
                           k = 20,
                           n = 200,
                           ...) {
  data <- logis_curve_gen(x = x,
                          y = y,
                          xend = xend,
                          yend = yend,
                          a = a,
                          b = b,
                          k = k,
                          n = n)
  grid::linesGrob(x = data$x,
                  y = data$y,
                  ...)
}

#' @rdname logisCurveGrob
#' @export
grid.logis_curve <- function(...) {
  grid::grid.draw(logisCurveGrob(...))
}

#' @noRd
logis_curve_gen <- function(x = 0,
                            y = 0,
                            xend = 1,
                            yend = 1,
                            a = 0,
                            b = 0.05,
                            k = 20,
                            n = 200) {
  if(n %% 2 != 0) n <- n + 1
  xx <- seq(-n / 2, n / 2)
  yy <- k / (1 + exp(a - b * xx))
  tibble::tibble(x = scales::rescale(xx, to = c(x, xend)),
                 y = scales::rescale(yy, to = c(y, yend)))
}
