#' @noRd
ggname <- function (prefix, grob)
{
  grob$name <- grid::grobName(grob, prefix)
  grob
}

#' @noRd
any_is_na <- function(x) any(is.na(x))

#' @noRd
empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}

#' @noRd
rescale_width <- function(w, scale = 1) {
  clss <- class(w)
  if("widthSpec" %in% clss) {
    w <- lapply(w, "/", scale)
  } else{
    w$w <- w$w / scale
  }
  class(w) <- clss
  w
}

#' @noRd
`%||%` <- function(a, b) {
  if(is.null(a)) b else a
}

#' @importFrom ggplot2 ggplot_build
#' @noRd
xrange <- function(.plot) {
  ggplot_build(.plot)$layout$panel_params[[1]]$x.range
}

#' @noRd
yrange <- function(.plot) {
  ggplot_build(.plot)$layout$panel_params[[1]]$y.range
}
