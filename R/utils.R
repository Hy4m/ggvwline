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
`%||%` <- function(a, b) {
  if(is.null(a)) b else a
}

#' @noRd
rename <- function (data, ...)
{
  ll <- list(...)
  if (length(ll) == 0) {
    data
  }
  else {
    old <- unname(unlist(ll))
    new <- names(ll)
    names(data)[names(data) %in% old] <- new
  }
  data
}

#' @noRd
unit_value <- function(x) {
  vapply(unclass(x), `[[`, numeric(1), 1)
}

#' @noRd
reset_width <- function(width, range) {
  if (inherits(width, "widthSpline")) {
    lv <- unit_value(width$left)
    lr <- unit_value(width$right)
    lu <- grid::unitType(width$left)
    ru <- grid::unitType(width$right)
    lv[which(lu) == "native"] <- lv[which(lu) == "native"] / range
    rv[which(ru) == "native"] <- rv[which(ru) == "native"] / range
    width <- widthSpec(list(left = unit(lv, lu),
                            right = unit(rv, ru)))
  } else if (inherits(width, "BezierWidth") || inherits(width, "widthSpec")) {
    v <- unit_value(width$w)
    u <- grid::unitType(width$w)
    v[which(u) == "native"] <- v[which(u) == "native"] / range
    width$w <- unit(v, u)
    if (grid::is.unit(width$d)) {
      dv <- unit_value(width$d)
      du <- grid::unitType(width$d)
      dv[which(du) == "native"] <- dv[which(du) == "native"] / range
      width$d <- dv
    }
  } else {
    v <- unit_value(width)
    u <- grid::unitType(width)
    v[which(u) == "native"] <- v[which(u) == "native"] / range
    width <- unit(v, u)
  }
  width
}
