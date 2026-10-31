#' The numbers behind a square donut
#'
#' Computes the whole donut, the hole and the dough for a numeric vector, with
#' the side lengths used to draw them. The dough is computed from squared
#' deviations about the mean (two passes over the data), so it stays accurate
#' even when the mean is very large relative to the spread.
#'
#' On the sum scale the areas are `sum(x^2)`, `n * mean^2` and
#' `sum((x - mean)^2)`. On the mean scale each area is divided by `n`, giving
#' `mean(x^2)`, `mean^2` and the population variance.
#'
#' `kappa` is the condition number of Chan, Golub and LeVeque (1983, eq. 2.1):
#' `kappa^2 = whole / dough`. The shortcut formula `sum(x^2) - n * mean^2`
#' loses accuracy roughly as `n * kappa^2 * .Machine$double.eps` grows.
#' `near_boundary` flags data where that bound exceeds `boundary_tol`. It is
#' informational only: this function never uses the shortcut formula.
#'
#' @param x A non-empty numeric vector with no missing or infinite values.
#' @param scale `"sum"` (default) for the sum-of-squares donut, or `"mean"`
#'   for the per-observation donut.
#' @param boundary_tol Relative-error threshold for `near_boundary`.
#'
#' @return A list of class `donut_stats` with elements `n`, `mean`, `E_x2`
#'   (mean of `x^2`), `sum_x2`, `variance` (population, divisor `n`), `ss_dev`
#'   (the dough on the sum scale), `side_outer`, `side_inner` and `side_dough`
#'   (side lengths on the chosen scale), `kappa`, `kappa2`, `shortcut_bound`
#'   and `near_boundary`. Printing it shows a short summary.
#'
#' @references Chan, T. F., Golub, G. H. and LeVeque, R. J. (1983).
#'   Algorithms for computing the sample variance: analysis and
#'   recommendations. *The American Statistician*, 37(3), 242--247.
#'
#' @examples
#' st <- donut_stats(c(1, 2, 3, 5, 9))
#' st
#' st$ss_dev            # the dough: 40
#' st$ss_dev / st$n     # population variance: 8
#' @export
donut_stats <- function(x, scale = c("sum", "mean"), boundary_tol = 1e-3) {
  scale <- match.arg(scale)
  if (!is.numeric(x) || length(x) == 0)
    stop("`x` must be a non-empty numeric vector.")
  if (any(!is.finite(x)))
    stop("`x` contains NA, NaN or infinite values; remove them first.")
  n <- length(x); mu <- mean(x)
  var_pop <- mean((x - mu)^2)            # dough per observation
  ex2 <- mu^2 + var_pop                  # mean of x^2, without cancellation
  k <- if (scale == "mean") 1 else sqrt(n)
  kappa2 <- if (var_pop > 0) ex2 / var_pop else if (ex2 > 0) Inf else 1
  bound <- n * kappa2 * .Machine$double.eps
  out <- list(n = n, mean = mu, E_x2 = ex2, sum_x2 = n * ex2,
              variance = var_pop, ss_dev = n * var_pop,
              side_outer = sqrt(ex2) * k, side_inner = abs(mu) * k,
              side_dough = sqrt(var_pop) * k,
              kappa = sqrt(kappa2), kappa2 = kappa2,
              shortcut_bound = bound, near_boundary = bound > boundary_tol)
  attr(out, "scale") <- scale
  class(out) <- c("donut_stats", "list")
  out
}

#' @export
print.donut_stats <- function(x, ...) {
  scale <- attr(x, "scale")
  if (is.null(scale)) scale <- "sum"
  lb <- lab_helper(x, scale)
  cat("Square donut, ", scale, " scale, n = ", x$n, "\n", sep = "")
  cat("  whole = hole + dough:  ", fmt_num(lb$area_out), " = ",
      fmt_num(lb$area_hol), " + ", fmt_num(lb$area_dgh), "\n", sep = "")
  cat("  population variance:   ", fmt_num(x$ss_dev), " / ", x$n, " = ",
      fmt_num(x$variance), "\n", sep = "")
  if (x$n > 1)
    cat("  sample variance:       ", fmt_num(x$ss_dev), " / ", x$n - 1, " = ",
        fmt_num(x$ss_dev / (x$n - 1)), "\n", sep = "")
  if (isTRUE(x$near_boundary))
    cat("  Glaze-thin: the shortcut formula sum(x^2) - n*mean^2 could lose",
        "accuracy here.\n")
  invisible(x)
}
