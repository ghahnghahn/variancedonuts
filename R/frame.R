# A square of dough at (x0, y0) with side s; `inner` is the side of the hole
# drawn on top (0 if solid). A stripe is a ring through the middle third of the
# dough. Sprinkles are one dotted row along the middle of the dough. The crust
# edge is drawn last so nothing covers it. Only donuts with a hole are
# decorated: a Berliner (no hole) stays plain, because a ring on solid dough
# would look like a hole (user, 2026-10-17).
# `ref` is the side of the largest donut in the picture. Sprinkle dots have a
# fixed size in mm, so they are skipped when the dough band is under 6% of
# `ref`: on thin or tiny dough they would overflow and make it look bigger.
dough_square <- function(p, x0, y0, s, inner, sch, filled = TRUE, ref = s) {
  sq <- function(p, a, fill, colour, lw)
    p + annotate("rect", xmin = x0 + a, xmax = x0 + s - a, ymin = y0 + a,
                 ymax = y0 + s - a, fill = fill, color = colour,
                 linewidth = lw)
  edge <- if (is.na(sch$edge)) NA else sch$edge
  if (filled) {
    p <- sq(p, 0, sch$fill, NA, 0)
    w <- (s - inner) / 2
    decorate <- inner > 1e-12 && w > 0
    if (decorate && !is.na(sch$stripe))
      p <- sq(sq(p, w / 3, sch$stripe, NA, 0), 2 * w / 3, sch$fill, NA, 0)
    if (decorate && isTRUE(!is.na(sch$sprinkles)) && w >= 0.06 * ref) {
      a <- w / 2
      p <- p + annotate("path", x = x0 + c(a, s - a, s - a, a, a),
                        y = y0 + c(a, a, s - a, s - a, a),
                        colour = sch$sprinkles, linewidth = 2.2,
                        linetype = "13", lineend = "round")
    }
  }
  sq(p, 0, NA, if (filled || !is.na(edge)) edge else "grey40", 0.6)
}

#' Draw one square donut
#'
#' Draws the whole donut (area `sum(x^2)`), the hole (area `n * mean^2`) and
#' the dough between them (area `sum((x - mean)^2)`), all to scale. Beside it,
#' the same dough can be shown as a Berliner: a solid square with no hole.
#' Below it, the dough is
#' divided by `n` and by `n - 1` to give the two variances.
#'
#' @inheritParams donut_stats
#' @param dough_mode `TRUE` (default) fills the dough. `FALSE` draws outlines
#'   only, like a picture frame.
#' @param color Color scheme, by number or name. See [donut_schemes()].
#' @param fill_dough,edge_dough Optional colors that override the scheme's
#'   dough fill and crust outline.
#' @param show_sigma Draw a Berliner beside the donut: the same dough as a
#'   solid square with no hole. On the mean scale its side is the standard
#'   deviation.
#' @param divide_dough Sum scale only: add the line dividing the dough by `n`
#'   (population variance) and by `n - 1` (sample variance).
#' @param show_sides Label the side lengths of the whole and the hole.
#' @param show_identity Print the identity, for example
#'   "whole = hole + dough: 120 = 80 + 40", below the donut.
#'
#' @return A ggplot object.
#'
#' @section Saving:
#' The labels use Greek letters, which R's basic `pdf()` device cannot draw.
#' Save with `ggplot2::ggsave("donut.pdf", p, device = cairo_pdf)`, or as PNG.
#'
#' @examples
#' x <- c(1, 2, 3, 5, 9)
#' p <- donut_frame(x)
#' p_mean <- donut_frame(x, scale = "mean")
#' p_berliner <- donut_frame(x - 4)    # mean zero: a donut with no hole
#' if (interactive()) p
#' @export
donut_frame <- function(x, scale = c("sum", "mean"), dough_mode = TRUE,
                        color = 1, fill_dough = NULL, edge_dough = NULL,
                        show_sigma = TRUE, divide_dough = TRUE,
                        show_sides = TRUE, show_identity = FALSE) {
  scale <- match.arg(scale)
  sch <- resolve_scheme(color, fill_dough, edge_dough)
  st <- donut_stats(x, scale); lb <- lab_helper(st, scale)
  s <- st$side_outer; h <- st$side_inner; sig <- st$side_dough
  tsz <- 3.6; dg <- side_digits(s, h)
  p <- dough_square(ggplot(), 0, 0, s, h, sch, dough_mode)
  if (h > 1e-12) {                                   # the hole
    hc <- s / 2
    ib <- inner_border(sch)
    p <- p + annotate("rect", xmin = hc - h/2, xmax = hc + h/2,
                      ymin = hc - h/2, ymax = hc + h/2,
                      fill = "white", color = ib$colour, linewidth = ib$linewidth)
    if (h > 0.15 * s) {
      if (show_sides)
        p <- p + annotate("text", x = hc, y = hc + 0.03 * s, size = tsz,
                          color = "grey25", label = sprintf(
                          "side = %s%s = %.*f", RT, fmt_num(lb$area_hol),
                          dg, h))
      p <- p + annotate("text", x = hc, y = hc - (show_sides * 0.05) * s,
                        size = tsz, color = "grey25", label = sprintf(
                        "area = %s = %s", lb$sym_hol, fmt_num(lb$area_hol)))
    }
  }
  xmax <- s
  if (show_sigma && sig > 1e-12) {                   # dough mode square
    x0 <- s + 0.20 * s
    p <- dough_square(p, x0, 0, sig, 0, sch, dough_mode) +
      annotate("text", x = x0 + sig/2, y = -0.08 * s, size = tsz,
               label = sprintf("%s = %s%s = %s",
                               if (scale == "mean") SIG else "side", RT,
                               fmt_num(lb$area_dgh), fmt_num(sig)))
    xmax <- max(x0 + sig, x0 + sig/2 + 0.22 * s)
  }
  if (show_sides)
    p <- p + annotate("text", x = s/2, y = 1.07 * s, size = tsz,
                      label = sprintf("side = %s%s = %.*f", RT,
                                      fmt_num(lb$area_out), dg, s))
  p <- p + annotate("text", x = s/2, y = (1.07 + show_sides * 0.08) * s,
                    size = tsz, color = "grey25", label = sprintf(
                    "area = %s = %s", lb$sym_out, fmt_num(lb$area_out)))
  if (show_identity)                                  # the core identity
    p <- p + annotate("text", x = s/2, y = -0.10 * s, size = tsz,
                      label = sprintf("%s = %s + %s :  %s = %s + %s",
                      lb$sym_out, lb$sym_hol, lb$sym_dgh, fmt_num(lb$area_out),
                      fmt_num(lb$area_hol), fmt_num(lb$area_dgh)))
  ymin <- -0.22 * s
  if (divide_dough && scale == "sum") {               # divide the dough
    d <- lb$area_dgh; n <- st$n
    divide <- sprintf("Divide the dough:  %s%s = %s %s %d = %s (population)",
                     SIG, S2, fmt_num(d), DIV, n, fmt_num(d / n))
    if (n > 1)
      divide <- sprintf("%s  %s  s%s = %s %s %d = %s (sample)", divide, DOT,
                       S2, fmt_num(d), DIV, n - 1, fmt_num(d / (n - 1)))
    p <- p + annotate("text", x = xmax / 2, y = -0.19 * s, label = divide,
                      size = tsz, color = "grey25")
    ymin <- -0.26 * s
  }
  p + scale_x_continuous(limits = c(-0.05 * s, xmax + 0.05 * s)) +
    scale_y_continuous(limits = c(ymin, 1.20 * s)) +
    coord_fixed(clip = "off") + theme_void(base_size = 12)
}
