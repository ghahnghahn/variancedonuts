#' Draw two square donuts to one scale
#'
#' Draws the donuts for two data sets side by side on one shared scale, so
#' their areas can be compared honestly: the larger donut is on the left and
#' the smaller one appears at its true relative size. A verdict line says
#' whether the two have the same dough, for example "Same dough, different
#' donuts" for `x` and `x + 2`.
#'
#' Three safeguards apply. An internal check confirms the drawn areas equal the
#' stated ones. A warning appears when the data sets have different numbers of
#' observations on the sum scale, because donut size then grows with `n` as
#' well as with spread. A note appears when one donut is under 1% of the other's
#' area, since it is drawn to scale and may be hard to see.
#'
#' @param x,y Numeric vectors to compare.
#' @param labels Names shown above the two donuts.
#' @param color Color scheme, by number or name (see [donut_schemes()]). One
#'   scheme is used for both donuts. Give two, for example `c(3, 7)`, to color
#'   `x` and `y` differently. Use this for genuinely different groups only:
#'   different colors can suggest a difference, and lighter fills can look
#'   slightly larger.
#' @inheritParams donut_frame
#' @param show_sigma Draw a Berliner beside each donut: its dough as a solid
#'   square with no hole.
#' @param show_names Show the names above the donuts.
#' @param show_verdict Show the verdict line below the donuts.
#'
#' @return A ggplot object with two attributes: `"verdict"`, the verdict text
#'   (kept even when not drawn), and `"stats"`, a list with both
#'   [donut_stats()] results, the area ratio `ss_ratio` and the side ratio
#'   `side_ratio`.
#'
#' @inheritSection donut_frame Saving
#'
#' @examples
#' x <- c(1, 2, 3, 5, 9)
#' p <- donut_compare(x, x - 2, labels = c("x", "x - 2"))
#' attr(p, "verdict")
#' if (interactive()) p
#' @export
donut_compare <- function(x, y, labels = c("Dataset 1", "Dataset 2"),
                          scale = c("sum", "mean"), dough_mode = TRUE,
                          show_sigma = FALSE, color = 1,
                          fill_dough = NULL, edge_dough = NULL,
                          show_names = TRUE, show_verdict = TRUE) {
  scale <- match.arg(scale)
  if (!length(color) %in% 1:2)
    stop("`color` must be one scheme, or two (one per donut).", call. = FALSE)
  sch1 <- resolve_scheme(color[1], fill_dough, edge_dough)
  sch2 <- resolve_scheme(color[length(color)], fill_dough, edge_dough)
  st1 <- donut_stats(x, scale); lb1 <- lab_helper(st1, scale)
  st2 <- donut_stats(y, scale); lb2 <- lab_helper(st2, scale)
  a1 <- lb1$area_out; a2 <- lb2$area_out
  if (max(a1, a2) == 0)
    stop("Both datasets are all zeros, so there is no donut to draw.",
         call. = FALSE)
  ratio <- min(a1, a2) / max(a1, a2)
  for (pr in list(list(st1, lb1), list(st2, lb2)))   # drawn = stated areas
    if (!isTRUE(all.equal(pr[[1]]$side_outer^2, pr[[2]]$area_out,
                          tolerance = 1e-8)) ||
        !isTRUE(all.equal(pr[[1]]$side_inner^2 + pr[[1]]$side_dough^2,
                          pr[[1]]$side_outer^2, tolerance = 1e-8)))
      stop("internal error: drawn areas do not match the statistics")
  if (scale == "sum" && st1$n != st2$n)
    warning("Datasets have different n (", st1$n, " vs ", st2$n, "). On ",
            "the sum scale, donut size also grows with n; consider ",
            "scale = \"mean\". The verdict compares variances.", call. = FALSE)
  if (ratio < 0.01)
    message(sprintf(paste0("Note: the smaller donut is %.2g%% of the larger ",
                           "one's area. It is drawn to scale and may be hard ",
                           "to see (subject to screen resolution)."),
                    100 * ratio))
  big <- list(st = st1, lb = lb1, nm = labels[1], sch = sch1)  # larger on the left
  sml <- list(st = st2, lb = lb2, nm = labels[2], sch = sch2)
  if (a2 > a1) { tmp <- big; big <- sml; sml <- tmp }
  sb <- big$st$side_outer; ss <- sml$st$side_outer
  x2 <- sb + 0.35 * sb; tsz <- 3.6
  add_one <- function(p, d, xoff) {
    s <- d$st$side_outer; h <- d$st$side_inner; sig <- d$st$side_dough
    p <- dough_square(p, xoff, 0, s, h, d$sch, dough_mode, ref = sb)
    if (h > 1e-12) {
      hc <- xoff + s / 2
      ib <- inner_border(d$sch)
      p <- p + annotate("rect", xmin = hc - h/2, xmax = hc + h/2,
                        ymin = s/2 - h/2, ymax = s/2 + h/2, fill = "white",
                        color = ib$colour, linewidth = ib$linewidth)
      if (h > 0.15 * s && s > 0.25 * sb)          # no label on a tiny donut
        p <- p + annotate("text", x = hc, y = s/2 + 0.03 * s, size = tsz,
                          color = "grey25", label = sprintf("%s = %s",
                          d$lb$sym_hol, fmt_num(d$lb$area_hol)))
    }
    if (show_sigma && sig > 1e-12)
      p <- dough_square(p, xoff + 1.15 * s, 0, sig, 0, d$sch, dough_mode)
    if (show_names)
      p <- p + annotate("text", x = xoff + s/2, y = s + 0.06 * sb,
                        label = d$nm, size = tsz + 0.4)
    p +
      annotate("text", x = xoff + s/2, y = s + (0.06 + show_names * 0.11) *
               sb, size = tsz, color = "grey25", label = sprintf(
               "area = %s = %s", d$lb$sym_out, fmt_num(d$lb$area_out))) +
      annotate("text", x = xoff + s/2, y = -0.10 * sb, size = tsz,
               label = sprintf("dough = %s = %s", d$lb$sym_dgh,
                               fmt_num(d$lb$area_dgh)))
  }
  p <- add_one(add_one(ggplot(), big, 0), sml, x2)
  va <- big$st$variance; vb <- sml$st$variance
  same <- isTRUE(all.equal(va, vb, tolerance = 1e-8)) || max(va, vb) < 1e-12
  same_n <- big$st$n == sml$st$n
  same_inner <- isTRUE(all.equal(big$lb$area_hol, sml$lb$area_hol,
                                tolerance = 1e-8))
  verdict <- if (same && same_inner && same_n) {
    "Same donut"                                  # identical pictures
  } else if (same && (same_n || scale == "mean")) {
    "Same dough, different donuts"
  } else if (same) {
    "Same variance, different donuts"
  } else if (same_n) {
    da <- big$lb$area_dgh; db <- sml$lb$area_dgh
    sprintf("More dough = more variance:  %s vs %s",
            fmt_num(max(da, db)), fmt_num(min(da, db)))
  } else {
    sprintf("Variance per observation:  %s %s vs %s %s",
            big$nm, fmt_num(va), sml$nm, fmt_num(vb))
  }
  xr <- x2 + max(ss, 0.5 * sb) + 0.05 * sb       # room for a tiny donut
  if (show_verdict)
    p <- p + annotate("text", x = (xr - 0.05 * sb) / 2, y = -0.25 * sb,
                      label = verdict, size = tsz + 0.6, fontface = "italic")
  out <- p +
    scale_x_continuous(limits = c(-0.05 * sb, xr)) +
    scale_y_continuous(limits = c((if (show_verdict) -0.35 else -0.18) * sb,
                                  1.25 * sb)) +
    coord_fixed(clip = "off") + theme_void(base_size = 12)
  attr(out, "verdict") <- verdict                 # kept even when not drawn
  attr(out, "stats") <- list(first = st1, second = st2, ss_ratio = ratio,
                             side_ratio = sqrt(ratio))
  out
}
