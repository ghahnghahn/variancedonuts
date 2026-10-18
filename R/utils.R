# Internal helpers. Not exported.

# Symbols used in labels. The source stays pure ASCII (a CRAN requirement),
# so each symbol is written as a \u escape.
S2    <- "\u00b2"
SIG   <- "\u03c3"
RT    <- "\u221a"
SUM   <- "\u03a3"
MU    <- "\u03bc"
MINUS <- "\u2212"
DIV   <- "\u00f7"
DOT   <- "\u00b7"

# Print a number without hiding small values (0.1 is not shown as 0)
fmt_num <- function(v) {
  if (abs(v - round(v)) < 1e-8 * max(1, abs(v)))
    return(format(round(v), scientific = FALSE))
  if (abs(v) >= 0.1) return(sprintf("%.2f", v))
  format(signif(v, 2), scientific = FALSE)
}

# Decimals needed so two nearly equal sides print differently (4.0025 vs 4)
side_digits <- function(a, b, d = 2) {
  while (d < 6 && round(a, d) == round(b, d) && abs(a - b) > 1e-12) d <- d + 1
  d
}

# Areas and symbols for the chosen scale
lab_helper <- function(st, scale) {
  m <- scale == "mean"
  list(area_out = if (m) st$E_x2 else st$sum_x2,
       area_hol = if (m) st$mean^2 else st$n * st$mean^2,
       area_dgh = if (m) st$variance else st$ss_dev,
       sym_out = if (m) paste0("E[X", S2, "]") else paste0(SUM, "x", S2),
       sym_hol = if (m) paste0(MU, S2) else paste0("n", MU, S2),
       sym_dgh = if (m) paste0(SIG, S2)
                 else paste0(SUM, "(x", MINUS, MU, ")", S2))
}
