# Shared test data and helpers. testthat loads helper-*.R files before the tests.
# Expected values come from the project codebook (02_data/codebook.md).

codebook <- list(
  example          = list(x = c(1, 2, 3, 5, 9),           sum_x2 = 120,   hole = 80,    ss = 40,  var = 8,    s2 = 10),
  example_plus_100 = list(x = c(1, 2, 3, 5, 9) + 100,     sum_x2 = 54120, hole = 54080, ss = 40,  var = 8,    s2 = 10),
  small_mean       = list(x = c(-4, -2, 0, 2, 9),         sum_x2 = 105,   hole = 5,     ss = 100, var = 20,   s2 = 25),
  berliner         = list(x = c(-8, -2, 0, 2, 8),         sum_x2 = 136,   hole = 0,     ss = 136, var = 27.2, s2 = 34),
  glaze_thin       = list(x = c(3.8, 3.9, 4.0, 4.1, 4.2), sum_x2 = 80.1,  hole = 80,    ss = 0.1, var = 0.02, s2 = 0.025)
)

# all text labels a plot actually renders (forces the plot to build)
plot_labels <- function(p) {
  d <- ggplot2::ggplot_build(p)$data
  unlist(lapply(d, function(l) if ("label" %in% names(l)) as.character(l$label)))
}

# all drawn rectangles, in drawing order, with their fill and edge colours
plot_rects <- function(p) {
  d <- ggplot2::ggplot_build(p)$data
  r <- do.call(rbind, lapply(d, function(l)
    if (all(c("xmin", "xmax", "ymin", "ymax") %in% names(l)))
      data.frame(xmin = l$xmin, xmax = l$xmax, ymin = l$ymin, ymax = l$ymax,
                 fill = as.character(l$fill), colour = as.character(l$colour),
                 stringsAsFactors = FALSE)))
  r$side <- r$xmax - r$xmin
  r
}

# all drawn paths (the sprinkles), one row per point, with a layer number
plot_paths <- function(p) {
  d <- ggplot2::ggplot_build(p)$data
  r <- do.call(rbind, lapply(seq_along(d), function(i) {
    l <- d[[i]]
    if (!is.null(l$linetype) && all(c("x", "y") %in% names(l)) && !"label" %in% names(l) &&
        !"xmin" %in% names(l))
      data.frame(layer = i, x = l$x, y = l$y, colour = as.character(l$colour),
                 linetype = as.character(l$linetype), stringsAsFactors = FALSE)
  }))
  if (is.null(r)) data.frame(layer = integer(0), x = numeric(0), y = numeric(0),
                             colour = character(0), linetype = character(0)) else r
}

verdict_of <- function(p) {
  lab <- plot_labels(p)
  lab[(grepl("dough|variance|Variance", lab) & !grepl("^dough =", lab)) |
        lab == "Same donut"]
}

inner_area <- function(st) st$n * st$mean^2
kappa2    <- function(st) st$sum_x2 / st$ss_dev     # Chan, Golub & LeVeque eq. 2.1, squared
rel_err   <- function(a, b) abs(a - b) / abs(b)

# scheme colours; the "white" stripe is cream so it never merges with the hole
PINK <- "#F4A6B8"; WHITE <- "#FFF2E4"; GOLD <- "#E3B072"; LIGHT <- "#F2D6AF"; CRUST <- "#9C6430"

# label symbols, written as escapes so the test files stay ASCII
U_DIV <- "\u00f7"; U_SIG <- "\u03c3"; U_RT <- "\u221a"
