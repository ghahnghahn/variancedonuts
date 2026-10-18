#' variancedonuts: square-donut pictures of the variance
#'
#' Draws the identity `sum(x^2) = n * mean^2 + sum((x - mean)^2)` as a square
#' donut. The whole donut is the sum of squared values, the hole comes from
#' the mean, and the dough is the sum of squared deviations. Dividing the dough
#' by `n` or `n - 1` gives the population or sample variance.
#'
#' Main functions:
#' * [donut_stats()]: the numbers behind the picture.
#' * [donut_frame()]: one donut.
#' * [donut_compare()]: two donuts drawn to one scale.
#' * [donut_schemes()]: the available color schemes.
#'
#' @keywords internal
#' @importFrom ggplot2 ggplot annotate scale_x_continuous scale_y_continuous
#'   coord_fixed theme_void
"_PACKAGE"
