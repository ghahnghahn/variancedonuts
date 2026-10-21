#' Example data sets for square donuts
#'
#' Small data sets used in the paper and in the Excel workbook. Most are either
#' the Example data `c(1, 2, 3, 5, 9)` or the Example data shifted by a
#' constant. Shifting the data by a constant changes the hole but never the
#' dough, so the Example data and the shifted versions all have the same
#' variance.
#'
#' @format A named list of numeric vectors, each with five values:
#' \describe{
#'   \item{example}{`c(1, 2, 3, 5, 9)`. Whole 120 = hole 80 + dough 40, so the
#'     population variance is 40 / 5 = 8 and the sample variance 40 / 4 = 10.}
#'   \item{example_minus_2}{The Example minus 2. Same dough, smaller mean.}
#'   \item{example_minus_3}{The Example minus 3, with mean 1.}
#'   \item{berliner}{The Example minus 4. The mean is zero, so the donut has no
#'     hole. This is called a Berliner, an all-dough pastry that is similar to a
#'     donut.}
#'   \item{example_plus_2}{The Example plus 2. Same dough, larger mean.}
#'   \item{example_plus_100}{The Example plus 100. Same dough again, but the
#'     mean is so large that the dough is a thin rim.}
#'   \item{small_mean}{`c(-4, -2, 0, 2, 9)`. Dough 100 around a hole of 5.}
#'   \item{glaze_thin}{`c(3.8, 3.9, 4.0, 4.1, 4.2)`. Dough 0.1 around a hole
#'     of 80: the shortcut formula's danger zone in miniature.}
#' }
#'
#' @examples
#' names(donut_examples)
#' donut_stats(donut_examples$example)
#' p <- donut_compare(donut_examples$example, donut_examples$example_minus_2)
#' attr(p, "verdict")
"donut_examples"
