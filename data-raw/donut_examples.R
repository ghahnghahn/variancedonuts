# Builds data/donut_examples.rda. Run from the package root:
#   source("data-raw/donut_examples.R")
# The values match the paper (Figures 1-4) and the Excel workbook's presets.

example <- c(1, 2, 3, 5, 9)

donut_examples <- list(
  example          = example,               # whole 120 = hole 80 + dough 40
  example_minus_2  = example - 2,           # paper Figure 2: same dough, smaller mean
  example_minus_3  = example - 3,           # paper Figure 3: mean 1
  berliner         = example - 4,           # paper Figure 1: mean 0, no hole
  example_plus_2   = example + 2,           # Excel preset: same dough, larger mean
  example_plus_100 = example + 100,         # same dough, a hole far bigger than the dough
  small_mean       = c(-4, -2, 0, 2, 9),    # dough 100, hole 5
  glaze_thin       = c(3.8, 3.9, 4.0, 4.1, 4.2)  # dough 0.1, hole 80
)

usethis::use_data(donut_examples, overwrite = TRUE)
