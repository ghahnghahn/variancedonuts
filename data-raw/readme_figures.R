# Builds the pictures used in README.md. Run from the package root:
#   source("data-raw/readme_figures.R")
devtools::load_all(quiet = TRUE)

ggplot2::ggsave("man/figures/README-frame.png",
                donut_frame(donut_examples$example),
                width = 6.5, height = 4.2, dpi = 150, bg = "white")

ggplot2::ggsave("man/figures/README-compare.png",
                donut_compare(donut_examples$example, donut_examples$example_minus_2,
                              labels = c("x", "x - 2")),
                width = 6.5, height = 4.2, dpi = 150, bg = "white")
