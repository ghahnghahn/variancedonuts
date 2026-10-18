# donut_frame(): builds and labels correctly

test_that("donut_frame builds for every dataset, scale and option", {
  for (nm in names(codebook)) for (sc in c("sum", "mean"))
    for (dm in c(TRUE, FALSE)) for (sg in c(TRUE, FALSE)) for (sh in c(TRUE, FALSE)) {
      p <- donut_frame(codebook[[nm]]$x, scale = sc, dough_mode = dm,
                       show_sigma = sg, divide_dough = sh)
      expect_s3_class(p, "ggplot")
      expect_no_error(plot_labels(p))
    }
})

test_that("donut_frame labels: Example, sum scale", {
  lab <- plot_labels(donut_frame(c(1, 2, 3, 5, 9), scale = "sum", show_identity = TRUE))
  expect_true(any(grepl("120 = 80 + 40", lab, fixed = TRUE)))
  expect_true(any(grepl(paste("40", U_DIV, "5 = 8 (population)"), lab, fixed = TRUE)))
  expect_true(any(grepl("^Divide the dough:", lab)))
  expect_true(any(grepl(paste("40", U_DIV, "4 = 10 (sample)"), lab, fixed = TRUE)))
  expect_false(any(grepl(paste0("^", U_SIG, " ="), lab)))       # no sigma label on the sum scale
  expect_true(any(grepl(paste0("^side = ", U_RT, "40"), lab)))
})

test_that("donut_frame labels: Example, mean scale shows sigma", {
  lab <- plot_labels(donut_frame(c(1, 2, 3, 5, 9), scale = "mean", show_identity = TRUE))
  expect_true(any(grepl("24 = 16 + 8", lab, fixed = TRUE)))
  expect_true(any(grepl(paste0(U_SIG, " = ", U_RT, "8"), lab, fixed = TRUE)))
  expect_false(any(grepl("Divide the dough", lab, fixed = TRUE)))  # sum scale only
})

test_that("donut_frame labels: glaze-thin dough is not shown as 0", {
  lab <- plot_labels(donut_frame(c(3.8, 3.9, 4.0, 4.1, 4.2), scale = "sum",
                                 show_identity = TRUE))
  expect_true(any(grepl("80.10 = 80 + 0.10", lab, fixed = TRUE)))
})

test_that("donut_frame edge cases build: n = 1, constant data, all zeros", {
  for (x in list(7, rep(4.1, 10), c(0, 0, 0))) {
    expect_no_error(plot_labels(donut_frame(x)))
  }
  lab <- plot_labels(donut_frame(7))
  expect_false(any(grepl("(sample)", lab, fixed = TRUE)))        # no n - 1 when n = 1
})

test_that("show_identity: the core identity line is off by default", {
  x <- c(1, 2, 3, 5, 9)
  expect_false(any(grepl("120 = 80 + 40", plot_labels(donut_frame(x)), fixed = TRUE)))
  expect_true(any(grepl("120 = 80 + 40",
                        plot_labels(donut_frame(x, show_identity = TRUE)), fixed = TRUE)))
})
