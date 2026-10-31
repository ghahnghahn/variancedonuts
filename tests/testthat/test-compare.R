# donut_compare(): verdicts and safeguards

test_that("same dough, different donuts (shift demo), with the small-donut note", {
  expect_message(p <- donut_compare(c(1, 2, 3, 5, 9), c(1, 2, 3, 5, 9) + 100),
                 "smaller donut")
  expect_true(any(grepl("Same dough", verdict_of(p))))
  expect_equal(attr(p, "stats")$ss_ratio, 120 / 54120, tolerance = 1e-12)
})

test_that("more dough = more variance, regardless of argument order", {
  for (args in list(list(c(-4, -2, 0, 2, 9), c(1, 2, 3, 5, 9)),
                    list(c(1, 2, 3, 5, 9), c(-4, -2, 0, 2, 9)))) {
    p <- do.call(donut_compare, args)
    expect_true(any(grepl("More dough = more variance:  100 vs 40", verdict_of(p), fixed = TRUE)))
  }
})

test_that("different n on the sum scale: warning, and the verdict compares variances", {
  x <- c(1, 2, 3, 5, 9)
  expect_warning(p <- donut_compare(x, rep(x, 2)), "different n")
  expect_true(any(grepl("Same variance", verdict_of(p))))
  expect_warning(p2 <- donut_compare(x, c(x, 20)), "different n")
  expect_true(any(grepl("Variance per observation", verdict_of(p2))))
})

test_that("different n on the mean scale: no warning", {
  x <- c(1, 2, 3, 5, 9)
  expect_no_warning(p <- donut_compare(x, rep(x, 2), scale = "mean"))
  expect_true(any(grepl("Same dough", verdict_of(p))))
})

test_that("donut_compare builds with zero-dough data and with Berliners", {
  expect_no_error(plot_labels(donut_compare(rep(3, 5), c(1, 2, 3, 5, 9))))
  expect_no_error(plot_labels(donut_compare(c(-8, -2, 0, 2, 8), c(1, 2, 3, 5, 9))))
  expect_no_error(plot_labels(suppressWarnings(donut_compare(7, c(1, 2, 3, 5, 9)))))
})

test_that("donut_compare with two all-zero datasets fails gracefully, not cryptically", {
  expect_error(donut_compare(c(0, 0), c(0, 0)), "all zeros")
  # one all-zero dataset against a real one is fine (drawn to scale, with the note)
  expect_no_error(suppressMessages(plot_labels(donut_compare(c(0, 0, 0, 0, 0), c(1, 2, 3, 5, 9)))))
})

test_that("verdict: the same data twice is 'Same donut', a shift is still 'different donuts'", {
  x <- c(1, 2, 3, 5, 9)
  expect_equal(verdict_of(donut_compare(x, x)), "Same donut")
  expect_equal(verdict_of(donut_compare(x, -x)), "Same donut")       # mirror image: same picture
  expect_equal(verdict_of(donut_compare(x, x + 2)), "Same dough, different donuts")
})

test_that("donut_compare: show_names / show_verdict drop labels, not the verdict logic", {
  x <- c(1, 2, 3, 5, 9)
  p <- donut_compare(x, x + 2, labels = c("A", "B"),
                     show_names = FALSE, show_verdict = FALSE)
  lab <- plot_labels(p)
  expect_false(any(lab %in% c("A", "B")))
  expect_false(any(grepl("Same dough", lab)))
  expect_equal(attr(p, "verdict"), "Same dough, different donuts")
  expect_true(any(grepl("dough = .* = 40", lab)))
  lab2 <- plot_labels(donut_compare(x, x + 2, labels = c("A", "B")))
  expect_true(all(c("A", "B") %in% lab2))
})
