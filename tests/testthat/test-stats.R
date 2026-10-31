# donut_stats(): codebook values, the identity, invariances, input checks

test_that("donut_stats reproduces every codebook value", {
  for (nm in names(codebook)) {
    cb <- codebook[[nm]]; st <- donut_stats(cb$x)
    info <- paste("dataset:", nm)
    expect_equal(st$n, 5, info = info)
    expect_equal(st$sum_x2, cb$sum_x2, tolerance = 1e-12, info = info)
    expect_equal(inner_area(st), cb$hole, tolerance = 1e-12, info = info)
    expect_equal(st$ss_dev, cb$ss, tolerance = 1e-10, info = info)
    expect_equal(st$variance, cb$var, tolerance = 1e-10, info = info)
    expect_equal(st$ss_dev / (st$n - 1), cb$s2, tolerance = 1e-10, info = info)
  }
})

test_that("whole = hole + dough, as areas and as squared sides, on both scales", {
  for (nm in names(codebook)) {
    x <- codebook[[nm]]$x
    s <- donut_stats(x, "sum"); m <- donut_stats(x, "mean")
    expect_equal(s$sum_x2, inner_area(s) + s$ss_dev, tolerance = 1e-12, info = nm)
    expect_equal(m$E_x2, m$mean^2 + m$variance, tolerance = 1e-12, info = nm)
    for (st in list(s, m)) {
      expect_equal(st$side_outer^2, st$side_inner^2 + st$side_dough^2, tolerance = 1e-12, info = nm)
    }
    # sum-scale sides are sqrt(n) times mean-scale sides
    expect_equal(s$side_outer, sqrt(s$n) * m$side_outer, tolerance = 1e-12, info = nm)
    # on the mean scale the Berliner has side sigma
    expect_equal(m$side_dough, sqrt(m$variance), tolerance = 1e-12, info = nm)
  }
})

test_that("E_x2 matches the plain mean of squares where cancellation is not an issue", {
  x <- c(1, 2, 3, 5, 9)
  expect_equal(donut_stats(x, "mean")$E_x2, mean(x^2), tolerance = 1e-12)
})

test_that("shift invariance: the dough does not change when a constant is added", {
  base <- c(3.8, 3.9, 4.0, 4.1, 4.2)       # glaze-thin, the hardest codebook case
  for (k in c(-1e6, -100, 0, 100, 1e3, 1e6, 1e8)) {
    st <- donut_stats(base + k)
    expect_lt(rel_err(st$ss_dev, 0.1), 1e-6, label = paste("shift", k))
  }
})

test_that("scaling by c multiplies the dough by c^2", {
  x <- c(1, 2, 3, 5, 9)
  for (cc in c(-3, 0.01, 7, 1e4)) {
    expect_equal(donut_stats(cc * x)$ss_dev, cc^2 * 40, tolerance = 1e-10, info = paste("c =", cc))
  }
})

test_that("order of the data does not matter", {
  x <- c(1, 2, 3, 5, 9)
  expect_equal(donut_stats(rev(x)), donut_stats(x), tolerance = 1e-12)
  expect_equal(donut_stats(c(9, 1, 5, 3, 2)), donut_stats(x), tolerance = 1e-12)
})

test_that("bad input gives a plain error", {
  expect_error(donut_stats(numeric(0)),       "non-empty numeric")
  expect_error(donut_stats(c("1", "2")),      "non-empty numeric")
  expect_error(donut_stats(c(TRUE, FALSE)),   "non-empty numeric")
  expect_error(donut_stats(NULL),             "non-empty numeric")
  expect_error(donut_stats(c(1, NA, 3)),      "NA, NaN or infinite")
  expect_error(donut_stats(c(1, NaN, 3)),     "NA, NaN or infinite")
  expect_error(donut_stats(c(1, Inf, 3)),     "NA, NaN or infinite")
  expect_error(donut_stats(1:3, scale = "median"))
})

test_that("integer input works like double input", {
  expect_equal(donut_stats(c(1L, 2L, 3L, 5L, 9L)), donut_stats(c(1, 2, 3, 5, 9)))
})

test_that("n = 1: no dough, no error", {
  st <- donut_stats(7)
  expect_equal(st$ss_dev, 0); expect_equal(st$variance, 0)
  expect_equal(st$sum_x2, 49); expect_equal(inner_area(st), 49)
})

test_that("constant data: dough is exactly zero", {
  st <- donut_stats(rep(4.1, 10))
  expect_equal(st$ss_dev, 0, tolerance = 1e-24)
  expect_gte(st$ss_dev, 0)
})

test_that("mean zero: no hole (the Berliner)", {
  st <- donut_stats(c(-8, -2, 0, 2, 8))
  expect_equal(st$side_inner, 0); expect_equal(st$sum_x2, st$ss_dev)
})

test_that("negative mean: inner side is sqrt(n)|mean| and the area is unchanged", {
  st_neg <- donut_stats(-c(1, 2, 3, 5, 9)); st_pos <- donut_stats(c(1, 2, 3, 5, 9))
  expect_equal(st_neg$side_inner, 4 * sqrt(5))
  expect_equal(inner_area(st_neg), inner_area(st_pos))
  expect_equal(st_neg$ss_dev, st_pos$ss_dev)
})

test_that("200 observations (the Excel limit) work", {
  set.seed(1); x <- rnorm(200, 50, 10)
  st <- donut_stats(x)
  expect_equal(st$n, 200)
  expect_equal(st$variance, var(x) * 199 / 200, tolerance = 1e-12)
})

# --- package only ------------------------------------------------------------

test_that("the result is a donut_stats list with exactly the documented fields", {
  st <- donut_stats(c(1, 2, 3, 5, 9))
  expect_s3_class(st, "donut_stats")
  expect_type(st, "list")
  expect_named(st, c("n", "mean", "E_x2", "sum_x2", "variance", "ss_dev",
                     "side_outer", "side_inner", "side_dough",
                     "kappa", "kappa2", "shortcut_bound", "near_boundary"))
  expect_equal(attr(st, "scale"), "sum")
  expect_equal(attr(donut_stats(1:3, "mean"), "scale"), "mean")
})

test_that("print() shows the identity and both variances", {
  out <- capture.output(print(donut_stats(c(1, 2, 3, 5, 9))))
  expect_true(any(grepl("120 = 80 + 40", out, fixed = TRUE)))
  expect_true(any(grepl("40 / 5 = 8", out, fixed = TRUE)))
  expect_true(any(grepl("40 / 4 = 10", out, fixed = TRUE)))
  expect_false(any(grepl("Glaze-thin", out)))
  capture.output(res <- withVisible(print(donut_stats(1:3))))
  expect_false(res$visible)                     # print() returns invisibly
})

test_that("print() handles n = 1, the mean scale and glaze-thin data", {
  expect_false(any(grepl("sample", capture.output(print(donut_stats(7))))))
  out_m <- capture.output(print(donut_stats(c(1, 2, 3, 5, 9), "mean")))
  expect_true(any(grepl("24 = 16 + 8", out_m, fixed = TRUE)))
  out_g <- capture.output(print(donut_stats(codebook$glaze_thin$x + 1e6)))
  expect_true(any(grepl("Glaze-thin", out_g)))
})
