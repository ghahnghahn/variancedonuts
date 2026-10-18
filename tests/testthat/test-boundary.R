# The numerical boundary: kappa (Chan, Golub & LeVeque 1983) and the CV

test_that("kappa^2 = whole / dough = 1 + 1/CV^2 (divisor n)", {
  for (nm in setdiff(names(codebook), "berliner")) {
    st <- donut_stats(codebook[[nm]]$x)
    cv2 <- st$variance / st$mean^2
    expect_equal(kappa2(st), 1 + 1 / cv2, tolerance = 1e-10, info = nm)
    expect_equal(st$ss_dev / inner_area(st), cv2, tolerance = 1e-10, info = nm)  # dough / hole
  }
  expect_equal(kappa2(donut_stats(codebook$berliner$x)), 1)     # no hole: perfectly conditioned
})

test_that("kappa^2 values used in the paper's table", {
  k <- sapply(codebook, function(cb) kappa2(donut_stats(cb$x)))
  expect_equal(unname(k[c("example", "berliner")]), c(3, 1))
  expect_equal(unname(k["small_mean"]), 1.05, tolerance = 1e-12)
  expect_equal(unname(k["example_plus_100"]), 1353, tolerance = 1e-12)
  expect_equal(unname(k["glaze_thin"]), 801, tolerance = 1e-10)
})

test_that("the textbook shortcut fails at the boundary while donut_stats does not", {
  shortcut_var <- function(x) mean(x^2) - mean(x)^2
  x <- c(3.8, 3.9, 4.0, 4.1, 4.2) + 1e8                        # kappa^2 about 5e15
  expect_gt(rel_err(shortcut_var(x), 0.02), 0.5)               # shortcut: garbage
  expect_lt(rel_err(donut_stats(x)$variance, 0.02), 1e-6)      # two-pass: fine
  # far from the boundary the two agree
  h <- c(1, 2, 3, 5, 9)
  expect_equal(shortcut_var(h), donut_stats(h)$variance, tolerance = 1e-12)
})

test_that("kappa fields: definitions agree with each other and with the helper", {
  for (nm in names(codebook)) {
    st <- donut_stats(codebook[[nm]]$x)
    expect_equal(st$kappa2, kappa2(st), tolerance = 1e-10, info = nm)
    expect_equal(st$kappa^2, st$kappa2, tolerance = 1e-12, info = nm)
    expect_gte(st$kappa, 1)                                      # CGL: kappa >= 1 always
    expect_equal(st$shortcut_bound, st$n * st$kappa2 * .Machine$double.eps, info = nm)
  }
})

test_that("kappa is the same on the sum and mean scales", {
  for (nm in names(codebook)) {
    x <- codebook[[nm]]$x
    expect_equal(donut_stats(x, "sum")$kappa, donut_stats(x, "mean")$kappa, info = nm)
  }
})

test_that("kappa in donut terms: outer side / Berliner side", {
  for (nm in names(codebook)) {
    st <- donut_stats(codebook[[nm]]$x)
    expect_equal(st$kappa, st$side_outer / st$side_dough, tolerance = 1e-10, info = nm)
  }
})

test_that("kappa matches CGL eq. 2.1 computed independently: ||x|| / sqrt(S)", {
  for (nm in names(codebook)) {
    x <- codebook[[nm]]$x
    S <- sum((x - mean(x))^2)
    expect_equal(donut_stats(x)$kappa, sqrt(sum(x^2)) / sqrt(S), tolerance = 1e-10, info = nm)
  }
})

test_that("kappa is unchanged by rescaling and grows with a shift", {
  x <- c(1, 2, 3, 5, 9)
  expect_equal(donut_stats(1000 * x)$kappa, donut_stats(x)$kappa, tolerance = 1e-12)
  expect_equal(donut_stats(-x)$kappa, donut_stats(x)$kappa, tolerance = 1e-12)
  k <- sapply(c(0, 10, 100, 1e4), function(s) donut_stats(x + s)$kappa)
  expect_true(all(diff(k) > 0))
  # shifting to mean zero gives a perfectly conditioned Berliner (CGL eq. 3.2, d = mean)
  expect_equal(donut_stats(x - mean(x))$kappa, 1)
})

test_that("kappa edge cases: constant data, all zeros, n = 1", {
  st <- donut_stats(rep(4.1, 10))
  expect_equal(st$kappa, Inf); expect_true(st$near_boundary)
  st0 <- donut_stats(c(0, 0, 0))
  expect_equal(st0$kappa, 1); expect_false(st0$near_boundary)
  st1 <- donut_stats(7)
  expect_equal(st1$kappa, Inf); expect_true(st1$near_boundary)
})

test_that("near_boundary: codebook data are safe, the extreme shifts are flagged", {
  for (nm in names(codebook)) expect_false(donut_stats(codebook[[nm]]$x)$near_boundary, info = nm)
  g <- codebook$glaze_thin$x
  expect_true(donut_stats(g + 1e6)$near_boundary)       # the Excel accuracy preset
  expect_true(donut_stats(g + 1e8)$near_boundary)       # where the shortcut returns 0
  expect_false(donut_stats(g + 1e3)$near_boundary)
})

test_that("near_boundary respects boundary_tol", {
  g <- codebook$glaze_thin$x + 1e3                       # bound about 1e-9
  expect_false(donut_stats(g)$near_boundary)
  expect_true(donut_stats(g, boundary_tol = 1e-12)$near_boundary)
})

test_that("the flag is honest: unflagged data never fool the shortcut by much", {
  skip_on_cran()                                         # 2,000 datasets
  shortcut_var <- function(x) mean(x^2) - mean(x)^2
  set.seed(7)
  n_flag <- 0
  for (i in 1:2000) {
    n <- sample(2:200, 1)
    x <- rnorm(n, sample(c(0, 1, 100, 1e4, 1e6, 1e8), 1), sample(c(1e-3, 0.1, 1, 10), 1))
    st <- donut_stats(x)
    if (!st$near_boundary) {
      expect_lt(rel_err(shortcut_var(x), st$variance), 10 * 1e-3,
                label = sprintf("i=%d kappa2=%g", i, st$kappa2))
    } else n_flag <- n_flag + 1
  }
  expect_gt(n_flag, 0)                                   # the stress set does reach the boundary
})

test_that("adding the kappa fields leaves the drawing untouched", {
  expect_no_error(plot_labels(donut_frame(c(1, 2, 3, 5, 9))))
  expect_no_error(suppressMessages(plot_labels(donut_compare(c(1, 2, 3, 5, 9), c(1, 2, 3, 5, 9) + 100))))
})
