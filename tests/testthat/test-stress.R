# Random stress tests. Skipped on CRAN to keep R CMD check fast there.

test_that("stress: 2,000 random datasets keep the identity and match var()", {
  skip_on_cran()
  set.seed(20261003)
  for (i in 1:2000) {
    n  <- sample(1:200, 1)
    mu <- sample(c(0, 1, -50, 1e3, 1e6), 1)
    sd <- sample(c(1e-3, 0.1, 1, 100), 1)
    x  <- rnorm(n, mu, sd)
    if (i %% 10 == 0) x <- round(x)                         # some integer data
    st <- donut_stats(x)
    info <- sprintf("i=%d n=%d mu=%g sd=%g", i, n, mu, sd)
    expect_equal(st$sum_x2, inner_area(st) + st$ss_dev, tolerance = 1e-10, info = info)
    expect_gte(st$ss_dev, 0)
    if (n > 1 && st$ss_dev > 0) {
      # reference: R's var() (also two-pass); loose when the data sit near the boundary
      tol <- max(1e-10, 1e-15 * kappa2(st))
      expect_equal(st$variance, var(x) * (n - 1) / n, tolerance = tol, info = info)
    }
  }
})

test_that("stress: random comparisons always build and never fail the internal area check", {
  skip_on_cran()
  set.seed(42)
  for (i in 1:60) {
    x <- rnorm(sample(1:50, 1), sample(c(0, 5, 1e4), 1), sample(c(0.01, 1, 10), 1))
    y <- rnorm(sample(1:50, 1), sample(c(0, 5, 1e4), 1), sample(c(0.01, 1, 10), 1))
    sc <- sample(c("sum", "mean"), 1)
    expect_no_error(suppressMessages(suppressWarnings(
      plot_labels(donut_compare(x, y, scale = sc)))), message = paste("i =", i))
  }
})
