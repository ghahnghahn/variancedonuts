# Internal formatting helpers

test_that("fmt_num shows small values instead of rounding them to 0", {
  expect_equal(fmt_num(40), "40")
  expect_equal(fmt_num(120), "120")
  expect_equal(fmt_num(0.1), "0.10")
  expect_equal(fmt_num(0.02), "0.02")
  expect_equal(fmt_num(80.1), "80.10")
  expect_false(fmt_num(0.0004) == "0")
  expect_equal(fmt_num(-4), "-4")
  expect_equal(fmt_num(1e6), "1000000")                         # no scientific notation
})

test_that("side_digits separates nearly equal sides", {
  expect_equal(side_digits(4, 2), 2)
  expect_gt(side_digits(4.0025, 4), 2)
  expect_equal(side_digits(5, 5), 2)                            # equal: no endless loop
})

test_that("the package source is pure ASCII (CRAN rule)", {
  skip_if_not(dir.exists(test_path("../../R")), "source files not available")
  for (f in list.files(test_path("../../R"), full.names = TRUE)) {
    bytes <- readBin(f, "raw", file.size(f))
    expect_false(any(bytes > as.raw(127)), info = basename(f))
  }
})
