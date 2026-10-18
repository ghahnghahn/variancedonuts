# Package-only extras: sprinkles (scheme 7), two colours in donut_compare(),
# and the "subject to screen resolution" note. Decided 6-10 Oct 2026.

test_that("scheme 7 is light dough with pink sprinkles, no edge, no stripe", {
  sc <- resolve_scheme("light_sprinkles")
  expect_equal(sc$id, 7)
  expect_equal(sc$fill, LIGHT)
  expect_true(is.na(sc$edge)); expect_true(is.na(sc$stripe))
  expect_equal(sc$sprinkles, PINK)
  expect_true(all(is.na(donut_schemes()$sprinkles[1:6])))
})

test_that("sprinkles: one dotted pink loop along the middle of the dough", {
  st <- donut_stats(codebook$example$x); s <- st$side_outer
  a <- (s - st$side_inner) / 4                         # half the band width
  pa <- plot_paths(donut_frame(codebook$example$x, color = 7, show_sigma = FALSE))
  expect_equal(length(unique(pa$layer)), 1)
  expect_true(all(pa$colour == PINK))
  expect_true(all(pa$linetype == "13"))
  expect_equal(range(pa$x), c(a, s - a), tolerance = 1e-12)
  expect_equal(range(pa$y), c(a, s - a), tolerance = 1e-12)
})

test_that("sprinkles never go in the hole", {
  for (nm in c("example", "small_mean")) {
    st <- donut_stats(codebook[[nm]]$x); s <- st$side_outer; h <- st$side_inner
    pa <- plot_paths(donut_frame(codebook[[nm]]$x, color = 7, show_sigma = FALSE))
    # every point lies outside the hole: at least one coordinate beyond its edge
    out_x <- pa$x < (s - h) / 2 | pa$x > (s + h) / 2
    out_y <- pa$y < (s - h) / 2 | pa$y > (s + h) / 2
    expect_true(all(out_x | out_y), info = nm)
  }
})

test_that("sprinkles are skipped on thin dough and on the Berliner beside a donut", {
  expect_equal(nrow(plot_paths(donut_frame(codebook$glaze_thin$x, color = 7))), 0)
  pa <- plot_paths(donut_frame(codebook$example$x, color = 7, show_sigma = TRUE))
  expect_equal(length(unique(pa$layer)), 1)          # the donut only
  expect_equal(nrow(plot_paths(donut_frame(codebook$example$x, color = 7,
                                           dough_mode = FALSE))), 0)   # outlines only
})

test_that("sprinkles are skipped on a donut too small for them in a comparison", {
  x <- codebook$example$x
  p <- suppressMessages(donut_compare(x, x + 100, color = 7))     # thin big donut, tiny Example
  expect_equal(nrow(plot_paths(p)), 0)
  p2 <- donut_compare(x * 2, x - 2, color = 7)                  # bands 9% and 7% of 2x's side
  expect_equal(length(unique(plot_paths(p2)$layer)), 2)          # both donuts sprinkled
  p3 <- donut_compare(x * 3, x - 2, color = 7)                  # x - 2's band: 5% of 3x's side
  expect_equal(length(unique(plot_paths(p3)$layer)), 1)          # only 3x is sprinkled
})

test_that("sprinkles never change the numbers or the verdict", {
  x <- codebook$example$x
  lab <- plot_labels(donut_frame(x, color = 7, show_identity = TRUE))
  expect_true(any(grepl("120 = 80 + 40", lab, fixed = TRUE)))
  expect_equal(attr(donut_compare(x, x + 2, color = 7), "verdict"),
               "Same dough, different donuts")
})

test_that("two colours: each donut keeps its own scheme, whichever is larger", {
  x <- codebook$example$x
  r <- plot_rects(donut_compare(x, x - 2, color = c(3, 7)))
  expect_equal(sum(r$fill == PINK, na.rm = TRUE), 1)       # x: golden with pink stripe
  expect_true(any(r$fill == LIGHT, na.rm = TRUE))           # x - 2: light dough
  expect_equal(length(unique(plot_paths(donut_compare(x, x - 2, color = c(3, 7)))$layer)), 1)
  # swap the order: the colours follow the data, not the position
  r2 <- plot_rects(donut_compare(x - 2, x, color = c(7, 3)))
  expect_equal(sum(r2$fill == PINK, na.rm = TRUE), 1)
})

test_that("two colours leave the numbers and the verdict unchanged", {
  x <- codebook$example$x
  p1 <- donut_compare(x, x - 2, color = 3)
  p2 <- donut_compare(x, x - 2, color = c(3, 7))
  expect_equal(attr(p1, "verdict"), attr(p2, "verdict"))
  expect_equal(attr(p1, "stats"), attr(p2, "stats"))
  expect_setequal(plot_labels(p1), plot_labels(p2))
})

test_that("the default is one scheme for both donuts; three schemes is an error", {
  x <- codebook$example$x
  r <- plot_rects(donut_compare(x, x - 2, color = 3))
  expect_equal(sum(r$fill == PINK, na.rm = TRUE), 2)            # one scheme, both glazed
  r1 <- plot_rects(donut_compare(x, x - 2))
  expect_false(any(r1$fill %in% c(PINK, WHITE)))                 # default: both plain
  expect_error(donut_compare(x, x - 2, color = c(1, 2, 3)), "one scheme, or two")
  expect_error(donut_compare(x, x - 2, color = c(3, 99)), "must be one of")
})

test_that("the tiny-donut note mentions screen resolution", {
  x <- codebook$example$x
  expect_message(donut_compare(x, x + 100), "subject to screen resolution")
})

test_that("flavors 8-10: strawberry, chocolate and vanilla, one color all over", {
  sch <- donut_schemes()
  fl <- sch[sch$id %in% 8:10, ]
  expect_equal(fl$name, c("strawberry", "chocolate", "vanilla"))
  expect_equal(fl$fill, c("#F4A6B8", "#6B4226", "#FFF2E4"))
  expect_equal(fl$edge, c("#C9607A", "#3B2314", "#C8A27A"))
  expect_true(all(is.na(fl$stripe)) && all(is.na(fl$sprinkles)))
  for (nm in fl$name) {
    r <- plot_rects(donut_frame(codebook$example$x, color = nm, show_sigma = FALSE))
    expect_equal(nrow(r), 3, info = nm)               # dough, crust edge, empty center
  }
})

test_that("only vanilla draws the border of the empty center in its crust color", {
  x <- codebook$example$x; h <- donut_stats(x)$side_inner
  center_border <- function(p) {
    r <- plot_rects(p); r$colour[abs(r$side - h) < 1e-9 & r$fill == "white"]
  }
  expect_equal(center_border(donut_frame(x, color = "vanilla")), "#C8A27A")
  for (i in 1:9) expect_equal(center_border(donut_frame(x, color = i)), "grey80",
                              info = paste("scheme", i))
  # the same rule in donut_compare(), per donut
  r <- plot_rects(donut_compare(x, x - 2, color = c("vanilla", "chocolate")))
  centers <- r[!is.na(r$fill) & r$fill == "white", "colour"]
  expect_setequal(centers, c("#C8A27A", "grey80"))
})

test_that("flavors never change the numbers", {
  x <- codebook$example$x
  for (nm in c("strawberry", "chocolate", "vanilla")) {
    lab <- plot_labels(donut_frame(x, color = nm, show_identity = TRUE))
    expect_true(any(grepl("120 = 80 + 40", lab, fixed = TRUE)), info = nm)
  }
})
