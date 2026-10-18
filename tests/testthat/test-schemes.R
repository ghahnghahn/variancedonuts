# Colour schemes and stripes

test_that("donut_schemes: ten schemes, unique ids and names, valid colours", {
  sch <- donut_schemes()
  expect_equal(nrow(sch), 10)
  expect_equal(sch$id, 1:10)
  expect_false(anyDuplicated(sch$name) > 0)
  cols <- c(sch$fill, na.omit(sch$edge), na.omit(sch$stripe), na.omit(sch$sprinkles), na.omit(sch$inner_edge))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
  expect_equal(sch$fill[1:2], c(GOLD, LIGHT))
})

test_that("schemes resolve by number or name; bad values give a plain error", {
  for (i in 1:10) {
    nm <- donut_schemes()$name[i]
    expect_equal(resolve_scheme(i), resolve_scheme(nm), info = nm)
  }
  expect_error(resolve_scheme(11), "must be one of")
  expect_error(resolve_scheme("purple"), "must be one of")
  expect_error(resolve_scheme(c(1, 2)), "must be one of")
  expect_error(donut_frame(c(1, 2, 3, 5, 9), color = 0), "must be one of")
})

test_that("fill_dough and edge_dough override a scheme", {
  sc <- resolve_scheme(3, fill_dough = "#123456", edge_dough = "#654321")
  expect_equal(c(sc$fill, sc$edge, sc$stripe), c("#123456", "#654321", PINK))
})

test_that("the default scheme is 1, plain golden with no glaze (user, 2026-10-17)", {
  expect_equal(formals(donut_frame)$color, 1)
  expect_equal(formals(donut_compare)$color, 1)
  r <- plot_rects(donut_frame(c(1, 2, 3, 5, 9), show_sigma = FALSE))
  expect_false(any(r$fill %in% c(PINK, WHITE)))
  expect_equal(r$fill[1], GOLD)
  expect_equal(nrow(plot_paths(donut_frame(c(1, 2, 3, 5, 9)))), 0)   # no sprinkles
  # scheme 3 still gives golden with pink glaze
  r3 <- plot_rects(donut_frame(c(1, 2, 3, 5, 9), color = 3, show_sigma = FALSE))
  expect_equal(sum(r3$fill == PINK, na.rm = TRUE), 1)
})

test_that("color = 1: golden dough, crust edge drawn last, no stripe", {
  r <- plot_rects(donut_frame(c(1, 2, 3, 5, 9), color = 1, show_sigma = FALSE))
  expect_equal(r$fill[1], GOLD)                       # dough fill first
  expect_true(is.na(r$fill[2]) && r$colour[2] == CRUST) # crust outline drawn over it
  expect_false(any(r$fill %in% c(PINK, WHITE)))      # the hole is "white", not a stripe
  expect_equal(nrow(r), 3)                            # fill, outline, hole
})

test_that("stripe sits in the middle third of the dough band (Example)", {
  st <- donut_stats(c(1, 2, 3, 5, 9)); s <- st$side_outer; h <- st$side_inner
  w <- (s - h) / 2
  for (cl in list(list(3, PINK, GOLD), list(4, WHITE, GOLD),
                  list(5, PINK, LIGHT), list(6, WHITE, LIGHT))) {
    r <- plot_rects(donut_frame(c(1, 2, 3, 5, 9), color = cl[[1]], show_sigma = FALSE))
    stripe <- r[!is.na(r$fill) & r$fill == cl[[2]] & abs(r$side - (s - 2 * w / 3)) < 1e-9, ]
    inner  <- r[!is.na(r$fill) & r$fill == cl[[3]] & abs(r$side - (s - 4 * w / 3)) < 1e-9, ]
    expect_equal(nrow(stripe), 1, info = paste("scheme", cl[[1]]))
    expect_equal(nrow(inner), 1, info = paste("scheme", cl[[1]]))
    # stripe is centred on the donut
    expect_equal((stripe$xmin + stripe$xmax) / 2, s / 2, tolerance = 1e-12)
    # the hole (white, grey border) is still the last rectangle and has the right side
    expect_equal(r$side[nrow(r)], h, tolerance = 1e-12)
    expect_equal(r$colour[nrow(r)], "grey80")
  }
})

test_that("stripes never change the stated areas or the verdict", {
  x <- c(1, 2, 3, 5, 9)
  for (i in 1:10) {
    lab <- plot_labels(donut_frame(x, color = i, show_identity = TRUE))
    expect_true(any(grepl("120 = 80 + 40", lab, fixed = TRUE)), info = paste("scheme", i))
    v <- verdict_of(suppressMessages(donut_compare(x, x + 100, color = i)))
    expect_true(any(grepl("Same dough", v)), info = paste("scheme", i))
  }
})

test_that("light schemes have no crust edge", {
  r <- plot_rects(donut_frame(c(1, 2, 3, 5, 9), color = 2, show_sigma = FALSE))
  expect_false(any(r$colour == CRUST, na.rm = TRUE))
})

test_that("no hole (mean 0): a Berliner stays plain, with no stripe or sprinkles", {
  # package only (user, 2026-10-17); the paper's listing stripes this donut
  for (x in list(codebook$berliner$x, codebook$example$x - 4)) {
    for (i in 3:7) {
      r <- plot_rects(donut_frame(x, color = i, show_sigma = FALSE))
      expect_false(any(r$fill %in% c(PINK, WHITE)), info = paste("scheme", i))
      expect_equal(nrow(plot_paths(donut_frame(x, color = i))), 0)
    }
  }
  # and it looks exactly like the Berliner drawn beside the Example donut
  b <- plot_rects(donut_frame(codebook$example$x - 4, show_sigma = FALSE))
  r <- plot_rects(donut_frame(codebook$example$x, scale = "sum"))
  side <- donut_stats(codebook$example$x)$side_dough
  beside <- r[abs(r$side - side) < 1e-9, c("fill", "colour")]
  expect_equal(b[, c("fill", "colour")], beside, ignore_attr = TRUE)
})

test_that("dough mode square is never striped, while the donut keeps its stripe", {
  st <- donut_stats(c(1, 2, 3, 5, 9), "mean"); sig <- st$side_dough
  s <- st$side_outer; w <- (s - st$side_inner) / 2
  for (i in 3:6) {
    r <- plot_rects(donut_frame(c(1, 2, 3, 5, 9), scale = "mean", color = i))
    stripe_col <- donut_schemes()$stripe[i]
    striped <- r[!is.na(r$fill) & r$fill == stripe_col, ]
    expect_equal(nrow(striped), 1, info = paste("scheme", i))          # only the donut's ring
    expect_equal(striped$side, s - 2 * w / 3, tolerance = 1e-9)
    expect_false(any(abs(r$side - (sig - sig / 3)) < 1e-9), info = paste("scheme", i))
  }
  r <- plot_rects(suppressMessages(donut_compare(c(1, 2, 3, 5, 9), c(1, 2, 3, 5, 9) + 1,
                                                 color = 3, show_sigma = TRUE)))
  expect_equal(sum(r$fill == PINK, na.rm = TRUE), 2)                     # one per donut
})

test_that("the cream stripe is never the same color as the empty center", {
  for (i in 1:10) {
    st <- donut_schemes()$stripe[i]
    expect_false(isTRUE(toupper(st) %in% c("#FFFFFF", "WHITE")), info = paste("scheme", i))
  }
})

test_that("outline mode (dough_mode = FALSE) draws no fill and no stripe", {
  r <- plot_rects(donut_frame(c(1, 2, 3, 5, 9), dough_mode = FALSE, color = 3))
  expect_false(any(r$fill %in% c(PINK, GOLD), na.rm = TRUE))
  expect_true(any(r$colour == CRUST, na.rm = TRUE))
  r2 <- plot_rects(donut_frame(c(1, 2, 3, 5, 9), dough_mode = FALSE, color = 2))
  expect_true(any(r2$colour == "grey40", na.rm = TRUE))   # light scheme: grey outline
})

test_that("every scheme builds for every codebook dataset, frame and compare", {
  for (i in 1:10) for (nm in names(codebook)) {
    x <- codebook[[nm]]$x
    expect_no_error(plot_labels(donut_frame(x, color = i)))
    expect_no_error(suppressMessages(plot_labels(donut_compare(x, c(1, 2, 3, 5, 9), color = i,
                                                               show_sigma = TRUE))))
  }
})
