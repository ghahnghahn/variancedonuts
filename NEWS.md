# variancedonuts 0.1.0

First release, to accompany the paper "Oh doughnuts, it's the variance!".

## Functions

* `donut_stats()` returns the whole, the hole, the dough and the side lengths,
  computing the dough from squared deviations so it stays accurate. It prints
  a short summary and flags glaze-thin data near the numerical boundary.
* `donut_frame()` draws one donut, with an optional Berliner (the same dough
  as a solid square) and the dough divided by `n` and `n - 1`.
* `donut_compare()` draws two donuts on one scale, with a verdict such as
  "Same dough, different donuts".
* `donut_schemes()` lists the color schemes.
* `donut_examples` holds the data sets from the paper.

## Differences from the code listed in the paper

The numbers are identical. The differences are in names and decoration:

* The inner side length is named `side_inner`, matching the paper's notation.
* `donut_stats()` returns a `donut_stats` object with a `print()` summary.
  It is still a list.
* The default color is plain golden (scheme 1). The listing's default,
  golden with a pink glaze stripe, is scheme 3.
* The glaze schemes are named `golden_pinkglaze`, `golden_whiteglaze`,
  `light_pinkglaze` and `light_whiteglaze`.
* Color scheme 7, `"light_sprinkles"`, adds sprinkles to the light donut.
* Color schemes 8 to 10 are flavors: `"strawberry"`, `"chocolate"` and
  `"vanilla"`. Vanilla draws the border of its empty center in its crust color.
* `donut_compare(color = c(3, 7))` colors the two donuts differently.
* A Berliner (a donut with no hole) is never striped or sprinkled.
* The tiny-donut note ends "(subject to screen resolution)".
