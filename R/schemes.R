#' Color schemes for the donuts
#'
#' Lists the color schemes that [donut_frame()] and [donut_compare()] accept
#' through their `color` argument, by number or by name. The default, scheme 1,
#' is plain golden dough with a crust edge. Schemes 3 to 6 add a stripe of pink
#' or white glaze: a ring through the middle third of the dough. It is
#' decoration only and changes no area. The "white" glaze is cream (`#FFF2E4`),
#' so it never merges with the empty center.
#'
#' Scheme 7, `"light_sprinkles"`, puts one row of pink sprinkles along the
#' middle of the light dough. Sprinkles are drawn at a fixed size, so they are
#' left out automatically when the dough is too thin to hold them. They never
#' go in the hole.
#'
#' Stripes and sprinkles decorate donuts with a hole only. A Berliner, a donut
#' with no hole (data with mean zero, or the solid square drawn beside a
#' donut), stays plain, so that no ring can be mistaken for a hole.
#'
#' Schemes 8 to 10 are flavors, with the dough all one color: strawberry
#' (frosted pink), chocolate and vanilla. Vanilla is so pale that the border of
#' its empty center is drawn in the crust color, so the dough stays easy to see.
#'
#' @return A data frame with columns `id`, `name`, `fill`, `edge` (crust
#'   outline, `NA` for none), `stripe`, `sprinkles` and `inner_edge` (the border
#'   of the empty center; `NA` for the standard light gray).
#'
#' @examples
#' donut_schemes()
#' @export
donut_schemes <- function() {
  data.frame(id = 1:10,
    name   = c("golden", "light", "golden_pinkglaze", "golden_whiteglaze",
               "light_pinkglaze", "light_whiteglaze", "light_sprinkles",
               "strawberry", "chocolate", "vanilla"),
    fill   = c(rep(c("#E3B072", "#F2D6AF", "#E3B072", "#F2D6AF"),
                   c(1, 1, 2, 3)),
               "#F4A6B8", "#6B4226", "#FFF2E4"),
    edge   = c(rep(c("#9C6430", NA, "#9C6430", NA), c(1, 1, 2, 3)),
               "#C9607A", "#3B2314", "#C8A27A"),
    stripe = c(NA, NA, "#F4A6B8", "#FFF2E4", "#F4A6B8", "#FFF2E4", NA,
               NA, NA, NA),
    sprinkles = c(rep(NA, 6), "#F4A6B8", NA, NA, NA),
    inner_edge = c(rep(NA, 9), "#C8A27A"),
    stringsAsFactors = FALSE)
}

# Border of the empty center: the scheme's inner_edge, or the standard light gray
inner_border <- function(sch) {
  if (isTRUE(!is.na(sch$inner_edge)))
    list(colour = sch$inner_edge, linewidth = 0.6)
  else
    list(colour = "grey80", linewidth = 0.4)
}

# Look up a scheme by number or name; fill_dough / edge_dough override it.
resolve_scheme <- function(color = 1, fill_dough = NULL, edge_dough = NULL) {
  sch <- donut_schemes()
  i <- if (is.numeric(color)) match(color, sch$id) else match(color, sch$name)
  if (length(color) != 1 || is.na(i))
    stop("`color` must be one of 1-", nrow(sch), " or a name: ",
         paste(sch$name, collapse = ", "), ". See donut_schemes().",
         call. = FALSE)
  out <- as.list(sch[i, ])
  if (!is.null(fill_dough)) out$fill <- fill_dough
  if (!is.null(edge_dough)) out$edge <- edge_dough
  out
}
