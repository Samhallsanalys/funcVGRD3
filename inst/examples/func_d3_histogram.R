# Påhittade data: ålder och kön för 2 000 personer, och för tre år
set.seed(1)
personer <- data.frame(
  alder = round(c(rnorm(1000, 42, 13), rnorm(1000, 47, 15))),
  kon   = rep(c("Kvinnor", "Män"), each = 1000),
  lan   = sample(c("Västra Götaland", "Skåne"), 2000, replace = TRUE)
)
personer <- personer[personer$alder >= 16 & personer$alder <= 90, ]

# Enklaste anropet: bara de obligatoriska parametrarna (intervallen väljs automatiskt).
# Håll muspekaren över en stapel för intervall, antal och andel.
p1 <- func_d3_histogram(personer, x_value = "alder")
if (interactive()) p1   # visas i RStudio:s Viewer

# Femårsklasser, staplat per kön, med medianen som referenslinje. Knapparna växlar
# mellan antal och andel och mellan staplade, grupperade och överlagrade staplar.
p2 <- func_d3_histogram(personer, x_value = "alder", group = "kon", bin_width = 5,
                        x_break_line = "median", x_break_line_label = "Median",
                        x_axis_title = "Ålder", y_axis_title = "Antal", title = "Åldersfördelning")
if (interactive()) p2

# Tre år i en rullista med uppspelning (▶): intervallen ligger still och staplarna
# växer till de nya höjderna
ar <- do.call(rbind, lapply(2022:2024, function(a) {
  d <- personer
  d$ar <- a
  d$alder <- d$alder + (a - 2022)
  d
}))
p3 <- func_d3_histogram(ar, x_value = "alder", bin_width = 5, interactive_filter_column = "ar",
                        histogram_y = "percent", y_axis_title = "Andel", data_labels = TRUE)
if (interactive()) p3

# En panel per län
p4 <- func_d3_histogram(personer, x_value = "alder", facet_var = "lan", bin_width = 10,
                        data_labels = TRUE)
if (interactive()) p4

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Histogram" = p1, "Per kön med median" = p2, "Tre år" = p3, "Per län" = p4),
              tempfile(fileext = ".html"))
