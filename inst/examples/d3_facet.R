# Påhittade data: sysselsatta per år, kommun och kön
syss <- expand.grid(ar = 2020:2024, kommun = c("Göteborg", "Borås", "Skövde"),
                    kon = c("Kvinnor", "Män"), stringsAsFactors = FALSE)
syss$antal <- round(c(Göteborg = 40000, Borås = 17000, Skövde = 9000)[syss$kommun] *
                      (1 + 0.01 * (syss$ar - 2020)) * ifelse(syss$kon == "Män", 1.05, 1))

# Linjediagram med en panel per kön
p1 <- d3_plot(syss) +
  d3_line(x_value = "ar", y_value = "antal", group = "kommun", legend_position = "bottom") +
  d3_facet(var = "kon")
if (interactive()) p1   # visas i RStudio:s Viewer

# Stapeldiagram med panelerna under varandra och egen skala i varje panel
p2 <- d3_plot(syss[syss$ar == 2024, ]) +
  d3_bar(x_value = "kommun", y_value = "antal") +
  d3_facet(var = "kon", ncol = 1, same_y_scale = FALSE)
if (interactive()) p2

# Ett cirkeldiagram per kön
p3 <- d3_plot(syss[syss$ar == 2024, ]) +
  d3_pie(x_value = "kommun", y_value = "antal", legend_position = "bottom") +
  d3_facet(var = "kon")
if (interactive()) p3

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Linjediagram per kön" = p1, "Stapeldiagram per kön" = p2,
                   "Cirkeldiagram per kön" = p3),
              tempfile(fileext = ".html"))
