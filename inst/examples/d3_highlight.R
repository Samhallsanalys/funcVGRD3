kommuner <- c("Göteborg", "Borås", "Trollhättan", "Skövde", "Uddevalla")

# Linjediagram: en kommun i egen färg och med tjockare linje
syss <- expand.grid(ar = 2020:2024, kommun = kommuner, stringsAsFactors = FALSE)
syss$antal <- round(20000 + 3000 * nchar(syss$kommun) + 400 * (syss$ar - 2020) * nchar(syss$kommun))
p1 <- d3_plot(syss) +
  d3_line(x_value = "ar", y_value = "antal", group = "kommun", legend_position = "right") +
  d3_highlight(groups = "Borås", color = "#D0573A")
if (interactive()) p1   # visas i RStudio:s Viewer

# Stapeldiagram: en markerad kommun, övriga tonas ned
arbl <- data.frame(kommun = kommuner, andel = c(6.8, 7.9, 9.4, 5.1, 6.2))
p2 <- d3_plot(arbl) +
  d3_bar(x_value = "kommun", y_value = "andel") +
  d3_highlight(x_values = "Borås")
if (interactive()) p2

# Hanteldiagram: två markerade kommuner med en färgad sidostapel
period <- data.frame(kommun = rep(kommuner, 2), ar = rep(c("2019", "2024"), each = 5),
                     andel = c(6.1, 7.2, 9.8, 4.9, 6.6, 6.8, 7.9, 9.4, 5.1, 6.2))
p3 <- d3_plot(period) +
  d3_dumbbell(x_value = "kommun", y_value = "andel", group = "ar") +
  d3_highlight(x_values = c("Borås", "Skövde"), bar = TRUE)
if (interactive()) p3

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Linjediagram" = p1, "Stapeldiagram" = p2, "Hanteldiagram" = p3),
              tempfile(fileext = ".html"))
