arbl <- data.frame(
  ar     = rep(2015:2024, 2),
  region = rep(c("Västra Götaland", "Riket"), each = 10),
  andel  = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4,
             7.4, 7.0, 6.7, 6.3, 6.8, 8.5, 8.8, 7.5, 7.7, 8.4)
)

# Parametrar som inte hör till något annat lager: namn vid linjeslutet och en
# referenslinje med text
p1 <- d3_plot(arbl) +
  d3_line(x_value = "ar", y_value = "andel", group = "region") +
  d3_params(legend_direct = TRUE, x_break_line = 2020, x_break_line_label = "Pandemin")
if (interactive()) p1   # visas i RStudio:s Viewer

# I ett stapeldiagram: sortering och en vågrät linje vid medelvärdet
arbl24 <- data.frame(kommun = c("Göteborg", "Borås", "Trollhättan", "Skövde", "Uddevalla"),
                     andel  = c(6.8, 7.9, 9.4, 5.1, 6.2))
p2 <- d3_plot(arbl24) +
  d3_bar(x_value = "kommun", y_value = "andel") +
  d3_params(sort_by = "value_desc", y_break_line = mean(arbl24$andel),
            y_break_line_label = "Medel")
if (interactive()) p2

# Spara båda diagrammen i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Linjediagram" = p1, "Stapeldiagram" = p2), tempfile(fileext = ".html"))
