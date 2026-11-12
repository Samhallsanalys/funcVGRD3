arbl <- data.frame(
  ar     = rep(2015:2024, 2),
  region = rep(c("Västra Götaland", "Riket"), each = 10),
  andel  = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4,
             7.4, 7.0, 6.7, 6.3, 6.8, 8.5, 8.8, 7.5, 7.7, 8.4)
)

# Linjediagram: data i d3_plot(), diagramtyp och x/y i d3_line()
p1 <- d3_plot(arbl) + d3_line(x_value = "ar", y_value = "andel", group = "region")
if (interactive()) p1   # visas i RStudio:s Viewer

# Fler parametrar till func_d3_linjediagram() i samma lager, med fullständiga namn
p2 <- d3_plot(arbl) +
  d3_line(x_value = "ar", y_value = "andel", group = "region",
          legend_position = "bottom", data_labels = TRUE, data_label_first_last = TRUE,
          data_label_decimals = 1)
if (interactive()) p2

# Datum på x-axeln
manad <- data.frame(manad = seq(as.Date("2023-01-01"), by = "month", length.out = 24),
                    antal = round(100 + 10 * sin(1:24 / 2)))
p3 <- d3_plot(manad) + d3_line(x_value = "manad", y_value = "antal", x_date_labels = "%b %Y")
if (interactive()) p3

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Linjediagram" = p1, "Med etiketter" = p2, "Datum på x-axeln" = p3),
              tempfile(fileext = ".html"))
