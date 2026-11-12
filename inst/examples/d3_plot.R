arbl <- data.frame(
  ar     = rep(2015:2024, 2),
  region = rep(c("Västra Götaland", "Riket"), each = 10),
  andel  = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4,
             7.4, 7.0, 6.7, 6.3, 6.8, 8.5, 8.8, 7.5, 7.7, 8.4)
)

# d3_plot() tar emot data, och ett diagramtypslager (här d3_line()) väljer diagramtypen
p1 <- d3_plot(arbl) +
  d3_line(x_value = "ar", y_value = "andel", group = "region")
if (interactive()) p1   # visas i RStudio:s Viewer

# Parametrar kan också anges direkt i d3_plot(), med fullständiga namn
p2 <- d3_plot(arbl, x_value = "ar", y_value = "andel", group = "region",
              title = "Arbetslöshet", legend_position = "bottom") +
  d3_line()
if (interactive()) p2

# Samma data och parametrar som stapeldiagram: bara diagramtypslagret byts
p3 <- d3_plot(arbl[arbl$ar >= 2020, ], x_value = "ar", y_value = "andel", group = "region") +
  d3_bar()
if (interactive()) p3

# Utan diagramtyp går objektet inte att rita
try(print(d3_plot(arbl)))

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog).
# Namnen i listan blir rubriker i filen.
func_d3_spara(list("Linjediagram" = p1, "Parametrar i d3_plot()" = p2, "Stapeldiagram" = p3),
              tempfile(fileext = ".html"))
