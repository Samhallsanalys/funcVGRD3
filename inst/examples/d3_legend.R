arbl <- data.frame(
  ar     = rep(2015:2024, 2),
  region = rep(c("Västra Götaland", "Riket"), each = 10),
  andel  = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4,
             7.4, 7.0, 6.7, 6.3, 6.8, 8.5, 8.8, 7.5, 7.7, 8.4)
)
linje <- d3_plot(arbl) + d3_line(x_value = "ar", y_value = "andel", group = "region")

# Förklaringen under diagrammet, med en egen rubrik
p1 <- linje + d3_legend(position = "bottom", title = "Område")
if (interactive()) p1   # visas i RStudio:s Viewer

# Namnen vid linjernas slut i stället för en förklaring
p2 <- linje + d3_legend(direct = TRUE, position = "none")
if (interactive()) p2

# Kartans förklaring till höger, med runda symboler och större text
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)
k <- d3_plot(kommuner) +
  d3_map(df_value = "andel", geom_col = "geometry", data_label_show = FALSE) +
  d3_legend(position = "right", title = "Andel (%)", key_shape = "circle", text_size = 13)
if (interactive()) k

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Förklaring under" = p1, "Namn vid linjeslutet" = p2, "Kartans förklaring" = k),
              tempfile(fileext = ".html"))
