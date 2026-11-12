arbl <- data.frame(
  ar     = rep(2015:2024, 2),
  region = rep(c("Västra Götaland", "Riket"), each = 10),
  andel  = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4,
             7.4, 7.0, 6.7, 6.3, 6.8, 8.5, 8.8, 7.5, 7.7, 8.4)
)

# En färg per grupp, matchad mot gruppnamnen
p1 <- d3_plot(arbl) +
  d3_line(x_value = "ar", y_value = "andel", group = "region", legend_position = "bottom") +
  d3_colors(colors = c("Västra Götaland" = "#1a9fb3", "Riket" = "#37474F"))
if (interactive()) p1   # visas i RStudio:s Viewer

# Kartans färgskala: mörkaste och ljusaste färg
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)
k <- d3_plot(kommuner) +
  d3_map(df_value = "andel", geom_col = "geometry", legend_position = "right",
         data_label_show = FALSE) +
  d3_colors(color = "#7E57C2", color_low = "#f3e5f5")
if (interactive()) k

# Egen färg för staplar under noll
forandring <- data.frame(kommun = c("Göteborg", "Borås", "Trollhättan", "Skövde"),
                         forandring = c(1.2, -0.8, 2.1, -1.5))
p2 <- d3_plot(forandring) +
  d3_bar(x_value = "kommun", y_value = "forandring") +
  d3_colors(color_negative = "#b2182b")
if (interactive()) p2

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Färg per grupp" = p1, "Kartans färgskala" = k, "Negativa staplar" = p2),
              tempfile(fileext = ".html"))
