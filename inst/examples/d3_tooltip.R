# Håll muspekaren över diagrammen för att se tooltipen

# Karta: enhet, en extra kolumn och rangordning med lägst värde som plats 1
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)
k <- d3_plot(kommuner) +
  d3_map(df_value = "andel", geom_col = "geometry", data_label_show = FALSE) +
  d3_tooltip(unit = " %", columns = c("Kommunkod" = "kommunkod"), rank_order = "asc")
if (interactive()) k   # visas i RStudio:s Viewer

# Linjediagram: bara linjen under muspekaren, med en decimal
arbl <- data.frame(
  ar     = rep(2015:2024, 2),
  region = rep(c("Västra Götaland", "Riket"), each = 10),
  andel  = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4,
             7.4, 7.0, 6.7, 6.3, 6.8, 8.5, 8.8, 7.5, 7.7, 8.4)
)
p1 <- d3_plot(arbl) +
  d3_line(x_value = "ar", y_value = "andel", group = "region") +
  d3_tooltip(mode = "nearest", decimals = 1, unit = " %")
if (interactive()) p1

# Hanteldiagram: förändringen bara i procentenheter, inte i procent
period <- data.frame(kommun = rep(c("Göteborg", "Borås", "Skövde"), 2),
                     ar = rep(c("2019", "2024"), each = 3), andel = c(6.1, 7.2, 4.9, 6.8, 7.9, 5.1))
p2 <- d3_plot(period) +
  d3_dumbbell(x_value = "kommun", y_value = "andel", group = "ar") +
  d3_tooltip(change_percent = FALSE, unit = " %")
if (interactive()) p2

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Karta" = k, "Linjediagram" = p1, "Hanteldiagram" = p2),
              tempfile(fileext = ".html"))
