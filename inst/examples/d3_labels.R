arbl <- data.frame(
  ar     = rep(2015:2024, 2),
  region = rep(c("Västra Götaland", "Riket"), each = 10),
  andel  = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4,
             7.4, 7.0, 6.7, 6.3, 6.8, 8.5, 8.8, 7.5, 7.7, 8.4)
)

# Etiketter på första och sista året, med en decimal
p1 <- d3_plot(arbl) +
  d3_line(x_value = "ar", y_value = "andel", group = "region") +
  d3_labels(show = TRUE, first_last = TRUE, decimals = 1)
if (interactive()) p1   # visas i RStudio:s Viewer

# Vita etiketter mitt i staplade staplar
syss <- data.frame(kommun = rep(c("Göteborg", "Borås", "Trollhättan"), 2),
                   kon    = rep(c("Kvinnor", "Män"), each = 3),
                   antal  = c(42000, 18000, 9500, 44000, 19500, 10200))
p2 <- d3_plot(syss) +
  d3_bar(x_value = "kommun", y_value = "antal", group = "kon", bar_type = "stacked") +
  d3_labels(show = TRUE, position = "middle", color = "white", decimals = 0)
if (interactive()) p2

# Kommunnamn i stället för värdet, med mindre text och utan ruta
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)
k <- d3_plot(kommuner) +
  d3_map(df_value = "andel", geom_col = "geometry") +
  d3_labels(column = "kommun", text_size = 2.2, box = FALSE)
if (interactive()) k

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Första och sista året" = p1, "Mitt i staplarna" = p2, "Kommunnamn" = k),
              tempfile(fileext = ".html"))
