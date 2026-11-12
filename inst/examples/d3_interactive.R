# Karta utan sökruta och zoom
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)
k <- d3_plot(kommuner) +
  d3_map(df_value = "andel", geom_col = "geometry", data_label_show = FALSE) +
  d3_interactive(search = FALSE, zoom = FALSE)
if (interactive()) k   # visas i RStudio:s Viewer

# Linjediagram utan knapparna för index och förändring, och utan nedladdningsknappar
arbl <- data.frame(ar = 2015:2024,
                   andel = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4))
p1 <- d3_plot(arbl) +
  d3_line(x_value = "ar", y_value = "andel") +
  d3_interactive(index_toggle = FALSE, export = FALSE)
if (interactive()) p1

# Stapeldiagram med år i en rullista (▶ spelar upp åren), som börjar på 2023
syss <- expand.grid(ar = 2022:2024, kommun = c("Göteborg", "Borås", "Skövde"),
                    stringsAsFactors = FALSE)
syss$antal <- c(41000, 41800, 42500, 17800, 17600, 18100, 9000, 9300, 9100)
p2 <- d3_plot(syss) +
  d3_bar(x_value = "kommun", y_value = "antal") +
  d3_interactive(filter_column = "ar", filter_default = 2023)
if (interactive()) p2

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Karta utan sök och zoom" = k, "Linjediagram utan knappar" = p1,
                   "Stapeldiagram med rullista" = p2),
              tempfile(fileext = ".html"))
