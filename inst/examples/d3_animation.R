# Linjediagram utan animation när det öppnas
arbl <- data.frame(ar = 2015:2024,
                   andel = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4))
p1 <- d3_plot(arbl) +
  d3_line(x_value = "ar", y_value = "andel") +
  d3_animation(duration = 0)
if (interactive()) p1   # visas i RStudio:s Viewer

# Cirkeldiagram som sveper in långsamt
bransch <- data.frame(bransch = c("Vård och omsorg", "Tillverkning", "Handel", "Utbildning"),
                      antal   = c(52000, 41000, 33000, 30000))
p2 <- d3_plot(bransch) +
  d3_pie(x_value = "bransch", y_value = "antal") +
  d3_animation(duration = 2500)
if (interactive()) p2

# Karta med tidsreglage där uppspelningen (▶) visar varje år i 0,6 sekunder
kommuner <- vg_kommuner
kod <- as.integer(kommuner$kommunkod)
per_ar <- do.call(rbind, lapply(2020:2024, function(a) {
  transform(kommuner, ar = a,
            andel = round(3 + (kod %% 89) / 10 + (a - 2022) * (((kod %% 7) - 3) / 4 + 0.1), 1))
}))
k <- d3_plot(per_ar) +
  d3_map(df_value = "andel", geom_col = "geometry", time_column = "ar", data_label_show = FALSE) +
  d3_animation(play_interval = 600)
if (interactive()) k

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Utan animation" = p1, "Långsam animation" = p2, "Snabb uppspelning" = k),
              tempfile(fileext = ".html"))
