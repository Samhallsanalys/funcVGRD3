# Västra Götalands kommuner (datasetet vg_kommuner), med påhittad befolkning 2022–2024.
# Varje kommun växer eller minskar med en egen takt.
kommuner <- vg_kommuner
kod <- as.integer(kommuner$kommunkod)
bef <- do.call(rbind, lapply(2022:2024, function(a) {
  takt <- 1 + ((kod %% 11) - 4) / 100 + 0.005
  transform(kommuner, ar = a, befolkning = round((5000 + (kod %% 97) * 900) * takt^(a - 2022)))
}))

# Förändringskarta: förändringen från första till senaste året
k1 <- d3_plot(bef) +
  d3_map_change(df_value = "befolkning", geom_col = "geometry", year_column = "ar")
if (interactive()) k1   # visas i RStudio:s Viewer

# Procentuell förändring med röd skala för minskning och förklaring till höger
k2 <- d3_plot(bef) +
  d3_map_change(df_value = "befolkning", geom_col = "geometry", year_column = "ar",
                percentage_growth = TRUE, breaks_decimals = 1, color_negative = "#b2182b",
                legend_position = "right", legend_title = "Förändring (%)")
if (interactive()) k2

# Byta diagramtyp: en koropletkarta med tidsreglage görs om till en förändringskarta.
# time_column byts automatiskt till förändringskartans namn year_column.
niva <- d3_plot(bef) + d3_map(df_value = "befolkning", geom_col = "geometry", time_column = "ar")
k3 <- niva + d3_map_change()
if (interactive()) k3

# Spara alla kartor ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Förändring" = k1, "Procentuell förändring" = k2,
                   "Bytt från koropletkarta" = k3),
              tempfile(fileext = ".html"))
