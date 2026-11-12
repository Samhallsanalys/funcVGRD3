# Västra Götalands kommuner (datasetet vg_kommuner), med påhittade värden 2020–2024. Varje kommun
# får en egen årlig förändring.
kommuner <- vg_kommuner
kod <- as.integer(kommuner$kommunkod)
per_ar <- do.call(rbind, lapply(2020:2024, function(a) {
  transform(kommuner, ar = a,
            andel = round(3 + (kod %% 89) / 10 + (a - 2022) * (((kod %% 7) - 3) / 4 + 0.1), 1))
}))

# Tidsreglage med uppspelning: en rad per kommun och år, kolumnen anges i d3_time()
k1 <- d3_plot(per_ar) +
  d3_map(df_value = "andel", geom_col = "geometry", data_label_show = FALSE) +
  d3_time(column = "ar")
if (interactive()) k1   # visas i RStudio:s Viewer

# Börja på 2022 och visa inte året i kartans hörn
k2 <- k1 + d3_time(default = 2022, label_show = FALSE)
if (interactive()) k2

# I förändringskartan byts column automatiskt till year_column; tidsreglaget visar
# förändringen från 2020 till det valda året, här 2023
k3 <- d3_plot(per_ar) +
  d3_map_change(df_value = "andel", geom_col = "geometry", data_label_show = FALSE) +
  d3_time(column = "ar", default = 2023)
if (interactive()) k3

# Spara alla kartor ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Tidsreglage" = k1, "Börjar på 2022" = k2, "Förändringskarta" = k3),
              tempfile(fileext = ".html"))
