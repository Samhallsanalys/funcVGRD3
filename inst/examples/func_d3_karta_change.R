# Västra Götalands kommuner (datasetet vg_kommuner), med påhittad befolkning 2014, 2019 och 2024.
# Varje kommun växer eller minskar med en egen takt, som aldrig är noll.
kommuner <- vg_kommuner
kod <- as.integer(kommuner$kommunkod)
bas <- 5000 + (kod %% 97) * 900
takt <- ((kod %% 11) - 4) / 100 + 0.005          # förändring per femårsperiod
bef <- do.call(rbind, lapply(c(2014, 2019, 2024), function(a) {
  transform(kommuner, ar = a, befolkning = round(bas * (1 + takt)^((a - 2014) / 5)))
}))

# Enklaste anropet: bara de obligatoriska parametrarna (och kolumnen med år). Kartan
# visar förändringen 2014–2024; tidsreglaget visar även förändringen 2014–2019.
karta1 <- func_d3_karta_change(bef, df_value = "befolkning", geom_col = "geometry",
                               year_column = "ar")
if (interactive()) karta1   # visas i RStudio:s Viewer

# Procentuell förändring med röd skala för minskning, förklaring till vänster och
# kommunnamn utanför kartan
karta2 <- func_d3_karta_change(bef, df_value = "befolkning", geom_col = "geometry",
                               year_column = "ar", percentage_growth = TRUE,
                               breaks_num_group = 5, breaks_decimals = 1,
                               color_negative = "#b2182b", legend_position = "left",
                               legend_title = "Förändring (%)",
                               data_label_repel = TRUE, data_label_outside_map = TRUE,
                               data_label_column = "kommun", data_label_show_value = TRUE)
if (interactive()) karta2

# Kartytan går att dra till ny storlek i hörnet nere till höger (interactive_resize); kartan
# behåller sin skala och rutan visar ett annat utsnitt. Tooltipen saknar raden med klassen
# (tooltip_class = FALSE).
karta3 <- func_d3_karta_change(bef, df_value = "befolkning", geom_col = "geometry",
                               year_column = "ar", width = 600, height = 650,
                               interactive_resize = TRUE, tooltip_class = FALSE)
if (interactive()) karta3

# Spara kartorna ovan i samma fristående HTML-fil (här i en temporär katalog).
# Namnen i listan blir rubriker i filen.
func_d3_spara(list("Förändring 2014–2024" = karta1,
                   "Procentuell förändring med namn utanför kartan" = karta2,
                   "Dra för att visa ett annat utsnitt" = karta3),
              tempfile(fileext = ".html"))
