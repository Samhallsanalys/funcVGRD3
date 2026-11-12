# Västra Götalands kommuner (datasetet vg_kommuner), med påhittad andel och befolkning
kommuner <- vg_kommuner
kod <- as.integer(kommuner$kommunkod)
kommuner$andel <- round(3 + (kod %% 89) / 10, 1)
kommuner$befolkning <- 5000 + (kod %% 97) * 900
karta <- d3_plot(kommuner) +
  d3_map(df_value = "andel", geom_col = "geometry", data_label_show = FALSE)

# En bubbla per kommun, med storlek efter befolkningen
k1 <- karta + d3_bubbles(column = "befolkning")
if (interactive()) k1   # visas i RStudio:s Viewer

# Mindre, mörka och genomskinliga bubblor
k2 <- karta + d3_bubbles(column = "befolkning", color = "#37474F", alpha = 0.5, max_size = 14)
if (interactive()) k2

# Spara båda kartorna i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Bubblor" = k1, "Mindre, mörka bubblor" = k2), tempfile(fileext = ".html"))
