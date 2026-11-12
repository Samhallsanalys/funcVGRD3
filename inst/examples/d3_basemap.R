# Västra Götalands kommuner (datasetet vg_kommuner), med påhittade värden
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)

# Länet som bakgrund (alla kommuner sammanslagna), och data för bara några kommuner
lan <- sf::st_sf(namn = "Västra Götaland", geometry = sf::st_union(sf::st_geometry(kommuner)))
urval <- kommuner[as.integer(kommuner$kommunkod) < 1450, ]

# Bakgrundskartan med egen färg; kartan visar hela länet
k1 <- d3_plot(urval) +
  d3_map(df_value = "andel", geom_col = "geometry", data_label_show = FALSE) +
  d3_basemap(lan, color = "#eeeeee", color_line = "#999999")
if (interactive()) k1   # visas i RStudio:s Viewer

# fit = FALSE zoomar till kommunerna med data i stället för hela bakgrunden
k2 <- d3_plot(urval) +
  d3_map(df_value = "andel", geom_col = "geometry") +
  d3_basemap(lan, color = "#eeeeee", fit = FALSE)
if (interactive()) k2

# Spara båda kartorna i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Hela länet som bakgrund" = k1, "Zoomat till data" = k2),
              tempfile(fileext = ".html"))
