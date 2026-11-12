# Västra Götalands kommuner (datasetet vg_kommuner), med påhittade värden
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)

# Koropletkarta: data i d3_plot(), diagramtyp och kartans parametrar i d3_map()
k1 <- d3_plot(kommuner) + d3_map(df_value = "andel", geom_col = "geometry")
if (interactive()) k1   # visas i RStudio:s Viewer

# Fler parametrar till func_d3_karta() i samma lager, med fullständiga namn
k2 <- d3_plot(kommuner) +
  d3_map(df_value = "andel", geom_col = "geometry", legend_position = "right",
         breaks_method = "quantile", breaks_decimals = 1, data_label_show = FALSE)
if (interactive()) k2

# Kategorier i stället för en färgskala
kommuner$niva <- ifelse(kommuner$andel >= 6, "Hög", "Låg")
k3 <- d3_plot(kommuner) +
  d3_map(group = "niva", geom_col = "geometry", data_label_show = FALSE)
if (interactive()) k3

# Spara alla kartor ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Koropletkarta" = k1, "Kvantilklasser" = k2, "Kategorier" = k3),
              tempfile(fileext = ".html"))
