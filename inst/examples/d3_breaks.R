# Västra Götalands kommuner (datasetet vg_kommuner), med påhittade värden
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)
karta <- d3_plot(kommuner) +
  d3_map(df_value = "andel", geom_col = "geometry", data_label_show = FALSE) +
  d3_legend(position = "right")

# Fem klasser med lika många kommuner i varje, och en decimal i klassgränserna
k1 <- karta + d3_breaks(method = "quantile", num_group = 5, decimals = 1)
if (interactive()) k1   # visas i RStudio:s Viewer

# Egna klassgränser
k2 <- karta + d3_breaks(method = "custom", custom = c(3, 5, 7, 9, 12))
if (interactive()) k2

# Spara alla kartor i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Standard (lika breda klasser)" = karta, "Kvantiler" = k1,
                   "Egna gränser" = k2),
              tempfile(fileext = ".html"))
