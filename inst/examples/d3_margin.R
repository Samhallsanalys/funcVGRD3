# Västra Götalands kommuner (datasetet vg_kommuner), med påhittade värden
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)
karta <- d3_plot(kommuner) +
  d3_map(df_value = "andel", geom_col = "geometry", data_label_show = FALSE) +
  d3_labs(title = "Arbetslöshet")

# Mer luft runt kartan (i punkter)
k1 <- karta + d3_margin(top = 30, right = 40, bottom = 30, left = 40)
if (interactive()) k1   # visas i RStudio:s Viewer

# Bara en marginal ändras; de andra behåller sina värden
k2 <- karta + d3_margin(left = 120)
if (interactive()) k2

# Spara alla kartor i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Standardmarginaler" = karta, "Större marginaler" = k1,
                   "Stor vänstermarginal" = k2),
              tempfile(fileext = ".html"))
