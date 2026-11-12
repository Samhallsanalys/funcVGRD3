# Västra Götalands kommuner (datasetet vg_kommuner) som bakgrundskarta
kommuner <- vg_kommuner

# Påhittade arbetsställen i en spiral kring varje kommuns mittpunkt, tätare i fyra städer
mitt <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(kommuner)))
antal <- ifelse(kommuner$kommun %in% c("Göteborg", "Borås", "Trollhättan", "Skövde"), 40, 4)
punkter <- do.call(rbind, lapply(seq_len(nrow(mitt)), function(i) {
  steg <- seq_len(antal[i])
  data.frame(x = mitt[i, 1] + sqrt(steg) * 1500 * cos(steg * 2.4),
             y = mitt[i, 2] + sqrt(steg) * 1500 * sin(steg * 2.4),
             anstallda = steg %% 30 + 1)
}))
punkter <- sf::st_as_sf(punkter, coords = c("x", "y"), crs = 3006)

# Täthetskarta: punkter i d3_plot(), diagramtyp och parametrar i d3_heatmap()
k1 <- d3_plot(punkter) + d3_heatmap(df_value = "anstallda", basemap = kommuner)
if (interactive()) k1   # visas i RStudio:s Viewer

# Färre konturnivåer, egna färger och utan de enskilda punkterna
k2 <- d3_plot(punkter) +
  d3_heatmap(df_value = "anstallda", basemap = kommuner, bins = 6,
             low_color = "#b3ebf2", high_color = "#005b89", show_points = FALSE,
             legend_position = "right")
if (interactive()) k2

# Spara båda kartorna i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Täthetskarta" = k1, "Färre nivåer utan punkter" = k2),
              tempfile(fileext = ".html"))
