# Västra Götalands kommuner (datasetet vg_kommuner) som bakgrundskarta
kommuner <- vg_kommuner

# Påhittade arbetsställen: punkter i en spiral kring varje kommuns mittpunkt,
# tätare i fyra städer
mitt <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(kommuner)))
antal <- ifelse(kommuner$kommun %in% c("Göteborg", "Borås", "Trollhättan", "Skövde"), 40, 4)
punkter <- do.call(rbind, lapply(seq_len(nrow(mitt)), function(i) {
  steg <- seq_len(antal[i])
  data.frame(x = mitt[i, 1] + sqrt(steg) * 1500 * cos(steg * 2.4),
             y = mitt[i, 2] + sqrt(steg) * 1500 * sin(steg * 2.4),
             kommun = kommuner$kommun[i])
}))
punkter$anstallda <- seq_len(nrow(punkter)) %% 50 + 1
punkter$sektor <- c("Privat", "Kommunal", "Region")[seq_len(nrow(punkter)) %% 3 + 1]
punkter <- sf::st_as_sf(punkter, coords = c("x", "y"), crs = 3006)

# Enklaste anropet: bara de obligatoriska parametrarna. Håll muspekaren över en punkt
# för detaljer och scrolla för att zooma.
karta1 <- func_d3_karta_heatmap(punkter, df_value = "anstallda", basemap = kommuner)
if (interactive()) karta1   # visas i RStudio:s Viewer

# Punkterna färgade per sektor (rullistan visar tätheten för en sektor i taget),
# ett reglage för utjämningen och egna färger på tätheten
karta2 <- func_d3_karta_heatmap(punkter, df_value = "anstallda", basemap = kommuner,
                                group = "sektor", interactive_adjust_values = c(0.3, 0.5, 1),
                                low_color = "#b3ebf2", high_color = "#005b89",
                                heatmap_alpha = 0.35, point_size = 1.2,
                                legend_position = "bottom", tooltip_name_column = "kommun",
                                title = "Arbetsställen per sektor")
if (interactive()) karta2

# Spara båda kartorna ovan i samma fristående HTML-fil (här i en temporär katalog).
# Namnen i listan blir rubriker i filen.
func_d3_spara(list("Täthetskarta" = karta1, "Per sektor med reglage för utjämning" = karta2),
              tempfile(fileext = ".html"))
