# Västra Götalands kommuner (datasetet vg_kommuner), med påhittade värden
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)

# Fyra påhittade områden (norr/syd och öst/väst om kommunernas mittpunkter), som
# sammanslagna kommuner
mitt <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(kommuner)))
kommuner$omrade <- paste(ifelse(mitt[, 2] > stats::median(mitt[, 2]), "Norr", "Syd"),
                         ifelse(mitt[, 1] > stats::median(mitt[, 1]), "öst", "väst"))
omraden <- stats::aggregate(kommuner["kommunkod"], by = list(omrade = kommuner$omrade),
                            FUN = length)
karta <- d3_plot(kommuner) +
  d3_map(df_value = "andel", geom_col = "geometry", data_label_show = FALSE)

# Områdenas gränser ovanpå kommunerna
k1 <- karta + d3_overlay(omraden)
if (interactive()) k1   # visas i RStudio:s Viewer

# Med områdenas namn, i en annan färg och med tjockare linjer
k2 <- karta + d3_overlay(omraden, label_column = "omrade", color = "#D0573A", line_size = 1.2)
if (interactive()) k2

# Spara båda kartorna i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Områdesgränser" = k1, "Med namn och färg" = k2),
              tempfile(fileext = ".html"))
