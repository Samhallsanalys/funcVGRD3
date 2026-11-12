# Påhittade data: utfall 2015–2024 och prognos 2025–2028 med osäkerhetsband
befolkning <- data.frame(ar = 2015:2028,
                         antal = round(1700 + 15 * (0:13) + 4 * sin(0:13)))
befolkning$lag <- ifelse(befolkning$ar >= 2025, befolkning$antal - 6 * (befolkning$ar - 2024), NA)
befolkning$hog <- ifelse(befolkning$ar >= 2025, befolkning$antal + 6 * (befolkning$ar - 2024), NA)
linje <- d3_plot(befolkning) +
  d3_line(x_value = "ar", y_value = "antal", y_axis_title = "Tusental")

# Streckad linje från 2025, med texten "Prognos"
p1 <- linje + d3_forecast(from = 2025)
if (interactive()) p1   # visas i RStudio:s Viewer

# Med osäkerhetsband och en egen text
p2 <- linje + d3_forecast(from = 2025, label = "Prognos 2025–2028",
                          band_lower = "lag", band_upper = "hog", band_alpha = 0.3)
if (interactive()) p2

# label = NULL tar bort texten
p3 <- p2 + d3_forecast(label = NULL)
if (interactive()) p3

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Prognos" = p1, "Med osäkerhetsband" = p2, "Utan text" = p3),
              tempfile(fileext = ".html"))
