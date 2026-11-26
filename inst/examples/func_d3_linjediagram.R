# Påhittade data: antal sysselsatta per år i fem kommuner
kommuner <- c("Göteborg", "Borås", "Trollhättan", "Skövde", "Uddevalla")
syss <- expand.grid(ar = 2015:2024, kommun = kommuner, stringsAsFactors = FALSE)
syss$antal <- round(c(310000, 52000, 29000, 30000, 26000)[match(syss$kommun, kommuner)] *
                      (1 + 0.012 * (syss$ar - 2015) + 0.015 * sin(syss$ar + nchar(syss$kommun))))
syss$kvinnor <- round(syss$antal * (0.47 + 0.02 * cos(syss$ar + nchar(syss$kommun))))

# Enklaste anropet: bara de obligatoriska parametrarna
p1 <- func_d3_linjediagram(syss[syss$kommun == "Borås", ], x_value = "ar", y_value = "antal")
if (interactive()) p1   # visas i RStudio:s Viewer

# En linje per kommun, förklaring till höger och etiketter på första och sista året.
# Håll muspekaren över diagrammet för hårkors, klicka i förklaringen för att dölja en linje
# och växla mellan värde, index och förändring med knapparna. tooltip_columns lägger till
# antalet kvinnor för varje punkt i tooltipen.
p2 <- func_d3_linjediagram(syss, x_value = "ar", y_value = "antal", group = "kommun",
                           title = "Sysselsatta 2015–2024", caption = "Källa: påhittade data",
                           legend_position = "right", data_labels = TRUE,
                           data_label_first_last = TRUE,
                           tooltip_columns = c("Varav kvinnor" = "kvinnor"))
if (interactive()) p2

# Lyft fram två kommuner och tona ned övriga
p3 <- func_d3_linjediagram(syss, x_value = "ar", y_value = "antal", group = "kommun",
                           group_highlight = c("Borås", "Skövde"),
                           title = "Borås och Skövde jämfört med övriga")
if (interactive()) p3

# Namn vid linjeslutet i stället för förklaring, prognos från 2023 och en skuggad period
p4 <- func_d3_linjediagram(syss[syss$kommun != "Göteborg", ], x_value = "ar", y_value = "antal",
                           group = "kommun", legend_direct = TRUE,
                           line_forecast_from = 2023,
                           x_shade_from = 2020, x_shade_to = 2021, x_shade_label = "Pandemin")
if (interactive()) p4

# Andelen kvinnor (0–1) som procent, med mer luft under linjerna, egna färger på
# stödlinjerna och "År 2020" som rubrik i tooltipen
syss$andel_kvinnor <- syss$kvinnor / syss$antal
p5 <- func_d3_linjediagram(syss, x_value = "ar", y_value = "andel_kvinnor", group = "kommun",
                           y_axis_format = "percent", y_axis_expand = c(0.2, 0.15),
                           show_major_grid = TRUE, grid_major_color = "#bdbdbd",
                           tooltip_x_format = function(x) paste("År", x), legend_position = "right")
if (interactive()) p5

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog).
# Namnen i listan blir rubriker i filen.
func_d3_spara(list("En kommun" = p1, "En linje per kommun" = p2,
                   "Två kommuner framhävda" = p3, "Prognos och skuggad period" = p4,
                   "Andel i procent" = p5),
              tempfile(fileext = ".html"))
