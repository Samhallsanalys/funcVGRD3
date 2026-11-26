# Påhittade data: arbetslöshet 2019 och 2024 per kommun
kommuner <- c("Göteborg", "Borås", "Trollhättan", "Skövde", "Uddevalla", "Alingsås")
arbl <- data.frame(kommun = rep(kommuner, 2),
                   ar     = rep(c("2019", "2024"), each = 6),
                   andel  = c(6.1, 7.2, 9.8, 4.9, 6.6, 4.2,
                              6.8, 7.9, 9.4, 5.1, 6.2, 4.8),
                   antal  = c(20100, 3700, 2700, 1400, 1900, 800,
                              23600, 4100, 2700, 1500, 1800, 1000))

# Enklaste anropet: bara de obligatoriska parametrarna. Håll muspekaren över en hantel för
# att se förändringen, och välj sortering i menyn ovanför diagrammet.
p1 <- func_d3_dumbbell(arbl, x_value = "kommun", y_value = "andel", group = "ar")
if (interactive()) p1   # visas i RStudio:s Viewer

# Linjerna färgade efter ökning eller minskning, pilar, etiketter på båda punkterna,
# sorterat efter förändring, en markerad kommun och antalet arbetslösa per år i tooltipen
p2 <- func_d3_dumbbell(arbl, x_value = "kommun", y_value = "andel", group = "ar",
                       line_color_by_change = TRUE, show_direction_arrow = TRUE,
                       tooltip_columns = c("Antal arbetslösa" = "antal"),
                       data_label_position = "both", sort_axis_by = "change",
                       highlight_x_values = "Borås", title = "Förändring 2019–2024")
if (interactive()) p2

# Stående hantlar med förklaringen under diagrammet
p3 <- func_d3_dumbbell(arbl, x_value = "kommun", y_value = "andel", group = "ar",
                       dumbbell_horizontal = FALSE, legend_position = "bottom")
if (interactive()) p3

# Större text för en export (text_scale skalar alla textstorlekar), bara lodräta
# stödlinjer och ingen linje längs y-axeln
p4 <- func_d3_dumbbell(arbl, x_value = "kommun", y_value = "andel", group = "ar",
                       text_scale = 1.3, show_major_grid = "x", y_axis_line = FALSE,
                       title = "Arbetslöshet 2019 och 2024")
if (interactive()) p4

# Andelar (0–1) som procent med förändringen i procentenheter, punktens storlek efter
# antalet arbetslösa, streckade linjer för ett snitt per år, kommunerna i dataordning med
# den första överst och en underrubrik som radbryts efter 50 tecken
arbl$kvot <- arbl$andel / 100
p5 <- func_d3_dumbbell(arbl, x_value = "kommun", y_value = "kvot", group = "ar",
                       y_axis_format = "percent", point_size_column = "antal", point_size = 7,
                       y_break_line = c(0.065, 0.068), y_break_line_label = c("Snitt 2019", "Snitt 2024"),
                       y_break_line_color = c("#1a9fb3", "#ffc107"),
                       sort_axis_by = "original", reverse_categories = TRUE, subtitle_width = 50,
                       subtitle = "Punktens storlek visar antalet arbetslösa. Streckade linjer: snittet för kommunerna.")
if (interactive()) p5

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog).
# Namnen i listan blir rubriker i filen.
func_d3_spara(list("Hanteldiagram" = p1, "Färgat efter förändring" = p2,
                   "Stående hantlar" = p3, "Större text och lodräta stödlinjer" = p4,
                   "Andelar, storlek och snittlinjer" = p5),
              tempfile(fileext = ".html"))
