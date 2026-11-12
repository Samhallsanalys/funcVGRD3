# Påhittade data: arbetslöshet per kommun
kommuner <- c("Göteborg", "Borås", "Trollhättan", "Skövde", "Uddevalla", "Alingsås")
arbl <- data.frame(kommun = kommuner, andel = c(6.8, 7.9, 9.4, 5.1, 6.2, 4.8),
                   antal = c(23600, 4100, 2700, 1500, 1800, 1000))

# Enklaste anropet: bara de obligatoriska parametrarna
p1 <- func_d3_stapeldiagram(arbl, x_value = "kommun", y_value = "andel")
if (interactive()) p1   # visas i RStudio:s Viewer

# Sorterade staplar med etiketter, en markerad kommun, en referenslinje och antalet
# arbetslösa som extra rad i tooltipen
p2 <- func_d3_stapeldiagram(arbl, x_value = "kommun", y_value = "andel",
                            sort_by = "value_desc", data_labels = TRUE,
                            highlight_x_values = "Borås",
                            tooltip_columns = c("Antal arbetslösa" = "antal"),
                            y_break_line = 6.5, y_break_line_label = "Snitt VG",
                            title = "Arbetslöshet per kommun")
if (interactive()) p2

# Grupperade staplar per kön, med år i en rullista (▶ spelar upp åren) och
# rikets värde som referens på varje stapel
syss <- expand.grid(ar = 2022:2024, kommun = kommuner[1:4], kon = c("Kvinnor", "Män"),
                    stringsAsFactors = FALSE)
syss$antal <- round(20000 + 900 * (syss$ar - 2022) + 3000 * nchar(syss$kommun) +
                      ifelse(syss$kon == "Män", 1500, 0))
syss$riket <- round(syss$antal * 1.05)
p3 <- func_d3_stapeldiagram(syss, x_value = "kommun", y_value = "antal", group = "kon",
                            interactive_filter_column = "ar",
                            reference_column = "riket", reference_label = "Riket",
                            legend_position = "bottom", tooltip_unit = " personer")
if (interactive()) p3

# Liggande, staplade staplar under originalets namn func_d3_barchart()
p4 <- func_d3_barchart(syss[syss$ar == 2024, ], x_value = "kommun", y_value = "antal",
                       group = "kon", bar_type = "stacked", horizontal = TRUE)
if (interactive()) p4

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog).
# Namnen i listan blir rubriker i filen.
func_d3_spara(list("Stapeldiagram" = p1, "Sorterat med markerad kommun" = p2,
                   "Per kön med år i rullista" = p3, "Liggande, staplade staplar" = p4),
              tempfile(fileext = ".html"))
