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

# Egen etikettext ur en kolumn (två rader med "\n") och radbruten kategoriaxel. Med en fast
# y_axis_max ryms inte den högsta etiketten ovanför stapeln; data_label_overflow = "inside"
# lägger den då i vitt inuti stapeln
ink <- data.frame(typ = c("Lön och företagande", "Pension", "Ersättning för långvarigt nedsatt arbetsförmåga"),
                  mdr = c(2193, 120, 45))
ink$etikett <- paste0(ink$mdr, " mdr kr\n(", format(round(100 * ink$mdr / sum(ink$mdr), 1), decimal.mark = ",", trim = TRUE), " %)")
p5 <- func_d3_stapeldiagram(ink, x_value = "typ", y_value = "mdr", data_labels = TRUE,
                            data_label_column = "etikett", data_label_overflow = "inside",
                            y_axis_max = 2200, y_axis_expand = c(0, 0), x_axis_text_wrap = 20)
if (interactive()) p5

# Andelar (0–1) som procent, år som tal med skalstreck vartannat år och en tunn
# stödlinje per år, grå stödlinjer som syns på en projektor och "År 2020" i tooltipen
andel <- data.frame(ar = 2014:2024, andel = c(.71, .72, .72, .73, .74, .74, .72, .73, .75, .76, .76))
p6 <- func_d3_stapeldiagram(andel, x_value = "ar", y_value = "andel", y_axis_format = "percent",
                            x_axis_breaks = seq(2014, 2024, 2), x_axis_minor_breaks = 2014:2024,
                            show_major_grid = "y", show_minor_grid = "x",
                            grid_major_color = "#bdbdbd", grid_minor_color = "#d4d4d4",
                            tooltip_x_format = function(x) paste("År", x))
if (interactive()) p6

# Liggande staplar med den första kategorin överst och förklaringen i samma ordning som
# delarna i stapeln läses
p7 <- func_d3_stapeldiagram(syss[syss$ar == 2024, ], x_value = "kommun", y_value = "antal",
                            group = "kon", bar_type = "stacked", horizontal = TRUE,
                            reverse_categories = TRUE, legend_reverse = TRUE)
if (interactive()) p7

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog).
# Namnen i listan blir rubriker i filen.
func_d3_spara(list("Stapeldiagram" = p1, "Sorterat med markerad kommun" = p2,
                   "Per kön med år i rullista" = p3, "Liggande, staplade staplar" = p4,
                   "Egen etikettext" = p5, "Andelar och år" = p6, "Första kategorin överst" = p7),
              tempfile(fileext = ".html"))
