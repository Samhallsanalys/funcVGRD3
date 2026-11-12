# Påhittade data: sysselsatta per bransch
bransch <- data.frame(
  bransch = c("Vård och omsorg", "Tillverkning", "Handel", "Utbildning", "Bygg",
              "Transport", "Kultur", "Jordbruk"),
  antal   = c(52000, 41000, 33000, 30000, 18000, 12000, 2500, 1200)
)

# Enklaste anropet: bara de obligatoriska parametrarna. Bitarna visar andelen i procent.
p1 <- func_d3_cirkeldiagram(bransch, x_value = "bransch", y_value = "antal")
if (interactive()) p1   # visas i RStudio:s Viewer

# En markerad bit som dras ut från mitten
p2 <- func_d3_cirkeldiagram(bransch, x_value = "bransch", y_value = "antal",
                            highlight_groups = "Utbildning",
                            title = "Sysselsatta per bransch")
if (interactive()) p2

# Munkdiagram med summan i mitten, små branscher sammanslagna till "Övrigt" och
# namn och andel i etiketterna i stället för en förklaring
p3 <- func_d3_cirkeldiagram(bransch, x_value = "bransch", y_value = "antal",
                            pie_inner_radius = 0.58, pie_center_text = "total",
                            pie_center_label = "sysselsatta", pie_other_threshold = 0.05,
                            pie_sort = "desc", data_label_format = "x_percent",
                            legend_position = "none")
if (interactive()) p3

# En munk per kön, med år i en rullista (▶ spelar upp åren)
per_kon <- expand.grid(ar = 2022:2024, kon = c("Kvinnor", "Män"),
                       bransch = bransch$bransch[1:5], stringsAsFactors = FALSE)
per_kon$antal <- round(5000 + 1000 * nchar(per_kon$bransch) + 800 * (per_kon$ar - 2022) +
                         ifelse(per_kon$kon == "Män", 2000, 0))
p4 <- func_d3_cirkeldiagram(per_kon, x_value = "bransch", y_value = "antal",
                            facet_var = "kon", pie_inner_radius = 0.45,
                            interactive_filter_column = "ar", legend_position = "bottom")
if (interactive()) p4

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog).
# Namnen i listan blir rubriker i filen.
func_d3_spara(list("Cirkeldiagram" = p1, "En utdragen bit" = p2,
                   "Munkdiagram med summa och Övrigt" = p3, "En munk per kön och år" = p4),
              tempfile(fileext = ".html"))
