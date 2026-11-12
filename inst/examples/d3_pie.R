# Påhittade data: sysselsatta per bransch
bransch <- data.frame(
  bransch = c("Vård och omsorg", "Tillverkning", "Handel", "Utbildning", "Bygg"),
  antal   = c(52000, 41000, 33000, 30000, 18000)
)

# Cirkeldiagram: data i d3_plot(), diagramtyp och kolumner i d3_pie()
p1 <- d3_plot(bransch) + d3_pie(x_value = "bransch", y_value = "antal")
if (interactive()) p1   # visas i RStudio:s Viewer

# Munkdiagram, med fler parametrar till func_d3_cirkeldiagram() i samma lager
p2 <- d3_plot(bransch) +
  d3_pie(x_value = "bransch", y_value = "antal", pie_inner_radius = 0.55,
         pie_center_text = "total", data_label_format = "x_percent", legend_position = "none")
if (interactive()) p2

# Spara båda diagrammen i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Cirkeldiagram" = p1, "Munkdiagram" = p2), tempfile(fileext = ".html"))
