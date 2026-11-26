arbk <- data.frame(
  ar     = rep(2015:2024, 3),
  grupp  = rep(c("Sysselsatta", "Arbetslösa", "Utanför"), each = 10),
  tusen  = c(1010, 1022, 1035, 1048, 1055, 1030, 1041, 1066, 1078, 1080,
             62, 60, 57, 55, 59, 78, 74, 62, 64, 70,
             250, 246, 243, 240, 238, 252, 249, 241, 238, 236)
)
arbk$grupp <- factor(arbk$grupp, levels = c("Sysselsatta", "Arbetslösa", "Utanför"))

# Ytdiagram: data i d3_plot(), diagramtyp och x/y i d3_area()
p1 <- d3_plot(arbk) + d3_area(x_value = "ar", y_value = "tusen", group = "grupp")
if (interactive()) p1   # visas i RStudio:s Viewer

# 100 % och rubriker i egna lager
p2 <- d3_plot(arbk) +
  d3_area(x_value = "ar", y_value = "tusen", group = "grupp", area_type = "filled") +
  d3_labs(title = "Andel av befolkningen 16–64 år", y_axis_title = NULL)
if (interactive()) p2

# Spara båda diagrammen i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Staplade ytor" = p1, "100 %" = p2), tempfile(fileext = ".html"))
