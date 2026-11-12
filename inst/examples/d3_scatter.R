# Påhittade data: utbildningsnivå och sysselsättning i åtta kommuner
kommuner <- data.frame(
  kommun     = c("Göteborg", "Borås", "Trollhättan", "Skövde", "Uddevalla", "Alingsås",
                 "Lidköping", "Mölndal"),
  region     = c("GR", "Sjuhärad", "Fyrbodal", "Skaraborg", "Fyrbodal", "GR", "Skaraborg", "GR"),
  hogutb     = c(48, 33, 30, 36, 29, 41, 31, 45),
  sysselsatt = c(78, 80, 76, 82, 77, 83, 81, 82)
)

# Spridningsdiagram: data i d3_plot(), diagramtyp och kolumner i d3_scatter()
p1 <- d3_plot(kommuner) + d3_scatter(x_value = "hogutb", y_value = "sysselsatt")
if (interactive()) p1   # visas i RStudio:s Viewer

# Med färg per region, kommunnamn och trendlinje, och rubriker i ett eget lager
p2 <- d3_plot(kommuner) +
  d3_scatter(x_value = "hogutb", y_value = "sysselsatt", group = "region",
             data_labels = TRUE, data_label_column = "kommun", trend_line = TRUE) +
  d3_labs(title = "Utbildning och sysselsättning", x_axis_title = "Eftergymnasial utbildning (%)")
if (interactive()) p2

# Spara båda diagrammen i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Spridningsdiagram" = p1, "Med grupper och trendlinje" = p2),
              tempfile(fileext = ".html"))
