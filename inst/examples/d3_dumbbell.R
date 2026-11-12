# Påhittade data: arbetslöshet 2019 och 2024 per kommun
arbl <- data.frame(kommun = rep(c("Göteborg", "Borås", "Trollhättan", "Skövde", "Uddevalla"), 2),
                   ar     = rep(c("2019", "2024"), each = 5),
                   andel  = c(6.1, 7.2, 9.8, 4.9, 6.6, 6.8, 7.9, 9.4, 5.1, 6.2))

# Hanteldiagram: data i d3_plot(), diagramtyp och kolumner i d3_dumbbell()
p1 <- d3_plot(arbl) + d3_dumbbell(x_value = "kommun", y_value = "andel", group = "ar")
if (interactive()) p1   # visas i RStudio:s Viewer

# Sorterat efter förändring, med linjerna färgade efter ökning eller minskning
p2 <- d3_plot(arbl) +
  d3_dumbbell(x_value = "kommun", y_value = "andel", group = "ar",
              sort_axis_by = "change", line_color_by_change = TRUE)
if (interactive()) p2

# Spara båda diagrammen i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Hanteldiagram" = p1, "Sorterat efter förändring" = p2),
              tempfile(fileext = ".html"))
