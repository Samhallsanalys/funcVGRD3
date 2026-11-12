# Påhittade data: sysselsatta per kommun och kön
syss <- data.frame(kommun = rep(c("Göteborg", "Borås", "Trollhättan", "Skövde"), 2),
                   kon    = rep(c("Kvinnor", "Män"), each = 4),
                   antal  = c(42000, 18000, 9500, 9000, 44000, 19500, 10200, 9800))

# Stapeldiagram: data i d3_plot(), diagramtyp och x/y i d3_bar()
p1 <- d3_plot(syss[syss$kon == "Kvinnor", ]) + d3_bar(x_value = "kommun", y_value = "antal")
if (interactive()) p1   # visas i RStudio:s Viewer

# Grupperade staplar per kön, sorterade efter värde
p2 <- d3_plot(syss) +
  d3_bar(x_value = "kommun", y_value = "antal", group = "kon", sort_by = "value_desc",
         legend_position = "bottom")
if (interactive()) p2

# Liggande, staplade staplar
p3 <- d3_plot(syss) +
  d3_bar(x_value = "kommun", y_value = "antal", group = "kon", bar_type = "stacked",
         horizontal = TRUE)
if (interactive()) p3

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Stapeldiagram" = p1, "Grupperade staplar" = p2, "Liggande, staplade" = p3),
              tempfile(fileext = ".html"))
