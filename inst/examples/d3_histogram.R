# Påhittade data: ålder och kön för 1 000 personer
set.seed(2)
personer <- data.frame(alder = round(runif(1000, 18, 80)),
                       kon   = sample(c("Kvinnor", "Män"), 1000, replace = TRUE))

# Histogram: data i d3_plot(), diagramtyp och kolumn i d3_histogram()
p1 <- d3_plot(personer) + d3_histogram(x_value = "alder")
if (interactive()) p1   # visas i RStudio:s Viewer

# Tioårsklasser per kön, med rubriker i ett eget lager
p2 <- d3_plot(personer) +
  d3_histogram(x_value = "alder", group = "kon", bin_width = 10) +
  d3_labs(title = "Åldersfördelning", x_axis_title = "Ålder")
if (interactive()) p2

# Spara båda diagrammen i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Histogram" = p1, "Per kön" = p2), tempfile(fileext = ".html"))
