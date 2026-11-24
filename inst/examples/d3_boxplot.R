# Påhittade data: inkomst (tkr) för 600 personer i tre delregioner
set.seed(2)
inkomst <- data.frame(region  = rep(c("Göteborgsregionen", "Sjuhärad", "Fyrbodal"), each = 200),
                      kon     = sample(c("Kvinnor", "Män"), 600, replace = TRUE),
                      inkomst = round(rlnorm(600, log(300), 0.3)))

# Lådagram: data i d3_plot(), diagramtyp och kolumner i d3_boxplot()
p1 <- d3_plot(inkomst) + d3_boxplot(x_value = "region", y_value = "inkomst")
if (interactive()) p1   # visas i RStudio:s Viewer

# Per kön och liggande, med rubriker i ett eget lager
p2 <- d3_plot(inkomst) +
  d3_boxplot(x_value = "region", y_value = "inkomst", group = "kon", horizontal = TRUE) +
  d3_labs(title = "Inkomst per delregion och kön", y_axis_title = "Inkomst (tkr)")
if (interactive()) p2

# Spara båda diagrammen i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Lådagram" = p1, "Per kön" = p2), tempfile(fileext = ".html"))
