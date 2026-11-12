arbl <- data.frame(
  ar     = rep(2015:2024, 2),
  region = rep(c("Västra Götaland", "Riket"), each = 10),
  andel  = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4,
             7.4, 7.0, 6.7, 6.3, 6.8, 8.5, 8.8, 7.5, 7.7, 8.4)
)
linje <- d3_plot(arbl) +
  d3_line(x_value = "ar", y_value = "andel", group = "region", legend_position = "bottom") +
  d3_labs(title = "Arbetslöshet")

# All text 30 % större, t.ex. inför en export till PowerPoint
p1 <- linje + d3_theme(text_scale = 1.3)
if (interactive()) p1   # visas i RStudio:s Viewer

# Annat typsnitt och större rubrik ("serif" finns på alla datorer)
p2 <- linje + d3_theme(font = "serif", title_size = 22)
if (interactive()) p2

# Spara alla diagram i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Standard" = linje, "text_scale = 1.3" = p1, "Typsnitt och rubrikstorlek" = p2),
              tempfile(fileext = ".html"))
