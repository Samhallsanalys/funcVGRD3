# Påhittade data: sysselsatta per bransch
bransch <- data.frame(
  bransch = c("Vård och omsorg", "Tillverkning", "Handel", "Utbildning", "Bygg",
              "Transport", "Kultur", "Jordbruk"),
  antal   = c(52000, 41000, 33000, 30000, 18000, 12000, 2500, 1200)
)
cirkel <- d3_plot(bransch) + d3_pie(x_value = "bransch", y_value = "antal")

# Munkdiagram med summan och en förklarande text i mitten
p1 <- cirkel + d3_donut(inner_radius = 0.55, center_text = "total", center_label = "sysselsatta")
if (interactive()) p1   # visas i RStudio:s Viewer

# Små branscher (under 5 %) sammanslagna till "Övrigt", störst först, mellanrum och
# rundade hörn
p2 <- cirkel +
  d3_donut(inner_radius = 0.55, other_threshold = 0.05, sort = "desc",
           pad_angle = 1, corner_radius = 4)
if (interactive()) p2

# Spara alla diagram i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Cirkeldiagram" = cirkel, "Munk med summa" = p1, "Med Övrigt" = p2),
              tempfile(fileext = ".html"))
