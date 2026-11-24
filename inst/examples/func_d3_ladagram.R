# Påhittade data: disponibel inkomst (tkr) för 1 200 personer i fyra delregioner
set.seed(1)
regioner <- c("Göteborgsregionen", "Sjuhärad", "Fyrbodal", "Skaraborg")
inkomst <- data.frame(
  region  = rep(regioner, each = 300),
  kon     = rep(c("Kvinnor", "Män"), 600),
  inkomst = round(c(rlnorm(300, log(320), 0.35), rlnorm(300, log(290), 0.3),
                    rlnorm(300, log(280), 0.3), rlnorm(300, log(285), 0.28)))
)

# Enklaste anropet: en låda för alla värden. Håll muspekaren över lådan för
# kvartiler, median, morrhår och medelvärde.
p1 <- func_d3_ladagram(inkomst, y_value = "inkomst")
if (interactive()) p1   # visas i RStudio:s Viewer

# En låda per delregion, sorterade efter medianen, med medelvärdet som romb och
# medianen som etikett. Rullistan ovanför diagrammet sorterar om lådorna.
p2 <- func_d3_ladagram(inkomst, x_value = "region", y_value = "inkomst",
                       sort_by = "value_desc", box_mean = TRUE, data_labels = TRUE,
                       data_label_decimals = 0, y_axis_title = "Disponibel inkomst (tkr)",
                       title = "Inkomst per delregion", tooltip_unit = " tkr")
if (interactive()) p2

# Kvinnor och män bredvid varandra, liggande lådor och medianen som referenslinje.
# Klicka på förklaringen för att dölja eller visa ett kön.
p3 <- func_d3_ladagram(inkomst, x_value = "region", y_value = "inkomst", group = "kon",
                       horizontal = TRUE, y_break_line = "median", y_break_line_label = "Median")
if (interactive()) p3

# Tre år i en rullista med uppspelning (▶): lådorna glider till de nya värdena
ar <- do.call(rbind, lapply(2022:2024, function(a) {
  d <- inkomst
  d$ar <- a
  d$inkomst <- round(d$inkomst * (1 + 0.04 * (a - 2022)))
  d
}))
p4 <- func_d3_ladagram(ar, x_value = "region", y_value = "inkomst", interactive_filter_column = "ar")
if (interactive()) p4

# Samma diagram under det engelska namnet, med alla värden som punkter
p5 <- func_d3_boxplot(inkomst[inkomst$region %in% regioner[1:2], ], x_value = "region",
                      y_value = "inkomst", point_show = TRUE)
if (interactive()) p5

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Lådagram" = p1, "Per delregion" = p2, "Per kön" = p3, "Tre år" = p4, "Med punkter" = p5),
              tempfile(fileext = ".html"))
