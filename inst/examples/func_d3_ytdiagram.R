# Påhittade data: befolkningen 16–64 år efter ställning på arbetsmarknaden, två länder
set.seed(1)
stallning <- expand.grid(ar = 2005:2024, land = c("Sverige", "Danmark"),
                         grupp = c("Sysselsatta", "Arbetslösa", "Utanför arbetskraften"),
                         stringsAsFactors = FALSE)
stallning$grupp <- factor(stallning$grupp, levels = c("Sysselsatta", "Arbetslösa", "Utanför arbetskraften"))
bas <- c(Sysselsatta = 4.8, Arbetslösa = 0.4, "Utanför arbetskraften" = 1.2)
stallning$miljoner <- round(bas[as.character(stallning$grupp)] *
                              ifelse(stallning$land == "Danmark", 0.55, 1) *
                              (1 + 0.005 * (stallning$ar - 2005) + 0.03 * sin(stallning$ar + as.integer(stallning$grupp))), 2)

# Staplade ytor, en per grupp (den första gruppen överst), med årtal vartannat år på x-axeln
p1 <- func_d3_ytdiagram(stallning[stallning$land == "Sverige", ], x_value = "ar", y_value = "miljoner",
                        group = "grupp", title = "Befolkningen 16–64 år i Sverige",
                        y_axis_title = "Miljoner personer", x_axis_breaks = seq(2006, 2024, 2))
if (interactive()) p1   # visas i RStudio:s Viewer

# 100 %: gruppernas andel varje år, en panel per land. Håll muspekaren över diagrammet för
# värde, andel och summa; klicka i förklaringen för att dölja en grupp (de övriga staplas om)
p2 <- func_d3_ytdiagram(stallning, x_value = "ar", y_value = "miljoner", group = "grupp",
                        area_type = "filled", facet_var = "land", legend_position = "bottom",
                        show_major_grid = "y", y_axis_title = "Andel",
                        tooltip_x_format = function(x) paste("År", x),
                        caption = "Källa: påhittade data")
if (interactive()) p2

# Spara båda diagrammen i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Staplade ytor" = p1, "100 % per land" = p2), tempfile(fileext = ".html"))
