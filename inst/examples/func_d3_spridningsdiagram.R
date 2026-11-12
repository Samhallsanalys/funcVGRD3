# Påhittade data: utbildningsnivå och sysselsättning i 18 kommuner, 2020–2024
set.seed(1)
namn <- c("Göteborg", "Borås", "Trollhättan", "Skövde", "Uddevalla", "Alingsås",
          "Lidköping", "Kungsbacka", "Mölndal", "Kungälv", "Lerum", "Partille",
          "Vänersborg", "Falköping", "Mariestad", "Ulricehamn", "Stenungsund", "Tjörn")
kommuner <- data.frame(
  kommun   = namn,
  region   = rep(c("Göteborgsregionen", "Sjuhärad", "Fyrbodal", "Skaraborg"), length.out = 18),
  invanare = round(c(600000, 115000, 60000, 58000, 57000, 42000, 40000, 85000, 70000,
                     47000, 44000, 40000, 40000, 33000, 30000, 25000, 27000, 16000)),
  hogutb   = round(runif(18, 22, 46), 1)
)
kommuner$sysselsatt <- round(70 + 0.25 * kommuner$hogutb + rnorm(18, 0, 2), 1)

# Enklaste anropet: bara de obligatoriska parametrarna. Håll muspekaren över en punkt
# för dess värden, och dra en ruta i diagrammet för att zooma in.
p1 <- func_d3_spridningsdiagram(kommuner, x_value = "hogutb", y_value = "sysselsatt")
if (interactive()) p1   # visas i RStudio:s Viewer

# Färg per region, kommunnamn, trendlinje med konfidensband och referenslinjer vid
# medelvärdena. Klicka på en region i förklaringen för att dölja den, och sök efter en
# kommun i sökrutan.
p2 <- func_d3_spridningsdiagram(kommuner, x_value = "hogutb", y_value = "sysselsatt",
                                group = "region",
                                data_labels = TRUE, data_label_column = "kommun",
                                trend_line = TRUE, trend_line_se = TRUE,
                                x_break_line = "mean", y_break_line = "mean",
                                x_break_line_label = "Medel", y_break_line_label = "Medel",
                                x_axis_title = "Eftergymnasial utbildning (%)",
                                y_axis_title = "Sysselsättningsgrad (%)",
                                title = "Utbildning och sysselsättning")
if (interactive()) p2

# Bubbeldiagram över fem år: punktens storlek efter antal invånare, en rullista med
# uppspelning (▶) där kommunerna glider mellan åren, och invånarna i tooltipen
ar <- do.call(rbind, lapply(2020:2024, function(a) {
  d <- kommuner
  d$ar <- a
  d$hogutb <- round(d$hogutb + 0.6 * (a - 2020) + rnorm(18, 0, 0.4), 1)
  d$sysselsatt <- round(d$sysselsatt + 0.3 * (a - 2020) + rnorm(18, 0, 0.5), 1)
  d
}))
p3 <- func_d3_spridningsdiagram(ar, x_value = "hogutb", y_value = "sysselsatt", group = "region",
                                point_size_column = "invanare", point_size_title = "Invånare",
                                point_border_color = "white", data_label_column = "kommun",
                                interactive_filter_column = "ar", trend_line = TRUE,
                                tooltip_columns = c("Invånare" = "invanare"))
if (interactive()) p3

# Samma diagram under det engelska namnet, en panel och trendlinje per region
p4 <- func_d3_scatterplot(kommuner, x_value = "hogutb", y_value = "sysselsatt", group = "region",
                          facet_var = "region", trend_line = "group", legend_position = "none")
if (interactive()) p4

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Spridningsdiagram" = p1, "Med grupper, etiketter och trendlinje" = p2,
                   "Bubblor över fem år" = p3, "En panel per region" = p4),
              tempfile(fileext = ".html"))
