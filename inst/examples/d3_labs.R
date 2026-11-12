arbl <- data.frame(
  ar     = rep(2015:2024, 2),
  region = rep(c("Västra Götaland", "Riket"), each = 10),
  andel  = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4,
             7.4, 7.0, 6.7, 6.3, 6.8, 8.5, 8.8, 7.5, 7.7, 8.4)
)
linje <- d3_plot(arbl) +
  d3_line(x_value = "ar", y_value = "andel", group = "region", legend_position = "bottom")

# Rubrik, underrubrik, källa, axeltitlar och förklaringens rubrik
p1 <- linje +
  d3_labs(title = "Arbetslöshet 2015–2024", subtitle = "Andel av arbetskraften",
          caption = "Källa: påhittade data", y_axis_title = "Procent", legend_title = "Område")
if (interactive()) p1   # visas i RStudio:s Viewer

# Ett senare d3_labs() byter en text, och NULL tar bort en
p2 <- p1 + d3_labs(title = "Ny rubrik", subtitle = NULL)
if (interactive()) p2

# Fungerar på samma sätt för kartor
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)
k <- d3_plot(kommuner) +
  d3_map(df_value = "andel", geom_col = "geometry", legend_position = "right") +
  d3_labs(title = "Arbetslöshet per kommun", legend_title = "Andel (%)")
if (interactive()) k

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("Texter i ett linjediagram" = p1, "Ny rubrik, ingen underrubrik" = p2,
                   "Texter i en karta" = k),
              tempfile(fileext = ".html"))
