arbl <- data.frame(ar = 2015:2024,
                   andel = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4))

# Ett lägre och smalare linjediagram (diagrammet krymper ändå för att få plats i fönstret)
p <- d3_plot(arbl) +
  d3_line(x_value = "ar", y_value = "andel") +
  d3_size(width = 700, height = 350)
if (interactive()) p   # visas i RStudio:s Viewer

# En mindre karta. Får den inte plats ritas den om i den bredd som finns (responsive), och
# under 450 px läggs förklaringen under kartan (responsive_breakpoint)
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)
k <- d3_plot(kommuner) +
  d3_map(df_value = "andel", geom_col = "geometry", data_label_show = FALSE,
         legend_position = "right") +
  d3_size(width = 500, height = 560, responsive_breakpoint = 450)
if (interactive()) k

# Samma karta med fast storlek, som före version 0.8.0
k_fast <- k + d3_size(responsive = FALSE)
if (interactive()) k_fast

# Kartan går att dra till ny storlek (d3_interactive(resize = TRUE)); d3_size sätter gränserna
k_dra <- k + d3_interactive(resize = TRUE) +
  d3_size(resize_min = c(350, 300), resize_max = c(900, 900))
if (interactive()) k_dra

# Bara höjden ändras; bredden behåller sitt värde
p2 <- p + d3_size(height = 500)
if (interactive()) p2

# Spara alla diagram ovan i samma fristående HTML-fil (här i en temporär katalog)
func_d3_spara(list("700 × 350" = p, "Karta 500 × 560" = k, "Karta med fast storlek" = k_fast,
                   "Karta som går att dra till ny storlek" = k_dra, "700 × 500" = p2),
              tempfile(fileext = ".html"))
