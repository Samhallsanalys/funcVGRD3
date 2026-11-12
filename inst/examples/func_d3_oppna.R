arbl <- data.frame(ar = 2015:2024,
                   andel = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4))
linje <- func_d3_linjediagram(arbl, x_value = "ar", y_value = "andel", title = "Arbetslöshet")

# Spara diagrammet och spara sökvägen i en variabel
fil <- tempfile(fileext = ".html")
func_d3_spara(linje, fil)

# Öppna filen i webbläsaren. Det startar ett annat program och görs därför bara i en
# interaktiv session.
if (interactive()) func_d3_oppna(fil)

# Finns filen inte blir det ett fel
try(func_d3_oppna(file.path(tempdir(), "finns_inte.html")))
