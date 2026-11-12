arbl <- data.frame(ar = 2015:2024,
                   andel = c(7.1, 6.9, 6.5, 6.2, 6.4, 8.3, 7.9, 6.8, 7.0, 7.4))
linje <- func_d3_linjediagram(arbl, x_value = "ar", y_value = "andel", title = "Arbetslöshet")
staplar <- func_d3_stapeldiagram(arbl, x_value = "ar", y_value = "andel", title = "Arbetslöshet")

# Ett diagram som fristående, interaktiv HTML-fil som fungerar utan nätverk. Här sparas
# den i en temporär katalog; ange t.ex. "utdata/arbetsloshet.html" för att spara i ett
# projekt. I RStudio är den utskrivna sökvägen en länk som öppnar filen.
func_d3_spara(linje, tempfile(fileext = ".html"))

# Flera diagram i samma HTML-fil: ange en lista. Namnen blir rubriker i filen.
func_d3_spara(list("Linjediagram" = linje, "Stapeldiagram" = staplar),
              tempfile(fileext = ".html"))

# PNG (t.ex. till PowerPoint) och SVG kräver webshot2 respektive chromote och Chrome
# eller Edge. De startar en webbläsare i bakgrunden och körs därför bara i en
# interaktiv session.
if (interactive() && requireNamespace("webshot2", quietly = TRUE)) {
  func_d3_spara(linje, tempfile(fileext = ".png"), zoom = 3)
  func_d3_spara(linje, tempfile(fileext = ".svg"))
}
