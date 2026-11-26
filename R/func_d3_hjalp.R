# Diagramfunktionerna som func_d3_hjalp() listar. En ny diagramtyp läggs till här.
.vgr_d3_anvandarfunktioner <- c(
  func_d3_karta          = "Koropletkarta (func_graf_karta) — gradient eller grupper (group), tidsreglage, nivå/förändring, bubblor",
  func_d3_karta_change   = "Förändringskarta (func_graf_karta_change)",
  func_d3_karta_heatmap  = "Täthetskarta för punkter (func_graf_karta_heatmap)",
  func_d3_linjediagram   = "Linjediagram (func_graf_linjediagram) — index/förändring, prognos, zoom",
  func_d3_ytdiagram      = "Ytdiagram — staplade ytor eller 100 %, samma argument som linjediagrammet",
  func_d3_stapeldiagram  = "Stapeldiagram (func_graf_barchart) — grupperad/staplad/100 %, sortering",
  func_d3_barchart       = "Samma som func_d3_stapeldiagram, under originalets namn",
  func_d3_dumbbell       = "Hanteldiagram (func_graf_dumbbell) — två perioder per kategori",
  func_d3_cirkeldiagram  = "Cirkel-/munkdiagram (parametrar som func_graf_linjediagram + pie_*)",
  func_d3_spridningsdiagram = "Spridningsdiagram (func_graf_spridningsdiagram) — grupper, bubblor, trendlinje, zoom, sök, år",
  func_d3_scatterplot    = "Samma som func_d3_spridningsdiagram, under ett engelskt namn",
  func_d3_histogram      = "Histogram (func_graf_histogram) — antal/andel, staplat/bredvid/överlagrat, år",
  func_d3_ladagram       = "Lådagram (func_graf_ladagram) — grupper, liggande, punkter, sortering, år",
  func_d3_boxplot        = "Samma som func_d3_ladagram, under ett engelskt namn"
)

#' Lista funktionerna i funcVGRD3
#'
#' Skriver ut diagramfunktionerna med en kort beskrivning, och vilka lager som finns för
#' `+`-syntaxen. Varje funktion har en egen hjälpsida, t.ex. `?func_d3_karta`.
#'
#' @return `NULL`, osynligt. Funktionen anropas för utskriften.
#' @seealso [d3_plot()] för `+`-syntaxen.
#' @export
#' @example inst/examples/func_d3_hjalp.R
func_d3_hjalp <- function() {
  # Bara det som verkligen lästes in (en fil som inte gick att läsa ska inte listas)
  inlasta <- names(.vgr_d3_anvandarfunktioner) %in% ls(environment(func_d3_hjalp), all.names = TRUE)
  cat("vgr_D3 — interaktiva D3-diagram. Anropa som func_graf_*-originalen, t.ex.\n",
      "  func_d3_linjediagram(df, x_value = \"ar\", y_value = \"antal\", group = \"kommun\")\n\n", sep = "")
  cat(sprintf("  %-24s %s\n", names(.vgr_d3_anvandarfunktioner)[inlasta], .vgr_d3_anvandarfunktioner[inlasta]), sep = "")
  if (exists("func_d3_spara", envir = environment(func_d3_hjalp), inherits = FALSE))
    cat(sprintf("  %-24s %s\n", c("func_d3_spara(p, fil)", "func_d3_oppna(fil)"),
                c("Sparar som .html, .png eller .svg", "Öppnar en sparad fil, t.ex. i webbläsaren")), sep = "")
  # Lager vars diagramtyp inte kunde läsas in listas inte (t.ex. d3_dumbbell utan func_d3_dumbbell)
  lager <- sort(grep("^d3_", ls(environment(func_d3_hjalp)), value = TRUE))
  lager <- Filter(function(l) {
    typ <- regmatches(paste(deparse(body(get(l, envir = environment(func_d3_hjalp)))), collapse = " "),
                      regexpr('(?<=typ = ")[a-z_]+', paste(deparse(body(get(l, envir = environment(func_d3_hjalp)))), collapse = " "), perl = TRUE))
    length(typ) == 0 || exists(paste0("func_d3_", typ), envir = environment(func_d3_hjalp), inherits = FALSE)
  }, lager)
  cat("\n  \"+\"-syntax: d3_plot(df) + d3_line(x_value = ..., y_value = ...) + d3_labs(title = ...)\n",
      "  Lager: ", paste(lager, collapse = ", "), "\n",
      "  Parametrarna förklaras på varje funktions hjälpsida, t.ex. ?func_d3_karta.\n",
      sep = "")
  invisible(NULL)
}
