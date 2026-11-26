# Regressionstest: y_break_line_label_side i func_d3_dumbbell (tillagt i 0.15.0, efter önskemålet från
# bildspelet foredrag_den_langa_bluffen, där texterna vid två närliggande EU-linjer krockade).
# Användning: Rscript tests/regression/test_dumbbell_label_side.R <funcVGRD3-katalog>
# Del 1 kontrollerar spec:en (R), del 2 var texterna ritas (SVG via chromote; hoppas över om
# chromote eller Chrome/Edge saknas). Skriver bara till tempfile().
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1)
suppressMessages(pkgload::load_all(normalizePath(args[1], winslash = "/"), quiet = TRUE, export_all = FALSE))
spec_av <- get("func_d3_spec", envir = asNamespace("funcVGRD3"))

fel <- 0
kontroll <- function(namn, villkor) {
  ok <- isTRUE(villkor)
  cat(if (ok) "OK  " else "FEL ", namn, "\n", sep = "")
  if (!ok) fel <<- fel + 1
}
ger_fel <- function(uttryck) inherits(try(uttryck, silent = TRUE), "try-error")

d <- data.frame(land = rep(c("A", "B", "C"), 2), grupp = rep(c("In", "Ut"), each = 3),
                v = c(0.8, 0.75, 0.7, 0.7, 0.6, 0.72))
linjer <- function(...) func_d3_dumbbell(d, "land", "v", group = "grupp", y_axis_format = "percent",
                                         y_break_line = c(0.68, 0.76), y_break_line_label = c("Ut", "In"), ...)

# ---------------------------------------------------------------- del 1: spec
kontroll("standard: höger", identical(unlist(spec_av(linjer())$value_ref$side), "right"))
kontroll("en per linje", identical(unlist(spec_av(linjer(y_break_line_label_side = c("left", "right")))$value_ref$side),
                                    c("left", "right")))
kontroll("felaktigt värde ger fel", ger_fel(spec_av(linjer(y_break_line_label_side = "center"))))
kontroll("tomt värde ger fel", ger_fel(spec_av(linjer(y_break_line_label_side = character(0)))))

# ---------------------------------------------------------------- del 2: ritat diagram (SVG)
kan_rita <- requireNamespace("chromote", quietly = TRUE) &&
  !inherits(try(chromote::find_chrome(), silent = TRUE), "try-error") && nzchar(chromote::find_chrome() %||% "")
if (!kan_rita) {
  cat("Del 2 hoppas över: chromote eller Chrome/Edge saknas.\n")
} else {
  # Texten vid referenslinjerna: x och text-anchor för "Ut" och "In" (de enda texterna med just det innehållet
  # som har ett text-anchor-attribut; gruppnamnen i förklaringen har det inte)
  rita <- function(p) {
    f <- tempfile(fileext = ".svg")
    suppressMessages(func_d3_spara(p + d3_size(width = 900, height = 500), f, delay = 1))
    x <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    t <- regmatches(x, gregexpr('<text[^>]*text-anchor="[a-z]+"[^>]*>(Ut|In)</text>', x))[[1]]
    data.frame(t = sub(".*>(Ut|In)</text>$", "\\1", t),
               x = as.numeric(sub('.*\\bx="([-0-9.]+)".*', "\\1", t)),
               a = sub('.*text-anchor="([a-z]+)".*', "\\1", t))
  }
  r <- rita(linjer(y_break_line_label_side = c("left", "right")))
  r0 <- rita(linjer())
  kontroll("liggande: två texter ritas", nrow(r) == 2 && nrow(r0) == 2)
  kontroll("liggande: vänster ger text-anchor end", identical(r$a[r$t == "Ut"], "end"))
  kontroll("liggande: höger ger text-anchor start", identical(r$a[r$t == "In"], "start"))
  kontroll("liggande: vänster flyttar texten 10 px", isTRUE(all.equal(r0$x[r0$t == "Ut"] - r$x[r$t == "Ut"], 10)))
  r <- rita(linjer(dumbbell_horizontal = FALSE, y_break_line_label_side = "left"))
  kontroll("stående: vänster ger text-anchor start", nrow(r) == 2 && all(r$a == "start"))
}

cat(if (fel == 0) "\nAlla kontroller gick igenom\n" else paste0("\n", fel, " kontroller misslyckades\n"))
quit(status = if (fel == 0) 0 else 1)
