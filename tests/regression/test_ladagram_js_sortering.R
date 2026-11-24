# Regressionstest: sorteringen efter median i inst/js/ladagram.js (funcVGRD3 0.14.0).
# Användning: Rscript tests/regression/test_ladagram_js_sortering.R <funcVGRD3-katalog>
# Kräver R-paketet V8. Plockar ut jämförelsefunktionen ur ladagram.js och kör den, så att
# testet följer filen. Före 0.14.0-rättningen gav två kategorier utan median (null/undefined)
# jämförelsen 1 åt båda hållen, vilket är en ogiltig sorteringsfunktion. Skriver ingenting till disk.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1)
js_fil <- file.path(normalizePath(args[1], winslash = "/"), "inst", "js", "ladagram.js")
if (!requireNamespace("V8", quietly = TRUE)) stop("Testet kräver R-paketet V8")

js <- paste(readLines(js_fil, encoding = "UTF-8", warn = FALSE), collapse = "\n")
# Kroppen i jämförelsen efter medianen: från "var sign" till sort-anropets slut
m <- regmatches(js, regexpr("var sign = [^\n]*\n\\s*return cats\\.slice\\(\\)\\.sort\\(function \\(a, b\\) \\{[\\s\\S]*?\\n\\s*\\}\\);", js, perl = TRUE))
if (!length(m)) stop("Hittar inte sorteringen efter median i ", js_fil)
kropp <- sub("^var sign = [^\n]*\n\\s*return cats\\.slice\\(\\)\\.sort\\(", "", m, perl = TRUE)
kropp <- sub("\\);$", "", kropp)

ctx <- V8::v8()
ctx$eval(paste0("var gor = function (med, sign) { return ", kropp, "; };"))
ctx$eval("
  var med = {A: 3, B: null, C: 1, D: undefined, E: 2};
  var cats = ['B', 'A', 'D', 'C', 'E'];
  function kontroll(sign) {
    var c = gor(med, sign), fel = 0;
    cats.forEach(function (x) { cats.forEach(function (y) {
      if (Math.sign(c(x, y)) !== -Math.sign(c(y, x))) fel++;
    }); });
    var s = cats.slice().sort(c);
    return {fel: fel, sist: s.slice(-2).sort().join(','), ordning: s.slice(0, 3).join(',')};
  }
")
fel <- 0
for (sign in c(1, -1)) {
  r <- ctx$call("kontroll", sign)
  namn <- if (sign == 1) "value_asc" else "value_desc"
  ok1 <- r$fel == 0
  ok2 <- identical(r$sist, "B,D")
  ok3 <- identical(r$ordning, if (sign == 1) "C,E,A" else "A,E,C")
  cat(if (ok1) "OK  " else "FEL ", namn, ": jämförelsen är symmetrisk (", r$fel, " fel)\n", sep = "")
  cat(if (ok2) "OK  " else "FEL ", namn, ": kategorier utan median står sist\n", sep = "")
  cat(if (ok3) "OK  " else "FEL ", namn, ": övriga sorteras efter medianen (", r$ordning, ")\n", sep = "")
  fel <- fel + sum(!c(ok1, ok2, ok3))  # inte `fel + !ok1 + ...`: ! binder svagare än + i R
}
cat(if (fel == 0) "\nSorteringen är rätt\n" else sprintf("\n%d kontroller misslyckades\n", fel))
quit(status = if (fel == 0) 0 else 1)
