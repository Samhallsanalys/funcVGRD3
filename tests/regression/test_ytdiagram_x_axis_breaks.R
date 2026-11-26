# Regressionstest: x_axis_breaks i func_d3_ytdiagram (tillagt i 0.15.0, efter förslaget från bildspelet
# foredrag_den_langa_bluffen, där helårsdiagrammet behövde årtal vartannat år).
# Användning: Rscript tests/regression/test_ytdiagram_x_axis_breaks.R <funcVGRD3-katalog>
# Kontrollerar spec:en (R). Skriver ingenting.
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
x_av <- function(obj) { s <- spec_av(obj); list(v = as.numeric(unlist(s$x$ticks)), l = unlist(s$x$tick_labels)) }

d <- expand.grid(ar = 1970:2025, grupp = c("A", "B"), stringsAsFactors = FALSE)
d$v <- 1 + (d$ar %% 3)
d$datum <- as.Date(paste0(d$ar, "-01-01"))

# 1. Tal: vartannat år, i stigande ordning, och utan värden utanför data
x <- x_av(func_d3_ytdiagram(d, "ar", "v", group = "grupp", x_axis_breaks = c(seq(2024, 1970, -2), 2030)))
kontroll("tal: vartannat år 1970-2024", identical(x$v, as.numeric(seq(1970, 2024, 2))))
kontroll("tal: etiketterna är årtalen", identical(x$l, as.character(seq(1970, 2024, 2))))

# 2. x_axis_breaks går före x_axis_n_breaks
x <- x_av(func_d3_ytdiagram(d, "ar", "v", group = "grupp", x_axis_breaks = c(1980, 2000), x_axis_n_breaks = 10))
kontroll("tal: går före x_axis_n_breaks", identical(x$v, c(1980, 2000)))

# 3. "whole" tar bort brutna värden, som i stapeldiagrammet
x <- x_av(func_d3_ytdiagram(d, "ar", "v", group = "grupp", x_axis_breaks = c(1990, 1990.5, 2000)))
kontroll("tal: brutna värden tas bort med whole", identical(x$v, c(1990, 2000)))

# 4. Datum: går före x_date_breaks, med x_date_labels
x <- x_av(func_d3_ytdiagram(d, "datum", "v", group = "grupp", x_date_breaks = "5 years", x_date_labels = "%Y",
                            x_axis_breaks = as.Date(c("2010-01-01", "1990-01-01"))))
kontroll("datum: egna skalstreck i ordning", identical(x$l, c("1990", "2010")))
kontroll("datum: värden i ms", identical(x$v, as.numeric(as.Date(c("1990-01-01", "2010-01-01"))) * 86400000))

# 5. Utan argumentet: som förut (breaks_pretty)
x <- x_av(func_d3_ytdiagram(d, "ar", "v", group = "grupp"))
kontroll("utan x_axis_breaks: automatiska skalstreck", identical(x$v, as.numeric(seq(1970, 2020, 10))))

# 6. 100 %-ytor och paneler får samma skalstreck
d$land <- rep(c("X", "Y"), length.out = nrow(d))
x <- x_av(func_d3_ytdiagram(d, "ar", "v", group = "grupp", area_type = "filled", facet_var = "land",
                            x_axis_breaks = seq(1975, 2025, 25)))
kontroll("filled + paneler", identical(x$v, c(1975, 2000, 2025)))

cat(if (fel == 0) "\nAlla kontroller gick igenom\n" else paste0("\n", fel, " kontroller misslyckades\n"))
quit(status = if (fel == 0) 0 else 1)
