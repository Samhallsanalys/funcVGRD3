# Regressionstest: func_d3_ladagram() ger samma lådor som func_graf_ladagram() i funcVGR.
# Användning: Rscript tests/regression/test_ladagram_paritet.R <funcVGRD3-katalog> <funcVGR-katalog>
# Statistiken räknas i båda paketen var för sig (funcVGRD3 beror inte på funcVGR), så det här
# testet är det som håller dem lika. Skriver ingenting till disk.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2)
d3_dir  <- normalizePath(args[1], winslash = "/")
vgr_dir <- normalizePath(args[2], winslash = "/")

suppressMessages(pkgload::load_all(d3_dir, quiet = TRUE))
# funcVGR kräver keyring men inte för lådagrammet. Saknas det läses R/-filerna in direkt.
if (requireNamespace("keyring", quietly = TRUE)) {
  graf <- get("func_graf_ladagram", envir = suppressMessages(pkgload::load_all(vgr_dir, quiet = TRUE))$env)
} else {
  graf <- suppressMessages(suppressWarnings(roxygen2::load_source(vgr_dir)))$func_graf_ladagram
}

set.seed(1)
df <- data.frame(
  region  = sample(c("Göteborg", "Borås", "Skövde", NA), 600, replace = TRUE, prob = c(.4, .3, .25, .05)),
  kon     = sample(c("Kvinnor", "Män", NA), 600, replace = TRUE, prob = c(.48, .48, .04)),
  ar      = sample(c("2023", "2024"), 600, replace = TRUE),
  inkomst = round(rlnorm(600, log(300), 0.4))
)
df$inkomst[c(5, 50)] <- NA
df <- rbind(df, data.frame(region = "Ale", kon = "Män", ar = "2024", inkomst = 250))  # en låda med ett värde

fran_vgr <- function(b, group, facet) {
  data.frame(k = if (!is.null(facet)) as.character(b[[facet]]) else "", x = as.character(b$.x),
             g = if (!is.null(group)) as.character(b[[group]]) else "",
             n = b$.n, q1 = b$.q1, median = b$.median, q3 = b$.q3, mean = b$.mean,
             lower = b$.lower, upper = b$.upper, stringsAsFactors = FALSE)
}
fran_d3 <- function(b, group, facet) {
  data.frame(k = if (!is.null(facet)) as.character(b$.f) else "", x = as.character(b$.x),
             g = if (!is.null(group)) as.character(b$.g) else "",
             n = b$n, q1 = b$q1, median = b$median, q3 = b$q3, mean = b$mean,
             lower = b$lower, upper = b$upper, stringsAsFactors = FALSE)
}
ordna <- function(d) { d <- d[order(d$k, d$x, d$g), ]; rownames(d) <- NULL; d }

fall <- list(
  en_lada      = list(),
  kategorier   = list(x_value = "region"),
  grupper      = list(x_value = "region", group = "kon"),
  utan_morrhar = list(x_value = "region", group = "kon", box_whisker_range = 0),
  hela_spannet = list(x_value = "region", box_whisker_range = Inf),
  smalt        = list(x_value = "region", box_whisker_range = 0.5),
  paneler      = list(x_value = "region", group = "kon", facet_var = "ar"),
  liggande     = list(x_value = "region", group = "kon", horizontal = TRUE)
)
fel <- 0
for (namn in names(fall)) {
  a <- c(list(df = df, y_value = "inkomst", debug = TRUE), fall[[namn]])
  v <- tryCatch(suppressMessages(suppressWarnings(do.call(graf, a))), error = function(e) e)
  d <- tryCatch(suppressMessages(suppressWarnings(do.call(func_d3_ladagram, a))), error = function(e) e)
  if (inherits(v, "error") || inherits(d, "error")) {
    cat("FEL ", namn, ": ", conditionMessage(if (inherits(v, "error")) v else d), "\n", sep = ""); fel <- fel + 1; next
  }
  bv <- ordna(fran_vgr(v$boxes, fall[[namn]]$group, fall[[namn]]$facet_var))
  bd <- ordna(fran_d3(d$boxes, fall[[namn]]$group, fall[[namn]]$facet_var))
  lika <- isTRUE(all.equal(bv, bd, tolerance = 1e-12))
  cat(if (lika) "OK  " else "FEL ", namn, " (", nrow(bv), " lådor)\n", sep = "")
  if (!lika) { fel <- fel + 1; print(all.equal(bv, bd)) }
}

# sort_by: samma kategoriordning i båda
for (s in c("value_asc", "value_desc", "name_asc", "name_desc")) {
  a <- list(df = df, x_value = "region", y_value = "inkomst", sort_by = s, debug = TRUE)
  v <- suppressMessages(suppressWarnings(do.call(graf, a)))$categories
  d <- suppressMessages(suppressWarnings(do.call(func_d3_ladagram, a)))$categories
  lika <- identical(as.character(v), as.character(d))
  cat(if (lika) "OK  " else "FEL ", "sort_by = ", s, "\n", sep = "")
  if (!lika) fel <- fel + 1
}

# interactive_filter_column: lådorna för varje filtervärde = func_graf_ladagram på just de raderna
d <- suppressMessages(suppressWarnings(func_d3_ladagram(df, x_value = "region", y_value = "inkomst", group = "kon",
                                                        interactive_filter_column = "ar", debug = TRUE)))$boxes
for (k in unique(df$ar)) {
  v <- suppressMessages(suppressWarnings(graf(df[df$ar == k, ], x_value = "region", y_value = "inkomst",
                                              group = "kon", debug = TRUE)))$boxes
  lika <- isTRUE(all.equal(ordna(fran_vgr(v, "kon", NULL)), ordna(fran_d3(d[d$.k == k, ], "kon", NULL)), tolerance = 1e-12))
  cat(if (lika) "OK  " else "FEL ", "filtervärde ", k, "\n", sep = "")
  if (!lika) fel <- fel + 1
}

# box_whisker_range = NA ger samma begripliga fel i båda
for (fn in list(graf = graf, d3 = func_d3_ladagram)) {
  e <- tryCatch(fn(df, y_value = "inkomst", box_whisker_range = NA_real_, debug = TRUE), error = function(e) conditionMessage(e))
  ok <- is.character(e) && grepl("box_whisker_range must be", e)
  cat(if (ok) "OK  " else "FEL ", "box_whisker_range = NA ger felmeddelandet\n", sep = "")
  if (!ok) fel <- fel + 1
}
cat(if (fel == 0) "\nAlla fall ger samma lådor\n" else sprintf("\n%d fall skiljer sig\n", fel))
quit(status = if (fel == 0) 0 else 1)
