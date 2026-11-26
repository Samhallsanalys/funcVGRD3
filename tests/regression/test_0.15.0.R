# Regressionstest för funcVGRD3 0.15.0: förslagen från bildspelet (data_label_column, x_axis_text_wrap,
# y_axis_expand, x_axis_breaks, grid_*, tooltip_x_format, y_axis_format, reverse_categories,
# legend_reverse, ytdiagram och hanteldiagrammets tillägg).
# Användning: Rscript tests/regression/test_0.15.0.R <funcVGRD3-katalog>
# Del 1 kontrollerar spec:en (R), del 2 det ritade diagrammet (SVG via chromote; hoppas över om
# chromote eller Chrome/Edge saknas). Skriver bara till tempfile().
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1)
suppressMessages(pkgload::load_all(normalizePath(args[1], winslash = "/"), quiet = TRUE, export_all = FALSE))
spec_av <- get("func_d3_spec", envir = asNamespace("funcVGRD3"))
punkt_px <- get("func_d3_punkt_px", envir = asNamespace("funcVGRD3"))

fel <- 0
kontroll <- function(namn, villkor) {
  ok <- isTRUE(villkor)
  cat(if (ok) "OK  " else "FEL ", namn, "\n", sep = "")
  if (!ok) fel <<- fel + 1
}
ger_fel <- function(uttryck) inherits(try(uttryck, silent = TRUE), "try-error")

# ---------------------------------------------------------------- data
ink <- data.frame(typ = c("Lön och företagande", "Pension", "Ersättning för långvarigt nedsatt arbetsförmåga"),
                  mdr = c(2193, 120, 45))
ink$etikett <- c("2193 mdr kr\n(90,0 %)", NA, "45 mdr kr\n(1,8 %)")
norden <- expand.grid(ar = 2000:2010, grupp = c("Sysselsatta", "Arbetslösa", "Utanför"), stringsAsFactors = FALSE)
norden$grupp <- factor(norden$grupp, levels = c("Sysselsatta", "Arbetslösa", "Utanför"))
norden$v <- c(rep(70, 11), rep(5, 11), rep(25, 11))
med <- data.frame(kommun = rep(c("Ale", "Borås", "Skövde"), 2), del = rep(c("Bas", "Ökning"), each = 3),
                  ar = c(40, 41, 42, 1, 2, 3))
med$del <- factor(med$del, levels = c("Ökning", "Bas"))
andel <- data.frame(k = c("A", "B", "C"), a = c(0.823, 0.79, 0.755))
db <- data.frame(g = rep(c("EU27", "Inrikes", "Utrikes"), 2), ar = rep(c("2010", "2025"), each = 3),
                 v = c(0.68, 0.70, 0.55, 0.76, 0.77, 0.62), s = c(4, 1, 0.25, 4, 1, NA))

# ---------------------------------------------------------------- del 1: spec
s1 <- spec_av(func_d3_stapeldiagram(ink, "typ", "mdr", data_labels = TRUE, data_label_column = "etikett",
                                    x_axis_text_wrap = 20))
lt <- setNames(lapply(s1$rows, function(r) r$lt), vapply(s1$rows, function(r) r$xt, ""))
kontroll("1a data_label_column: texten följer med raden", identical(lt[["Lön och företagande"]], "2193 mdr kr\n(90,0 %)"))
kontroll("1b data_label_column: saknad text ger NA (ingen etikett)", is.na(lt[["Pension"]]))
kontroll("1c data_label_column: labels$column = TRUE", isTRUE(s1$labels$column))
kontroll("1d data_label_overflow: standard \"expand\"", identical(s1$labels$overflow, "expand"))
kontroll("1e data_label_overflow: okänt värde ger fel",
         ger_fel(spec_av(func_d3_stapeldiagram(ink, "typ", "mdr", data_label_overflow = "ut"))))
kontroll("2a x_axis_text_wrap: långa kategorier radbryts",
         any(grepl("\n", unlist(s1$x$level_labels))) && length(s1$x$level_labels) == length(s1$x$levels))
kontroll("2b \\n i kategorinamn behålls utan x_axis_text_wrap",
         identical(funcVGRD3:::func_d3_radbryt("Rad 1\nrad 2 som är lång", NULL), "Rad 1\nrad 2 som är lång"))
kontroll("2c x_axis_text_wrap: ingen level_labels när inget radbryts",
         is.null(spec_av(func_d3_stapeldiagram(andel, "k", "a"))$x$level_labels))
s3 <- spec_av(func_d3_stapeldiagram(norden, "ar", "v", group = "grupp", bar_type = "stacked", y_axis_max = 100,
                                    y_axis_expand = c(0, 0), x_axis_breaks = seq(2000, 2010, 2),
                                    x_axis_minor_breaks = 2000:2010, legend_reverse = TRUE,
                                    tooltip_x_format = function(x) paste("År", x)))
kontroll("3a y_axis_expand ersätter standardluften", identical(s3$y$expand, c(0, 0)))
kontroll("3b y_axis_expand: fel längd ger fel",
         ger_fel(spec_av(func_d3_stapeldiagram(andel, "k", "a", y_axis_expand = 0.1))))
kontroll("3c standardluften oförändrad", identical(spec_av(func_d3_stapeldiagram(andel, "k", "a"))$y$expand, c(0.05, 0.15)))
kontroll("4a x_axis_breaks: egna skalstreck", identical(unlist(s3$x$ticks), seq(2000, 2010, 2)))
kontroll("4b x_axis_minor_breaks: en stödlinje per år", identical(unlist(s3$x$minor), as.numeric(2000:2010)))
s4 <- spec_av(func_d3_stapeldiagram(norden, "ar", "v", x_axis_n_breaks = 10))
kontroll("4c x_axis_n_breaks: fler skalstreck än standard",
         length(s4$x$ticks) > length(spec_av(func_d3_stapeldiagram(norden, "ar", "v"))$x$ticks))
d_ar <- data.frame(d = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")), v = 1:3)
s4d <- spec_av(func_d3_stapeldiagram(d_ar, "d", "v", x_axis_breaks = as.Date(c("2020-01-01", "2022-01-01")),
                                     x_date_format = "%Y", tooltip_x_format = "%Y"))
kontroll("4d x_axis_breaks på datumaxel", identical(unlist(s4d$x$tick_labels), c("2020", "2022")))
kontroll("6a tooltip_x_format som datumformat", identical(s4d$rows[[1]]$xl, "2020"))
kontroll("6b tooltip_x_format som funktion", identical(s3$rows[[1]]$xl, "År 2000"))
kontroll("6c tooltip_x_format som text på en icke-datumaxel ger fel",
         ger_fel(spec_av(func_d3_stapeldiagram(andel, "k", "a", tooltip_x_format = "%Y"))))
kontroll("6d utan tooltip_x_format finns inget xl", is.null(spec_av(func_d3_stapeldiagram(andel, "k", "a"))$rows[[1]]$xl))
kontroll("11 legend_reverse", isTRUE(s3$legend$reverse))
s5 <- spec_av(func_d3_stapeldiagram(andel, "k", "a", grid_major_color = "grey50", grid_minor_width = 0.4))
kontroll("5a grid_*: färg som hex med gemener", identical(s5$grid$major_color, "#7f7f7f"))
kontroll("5b grid_*: tjocklek i px", identical(s5$grid$minor_width, round(0.4 * 72.27 / 25.4, 2)))
kontroll("5c grid_*: standard 1,42 och 0,71 px", identical(c(spec_av(func_d3_stapeldiagram(andel, "k", "a"))$grid$major_width,
                                                           spec_av(func_d3_stapeldiagram(andel, "k", "a"))$grid$minor_width), c(1.42, 0.71)))
kontroll("5g grid_*: färg NULL eller okänd ger fel (code-review)",
         ger_fel(spec_av(func_d3_stapeldiagram(andel, "k", "a", grid_major_color = NULL))) &&
         ger_fel(spec_av(func_d3_stapeldiagram(andel, "k", "a", grid_minor_color = "intefärg"))))
kontroll("5d grid_*: negativ tjocklek ger fel", ger_fel(spec_av(func_d3_stapeldiagram(andel, "k", "a", grid_major_width = -1))))
kontroll("5e grid_* i histogram, spridnings- och lådagram",
         identical(spec_av(func_d3_histogram(norden, "v", grid_major_color = "#123456"))$grid$major_color, "#123456") &&
         identical(spec_av(func_d3_spridningsdiagram(norden, "ar", "v", grid_minor_color = "#123456"))$grid$minor_color, "#123456") &&
         identical(spec_av(func_d3_ladagram(norden, "grupp", "v", grid_major_width = 1))$grid$major_width, 2.85))
kontroll("7a y_axis_format = \"percent\" i spec", identical(spec_av(func_d3_stapeldiagram(andel, "k", "a", y_axis_format = "percent"))$y$format, "percent"))
kontroll("7b y_axis_format: okänt värde ger fel", ger_fel(spec_av(func_d3_stapeldiagram(andel, "k", "a", y_axis_format = "procent"))))
kontroll("10a reverse_categories (stapel)", isTRUE(spec_av(func_d3_stapeldiagram(med, "kommun", "ar", reverse_categories = TRUE))$x$reverse))
kontroll("10b reverse_categories gäller inte talaxel", isFALSE(spec_av(func_d3_stapeldiagram(norden, "ar", "v", reverse_categories = TRUE))$x$reverse))

# Linjediagram
sl <- spec_av(func_d3_linjediagram(norden, "ar", "v", group = "grupp", y_axis_format = "percent",
                                   tooltip_x_format = function(x) paste("År", x)))
kontroll("L1 linjediagram: tooltip_x_format i x_text", identical(sl$series[[1]]$x_text[[1]], "År 2000"))
kontroll("L2 linjediagram: y_axis_expand standard c(0.05, 0.15)", identical(sl$y$expand, c(0.05, 0.15)))
kontroll("L3 linjediagram: y_axis_format", identical(sl$y$format, "percent"))
kontroll("L4 linjediagram: ingen area", is.null(sl$area))

# Ytdiagram
sy <- spec_av(func_d3_ytdiagram(norden, "ar", "v", group = "grupp", area_type = "filled"))
kontroll("12a ytdiagram: area_type i spec", identical(sy$area$type, "filled"))
kontroll("12b ytdiagram: y_axis_expand standard c(0, 0) för 100 %", identical(sy$y$expand, c(0, 0)))
kontroll("12c ytdiagram: y_axis_expand standard c(0, 0.05) staplat",
         identical(spec_av(func_d3_ytdiagram(norden, "ar", "v", group = "grupp"))$y$expand, c(0, 0.05)))
kontroll("12d ytdiagram: x-axeln utan luft", identical(sy$x$domain, c(2000, 2010)))
kontroll("12e ytdiagram: inga etiketter eller indexknappar", isFALSE(sy$labels$show) && isFALSE(sy$interactive$index_toggle))
kontroll("12f ytdiagram: okänd area_type ger fel", ger_fel(spec_av(func_d3_ytdiagram(norden, "ar", "v", area_type = "x"))))
kontroll("12g d3_area() i +-syntaxen",
         identical(spec_av(d3_plot(norden) + d3_area(x_value = "ar", y_value = "v", group = "grupp"))$area$type, "stacked"))
kontroll("12j +-syntax: bar_type/bar_line_* följer med till d3_area() (consistency-checker)",
         identical(spec_av(func_d3_stapeldiagram(norden, "ar", "v", group = "grupp", bar_type = "filled", bar_line_color = "red") +
                             d3_area())$area[c("type", "line_color")], list(type = "filled", line_color = "#FF0000")))
kontroll("12k legend_reverse finns i linjediagrammet (d3_area + d3_line)",
         isTRUE(spec_av(func_d3_ytdiagram(norden, "ar", "v", group = "grupp", legend_reverse = TRUE) + d3_line())$legend$reverse))
kontroll("12h ytdiagram: linjediagrammets parametrar finns inte", ger_fel(spec_av(func_d3_ytdiagram(norden, "ar", "v", data_labels = TRUE))))
kontroll("12i ytdiagram: text_scale skalar rubriken",
         spec_av(func_d3_ytdiagram(norden, "ar", "v", title = "T", text_scale = 2))$text$title_size ==
           2 * spec_av(func_d3_ytdiagram(norden, "ar", "v", title = "T"))$text$title_size)

# Hanteldiagram
lang <- paste("Punktens storlek visar gruppens andel av de sysselsatta.", "Streckade linjer visar EU-genomsnittet.")
sd <- spec_av(func_d3_dumbbell(db, "g", "v", "ar", subtitle = lang, point_size_column = "s", point_size = 8,
                               y_break_line = c(0.68, 0.76), y_break_line_label = "EU",
                               y_axis_format = "percent", reverse_categories = TRUE))
kontroll("8a subtitle_width: underrubriken radbryts (standard 60)", length(sd$text$subtitle) >= 2 &&
           all(nchar(unlist(sd$text$subtitle)) <= 60))
kontroll("8b subtitle_width: egna \\n behålls",
         identical(unlist(spec_av(func_d3_dumbbell(db, "g", "v", "ar", subtitle = "A\nB"))$text$subtitle), c("A", "B")))
kontroll("8c subtitle_width = NULL radbryter inte",
         length(spec_av(func_d3_dumbbell(db, "g", "v", "ar", subtitle = lang, subtitle_width = NULL))$text$subtitle) == 1)
storlek <- lapply(sd$rows, function(r) unlist(r$size))
names(storlek) <- vapply(sd$rows, function(r) r$x, "")
kontroll("8d point_size_column: största värdet får point_size", isTRUE(all.equal(storlek$EU27[1], punkt_px(8))))
kontroll("8e point_size_column: ytan proportionell mot värdet",
         isTRUE(all.equal(storlek$Inrikes[1], punkt_px(8 * sqrt(1 / 4)))))
kontroll("8f point_size_column: saknat värde ger point_size", isTRUE(all.equal(storlek$Utrikes[2], punkt_px(8))))
kontroll("8g point_size_column visas i tooltipen", "s" %in% unlist(sd$tooltip$columns))
kontroll("8h point_size_column måste vara numerisk",
         ger_fel(spec_av(func_d3_dumbbell(transform(db, s = as.character(s)), "g", "v", "ar", point_size_column = "s"))))
kontroll("8i y_break_line i spec", identical(unlist(sd$value_ref$v), c(0.68, 0.76)) && identical(unlist(sd$value_ref$label), "EU"))
kontroll("9a tooltip_unit_change: procentenheter vid y_axis_format = \"percent\"", identical(sd$tooltip$unit_change, " procentenheter"))
kontroll("9b tooltip_unit_change: standard = tooltip_unit",
         identical(spec_av(func_d3_dumbbell(db, "g", "v", "ar", tooltip_unit = " kr"))$tooltip$unit_change, " kr"))
kontroll("9c tooltip_unit_change angiven", identical(spec_av(func_d3_dumbbell(db, "g", "v", "ar", tooltip_unit_change = " p.e."))$tooltip$unit_change, " p.e."))
kontroll("10c reverse_categories (hantel)", isTRUE(sd$reverse))
kontroll("5f grid_minor_width i hanteldiagram: standard 0,85 px som förut",
         identical(spec_av(func_d3_dumbbell(db, "g", "v", "ar"))$grid$minor_width, 0.85))

# behavior-verifier (rättade 2026-11-26)
sb2 <- spec_av(func_d3_stapeldiagram(data.frame(k = c("A", "B", "C"), v = c(40, 70, 95)), "k", "v", horizontal = TRUE,
                                     data_labels = TRUE, y_axis_max = 100, y_axis_expand = c(0, 0)))
kontroll("B2 liggande staplar: angiven y_axis_expand får ingen etikettluft", identical(sb2$y$expand, c(0, 0)))
sb3 <- spec_av(func_d3_stapeldiagram(data.frame(ar = 2000:2023, v = 1), "ar", "v", x_axis_n_breaks = 10))
kontroll("B3 \"whole\": inga brutna årtal på talaxeln", all(unlist(sb3$x$ticks) == round(unlist(sb3$x$ticks))))
sb4 <- spec_av(func_d3_dumbbell(transform(db, s = c(4, 1, 0, 4, 1, 0.01)), "g", "v", "ar", point_size_column = "s", point_size = 8))
r0 <- sb4$rows[[which(vapply(sb4$rows, function(r) r$x, "") == "Utrikes")]]$size[[1]]
kontroll("B4 point_size_column: värdet 0 ger den minsta punkten", r0 < punkt_px(8 * sqrt(0.25)))
kontroll("B5 y_break_line: NA tas bort", identical(unlist(spec_av(func_d3_dumbbell(db, "g", "v", "ar", y_break_line = c(0.7, NA)))$value_ref$v), 0.7))
sl2 <- spec_av(func_d3_linjediagram(data.frame(x = 1:4, g = c("a", "a", "b", "b"), v = 1:4), "x", "v", group = "g",
                                    tooltip_x_format = function(x) paste0(x, "/", length(x))))
kontroll("B6 tooltip_x_format får hela x-kolumnen i linjediagrammet", identical(sl2$series[[1]]$x_text[[1]], "1/4"))

# ---------------------------------------------------------------- del 2: ritat diagram (SVG)
kan_rita <- requireNamespace("chromote", quietly = TRUE) &&
  !inherits(try(chromote::find_chrome(), silent = TRUE), "try-error") && nzchar(chromote::find_chrome() %||% "")
if (!kan_rita) {
  cat("Del 2 hoppas över: chromote eller Chrome/Edge saknas.\n")
} else {
  svg <- function(p) {
    f <- tempfile(fileext = ".svg")
    suppressMessages(func_d3_spara(p + d3_size(width = 900, height = 500), f, delay = 1))
    paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }
  nb <- " "
  # Textelement: innehåll och attribut
  texter <- function(x) regmatches(x, gregexpr("<text[^>]*>.*?</text>", x))[[1]]
  y_for <- function(x, txt) {
    t <- grep(paste0(">", txt, "<"), texter(x), value = TRUE, fixed = FALSE)[1]
    as.numeric(sub('.*\\by="([-0-9.]+)".*', "\\1", t))
  }
  x1 <- svg(func_d3_stapeldiagram(ink, "typ", "mdr", data_labels = TRUE, data_label_column = "etikett", x_axis_text_wrap = 20))
  kontroll("S1 etikett med två rader ritas som tspan-rader", grepl("<tspan[^>]*>\\(90,0 %\\)</tspan>", x1))
  kontroll("S2 kategoriaxeln radbryts i tspan-rader", grepl("<tspan[^>]*>arbetsförmåga</tspan>", x1) ||
             grepl("<tspan[^>]*>[^<]*arbetsförmåga</tspan>", x1))
  # behavior-verifier: första radens dy ska ta med textens egen dy (0.85em under axeln, 0.35em i etiketter)
  kontroll("B1a kategoritext med flera rader: första raden dy=0.85em", grepl('<tspan x="[-0-9.]+" dy="0.85em">', x1))
  kontroll("B1b etikett med två rader: första raden dy=-0.25em", grepl('<tspan x="0" dy="-0.25em">2193 mdr kr</tspan>', x1))
  x2 <- svg(func_d3_stapeldiagram(ink, "typ", "mdr", data_labels = TRUE, data_label_column = "etikett",
                                  data_label_overflow = "inside", y_axis_max = 2200, y_axis_expand = c(0, 0)))
  kontroll("S3 data_label_overflow = \"inside\": etiketten blir vit", grepl('fill="#FFFFFF"[^>]*>(<tspan[^>]*>)?2193 mdr kr', x2))
  x3 <- svg(func_d3_stapeldiagram(norden, "ar", "v", group = "grupp", bar_type = "stacked", y_axis_max = 100,
                                  y_axis_expand = c(0, 0), y_axis_breaks = seq(0, 100, 20), legend_reverse = TRUE))
  kontroll("S4 y_axis_expand = c(0, 0): axeln slutar vid 100 (inget 120)", grepl(">100<", x3) && !grepl(">120<", x3))
  leg <- regmatches(x3, gregexpr(">(Sysselsatta|Arbetslösa|Utanför)<", x3))[[1]]
  kontroll("S5 legend_reverse: Utanför först i förklaringen", identical(leg[1], ">Utanför<"))
  x4 <- svg(func_d3_stapeldiagram(med, "kommun", "ar", group = "del", bar_type = "stacked", horizontal = TRUE,
                                  reverse_categories = TRUE))
  kontroll("S6 reverse_categories: Ale överst i liggande staplar", y_for(x4, "Ale") < y_for(x4, "Skövde"))
  x5 <- svg(func_d3_stapeldiagram(andel, "k", "a", y_axis_format = "percent", data_labels = TRUE))
  kontroll("S7 y_axis_format = \"percent\": axeltext och etikett i procent",
           grepl(paste0(">80", nb, "%<"), x5) && grepl(paste0(">82,3", nb, "%<"), x5))
  x6 <- svg(func_d3_stapeldiagram(andel, "k", "a", show_major_grid = TRUE, grid_major_color = "#123456", grid_major_width = 1))
  kontroll("S8 grid_*: stödlinjernas färg och tjocklek", grepl('stroke="#123456" stroke-width="2.85"', x6))
  x7 <- svg(func_d3_ytdiagram(norden, "ar", "v", group = "grupp", area_type = "filled"))
  kontroll("S9 ytdiagram: en yta per grupp", length(gregexpr('class="vgr-area"[^>]*fill="#', x7)[[1]]) == 3)
  kontroll("S10 ytdiagram 100 %: y-axeln i procent", grepl(paste0(">100", nb, "%<"), x7))
  # code-review: dubblettrad ska ge den första raden, och y_break_line får inte bredda 100 %-axeln
  dubb <- rbind(data.frame(ar = 1:3, g = "A", v = c(10, 10, 10)), data.frame(ar = 1:3, g = "B", v = c(10, 10, 10)),
                data.frame(ar = 2, g = "A", v = 1000))
  x7b <- svg(func_d3_ytdiagram(dubb, "ar", "v", group = "g", y_axis_max = NULL))
  kontroll("S9b ytdiagram: dubblettrad ger första värdet (axeln når inte 1 000)", !grepl(">1[  ]?000<", x7b))
  x7c <- svg(func_d3_ytdiagram(dubb[1:6, ], "ar", "v", group = "g", area_type = "filled", y_break_line = 50))
  kontroll("S9c ytdiagram 100 %: y_break_line breddar inte axeln", grepl(paste0(">100", nb, "%<"), x7c) &&
             !grepl(paste0(">1[ ", nb, "]?000", nb, "%<"), x7c))
  x8 <- svg(func_d3_dumbbell(db, "g", "v", "ar", y_break_line = 0.68, y_break_line_label = "EU 2010",
                             y_axis_format = "percent", reverse_categories = TRUE, sort_axis_by = "original",
                             point_size_column = "s"))
  kontroll("S11 y_break_line: streckad linje med text", grepl("stroke-dasharray=\"6 4\"", x8) && grepl(">EU 2010<", x8))
  kontroll("S12 hanteldiagram: procentaxel", grepl(paste0("[0-9]", nb, "%<"), x8))
  kontroll("S13 hanteldiagram reverse_categories: EU27 överst", y_for(x8, "EU27") < y_for(x8, "Utrikes"))
}

cat(if (fel == 0) "\nALLA KONTROLLER GICK IGENOM\n" else paste0("\n", fel, " KONTROLL(ER) MISSLYCKADES\n"))
quit(status = if (fel == 0) 0 else 1)
