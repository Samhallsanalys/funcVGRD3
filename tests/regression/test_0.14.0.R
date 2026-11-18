# Regressionstester för funcVGRD3 0.14.0. Körs inte av R CMD check (tests/regression är
# undantagen i .Rbuildignore). Kör:
#   Rscript tests/regression/test_0.14.0.R [paketkatalog] [utdatamapp]
# Utan argument testas paketet i arbetskatalogen. Utdata (en logg) skrivs till utdatamappen,
# standard tempdir().
args <- commandArgs(trailingOnly = TRUE)
pkg <- if (length(args) >= 1) args[[1]] else "."
ut  <- if (length(args) >= 2) args[[2]] else tempdir()
suppressMessages(pkgload::load_all(pkg, quiet = TRUE, export_all = TRUE))
ns <- asNamespace("funcVGRD3")

resultat <- character()
test <- function(namn, uttryck) {
  ok <- tryCatch(isTRUE(uttryck), error = function(e) { message("  fel: ", conditionMessage(e)); FALSE })
  resultat[[namn]] <<- if (ok) "OK" else "MISSLYCKADES"
  cat(sprintf("%-66s %s\n", namn, resultat[[namn]]))
}
brp <- ns$func_d3_karta_brytpunkter

# 1. Heltalsgränser och värden med decimaler: etiketterna visar klassernas innehåll. Förut blev
#    [-2, 0) "-2 – -1" fast -0,4 ingår, och [0, 2) "0-1" fast 1,8 ingår.
b <- brp(c(-6.3, -2.1, -0.4, 0, 1.8, 4.9, 9.95, 12), "custom", c(-5, -2, 0, 2, 5, 10))
test("1 förändring med decimaler: -5,0 – -2,1 ... 0,0-1,9",
     identical(b$labels, c("< -5", "-5,0 – -2,1", "-2,0 – -0,1", "0,0-1,9", "2,0-4,9", "5,0-9,9", "10 +")))

b <- brp(c(41.2, 44.7, 63.15, 85), "custom", seq(40, 80, 5))
test("2 nivå med decimaler: 40,0-44,9 ... 75,0-79,9",
     identical(b$labels, c("< 40", paste0(seq(40, 75, 5), ",0-", seq(44, 79, 5), ",9"), "80 +")))

# 3. Heltalsvärden behåller heltalsformen (oförändrat beteende)
b <- brp(c(41, 44, 63, 85), "custom", seq(40, 80, 5))
test("3 nivå med heltal: oförändrat 40-44 ... 75-79",
     identical(b$labels, c("< 40", paste0(seq(40, 75, 5), "-", seq(44, 79, 5)), "80 +")))

# 4. Flyttalsbrus ger ingen decimal i etiketten och ingen fel klass: 3,3 - 8,3 =
#    -5,0000000000000009 visas som -5,0 och hör till klassen från -5, inte "< -5"
v <- c(3.3 - 8.3, -6, 0, 3, 12)
b <- brp(c(-6, 0, 3, 12), "custom", c(-5, -2, 0, 2, 5, 10))
test("4a heltalsvärden: etiketterna är heltal (-5 – -3)", identical(b$labels[2], "-5 – -3"))
# Genom hela func_d3_karta: en kommun går från 8,3 till 3,3. Förändringen ska vara -5 (inte
# -5,0000000000000009) och höra till klassen från -5, som i webbläsaren (karta.js)
k4 <- vg_kommuner
k4 <- rbind(transform(k4, ar = 2020, andel = 8.3), transform(k4, ar = 2021, andel = 8.3 + (seq_len(nrow(k4)) %% 7) - 3))
k4$andel[k4$ar == 2021][1] <- 3.3
dbg <- func_d3_karta(k4, df_value = "andel", geom_col = "geometry", time_column = "ar",
                     breaks_custom_change = c(-5, -2, 0, 2, 5, 10), debug = TRUE)
f1 <- dbg$forandring[dbg$forandring$kommunkod == k4$kommunkod[1], ]
test("4b flyttalsbrus (3,3 - 8,3) blir -5 i klassen från -5",
     identical(f1$.forandring, -5) && identical(ns$func_d3_karta_klass(f1$.forandring, dbg$breaks_forandring), 1L))
b2 <- brp(c(3.3 - 8.3, 5.1 - 0.1, 12), "custom", c(-5, -2, 0, 2, 5, 10))
test("4c flyttalsbrus räknas inte som decimaler i etiketterna", identical(b2$labels[2], "-5 – -3"))

# 5. tooltip_class = FALSE tar bort klassraden; standard är att visa den
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)
s_av <- function(...) ns$func_d3_spec(func_d3_karta(kommuner, df_value = "andel", geom_col = "geometry", ...))
test("5a tooltip_class = FALSE ger class = FALSE i specen",
     identical(s_av(tooltip_class = FALSE)$tooltip$class, FALSE))
test("5b standard visar klassraden", identical(s_av()$tooltip$class, TRUE))
per_ar <- do.call(rbind, lapply(2020:2021, function(a) transform(kommuner, ar = a, befolkning = 1000 + a)))
test("5c func_d3_karta_change har tooltip_class",
     identical(ns$func_d3_spec(func_d3_karta_change(per_ar, df_value = "befolkning", geom_col = "geometry",
                                                     year_column = "ar", tooltip_class = FALSE))$tooltip$class,
               FALSE))

# 7. Tooltipens värde har minst lika många decimaler som klassetiketterna: med standardvärden
#    och värden med decimaler är etiketterna "5,0-6,9", så värdet 9,6 ska inte visas som "10"
test("7a nivå: tooltipens decimaler minst etiketternas (1)", identical(as.numeric(s_av()$modes$niva$decimals), 1))
test("7b angivet tooltip_decimals = 0 gäller ändå", identical(as.numeric(s_av(tooltip_decimals = 0)$modes$niva$decimals), 0))

# 8. R räknar förändringen som karta.js: på nivåvärdena med 15 värdesiffror (som i JSON).
#    a = 61/12 och b = a + 5: i webbläsaren blir förändringen 4,99999999999997 (klass [0, 5)),
#    och R ska komma fram till samma värde och klass
k8 <- vg_kommuner
k8 <- rbind(transform(k8, ar = 2020, v = 61 / 12 + seq_len(nrow(k8))), transform(k8, ar = 2021, v = 0))
k8$v[k8$ar == 2021] <- k8$v[k8$ar == 2020] + c(5, rep(1, nrow(vg_kommuner) - 1))
d8 <- func_d3_karta(k8, df_value = "v", geom_col = "geometry", time_column = "ar",
                    breaks_custom_change = c(0, 5, 10), debug = TRUE)
f8 <- d8$forandring$.forandring[d8$forandring$kommunkod == vg_kommuner$kommunkod[1]]
js8 <- { a <- signif(61 / 12 + 1, 15); b <- signif(k8$v[k8$ar == 2021][1], 15); signif(b - a, 15) }
test("8 förändringen räknas som i karta.js (15 värdesiffror)",
     identical(f8, js8) && identical(ns$func_d3_karta_klass(f8, d8$breaks_forandring), ns$func_d3_karta_klass(js8, d8$breaks_forandring)))

# 9. Förändringsläget: decimaler i ett tidigare år ger decimaler i etiketterna, även om sista
#    årets förändringar är heltal (förändringen 2020-2021 är 1,5)
k9 <- vg_kommuner
k9 <- rbind(transform(k9, ar = 2020, v = 10), transform(k9, ar = 2021, v = 11.5), transform(k9, ar = 2022, v = 13))
s9 <- ns$func_d3_spec(func_d3_karta(k9, df_value = "v", geom_col = "geometry", time_column = "ar",
                                    breaks_custom_change = c(0, 2, 10)))
test("9 förändring: decimaler i ett tidigare år ger '0,0-1,9'", "0,0-1,9" %in% unlist(s9$modes$forandring$class_labels))

# 10. tooltip_class behandlas som tooltip_rank: bara TRUE visar raden
test("10 tooltip_class = NA tar bort raden", identical(s_av(tooltip_class = NA)$tooltip$class, FALSE))

# 11. Förändringslägets rader för start- och slutår (nivåvärden, TT.decimals) har samma decimaler
#     som nivålägets värderad
k11 <- rbind(transform(kommuner, ar = 2020), transform(kommuner, ar = 2021, andel = andel + 0.5))
s11 <- ns$func_d3_spec(func_d3_karta(k11, df_value = "andel", geom_col = "geometry", time_column = "ar"))
test("11 nivåvärden i förändringsläget: samma decimaler som nivåläget",
     identical(as.numeric(s11$tooltip$decimals), as.numeric(s11$modes$niva$decimals)) &&
       identical(as.numeric(s11$tooltip$decimals), 1))

# 12. Heltalsförändringar behåller heltalsetiketter även när nivåvärdena har decimaler
#     (10,5 -> 12,5 osv.); refuter runda 1
k12 <- vg_kommuner
i12 <- seq_len(nrow(k12))
k12 <- rbind(transform(k12, ar = 2020, v = 10.5 + i12 %% 5), transform(k12, ar = 2021, v = 10.5 + i12 %% 5 + i12 %% 4))
s12 <- ns$func_d3_spec(func_d3_karta(k12, df_value = "v", geom_col = "geometry", time_column = "ar",
                                     breaks_custom_change = c(0, 2, 4)))
test("12 heltalsförändring med decimalnivåer: '0-1', '2-3'",
     identical(unlist(s12$modes$forandring$class_labels), c("< 0", "0-1", "2-3", "4 +")))

# 13. Ett område som saknar värde första året men ändras med decimaler mellan senare år
#     (10,5 -> 12,0) ger decimaletiketter, eftersom ett senare startår kan ge förändringen 1,5;
#     övriga områden har heltalsförändringar (refuter runda 2)
k13 <- vg_kommuner
i13 <- seq_len(nrow(k13))
k13 <- rbind(transform(k13, ar = 2020, v = 10.5 + i13 %% 5), transform(k13, ar = 2021, v = 11.5 + i13 %% 5),
             transform(k13, ar = 2022, v = 12.5 + i13 %% 5))
k13$v[k13$ar == 2020][1] <- NA
k13$v[k13$ar == 2021][1] <- 10.5
k13$v[k13$ar == 2022][1] <- 12
s13 <- ns$func_d3_spec(func_d3_karta(k13, df_value = "v", geom_col = "geometry", time_column = "ar",
                                     breaks_custom_change = c(0, 2, 4)))
test("13 saknat första år med decimalförändring ger '0,0-1,9'",
     "0,0-1,9" %in% unlist(s13$modes$forandring$class_labels))
# Med låst startår (interactive_time_range = FALSE) räknas förändringen bara från första året,
# där område 1 saknar värde, så etiketterna förblir heltal (refuter runda 3)
s13b <- ns$func_d3_spec(func_d3_karta(k13, df_value = "v", geom_col = "geometry", time_column = "ar",
                                      breaks_custom_change = c(0, 2, 4), interactive_time_range = FALSE))
test("13b låst startår: heltalsetiketter ('0-1')", "0-1" %in% unlist(s13b$modes$forandring$class_labels))
# Utan tidsreglage finns inget startårshandtag, så startåret är låst även med standardvärdet
# för interactive_time_range (refuter runda 4), i båda funktionerna
s13c <- ns$func_d3_spec(func_d3_karta(k13, df_value = "v", geom_col = "geometry", time_column = "ar",
                                      breaks_custom_change = c(0, 2, 4), interactive_time_slider = FALSE))
s13d <- ns$func_d3_spec(func_d3_karta_change(k13, df_value = "v", geom_col = "geometry", year_column = "ar",
                                             breaks_method = "custom", breaks_custom = c(0, 2, 4),
                                             interactive_time_slider = FALSE))
# R och karta.js bedömer tidsreglaget lika även för värden som inte är strikt logiska (runda 5)
s13e <- ns$func_d3_spec(func_d3_karta(k13, df_value = "v", geom_col = "geometry", time_column = "ar",
                                      breaks_custom_change = c(0, 2, 4), interactive_time_slider = 1))
test("13d slider = 1: specen säger FALSE och etiketterna är heltal",
     identical(s13e$time$slider, FALSE) && "0-1" %in% unlist(s13e$modes$forandring$class_labels))
test("13c utan tidsreglage: heltalsetiketter (func_d3_karta och _change)",
     "0-1" %in% unlist(s13c$modes$forandring$class_labels) &&
       "0-1" %in% unlist(s13d$modes$forandring$class_labels))

# 6. karta.js: klassraden ritas bara när TT.class inte är false, och förändringens klass räknas
#    på värdet med 15 värdesiffror (samma som i R). Kontrolleras i koden; webbläsartestet står
#    i rapporten.
js <- readLines(file.path(pkg, "inst", "js", "karta.js"), encoding = "UTF-8")
test("6a karta.js: klassraden styrs av TT.class", any(grepl("if (TT.class) {", js, fixed = TRUE)))
test("6b karta.js: klassen räknas på det avrundade värdet",
     any(grepl("cl[i] = classify(d[i], ms.breaks)", js, fixed = TRUE)))

writeLines(paste(names(resultat), resultat), file.path(ut, "regressionstest_0.14.0.txt"))
cat("\n", sum(resultat == "OK"), "av", length(resultat), "tester gick igenom\n")
quit(status = if (all(resultat == "OK")) 0 else 1)
