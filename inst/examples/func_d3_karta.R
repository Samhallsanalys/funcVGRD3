# Västra Götalands kommuner (datasetet vg_kommuner), med påhittade värden
kommuner <- vg_kommuner
kommuner$andel <- round(3 + (as.integer(kommuner$kommunkod) %% 89) / 10, 1)

# Enklaste anropet: bara de obligatoriska parametrarna (och df_value). Håll muspekaren
# över en kommun för rangordning, scrolla för att zooma och sök efter en kommun: välj ett
# namn i listan så zoomar kartan dit, och krysset i sökrutan tömmer den. På en pekskärm
# visar första trycket på en kommun tooltipen och nästa tryck zoomar dit.
karta1 <- func_d3_karta(kommuner, df_value = "andel", geom_col = "geometry")
if (interactive()) karta1   # visas i RStudio:s Viewer

# Förklaring, rubrik, kvantilklasser med en decimal och utan etiketter
karta2 <- func_d3_karta(kommuner, df_value = "andel", geom_col = "geometry",
                        title = "Arbetslöshet", legend_position = "right",
                        legend_title = "Andel (%)", breaks_method = "quantile",
                        breaks_decimals = 1, data_label_show = FALSE)
if (interactive()) karta2

# Kategorier i stället för en färgskala
kommuner$niva <- ifelse(kommuner$andel >= 6, "Hög", "Låg")
karta3 <- func_d3_karta(kommuner, group = "niva", geom_col = "geometry",
                        colors = c("Hög" = "#1a9fb3", "Låg" = "#ffc107"),
                        data_label_show = FALSE)
if (interactive()) karta3

# Tidsreglage: en rad per kommun och år. Knapparna växlar mellan nivå och förändring
# sedan första året, och color_negative ger en röd skala för minskning. Varje kommun
# får en egen årlig förändring som aldrig är noll.
kod <- as.integer(kommuner$kommunkod)
per_ar <- do.call(rbind, lapply(2020:2024, function(a) {
  x <- kommuner
  x$ar <- a
  x$andel <- round(x$andel + (a - 2022) * (((kod %% 7) - 3) / 4 + 0.1), 1)
  x
}))
# Andelen är i procent, så förändringen är i procentenheter: tooltip_unit_change och
# legend_title_change anger det i förändringsläget, och breaks_custom_change ger
# förändringen egna klassgränser runt noll. I förändringsläget har tidsreglaget två
# handtag, så att både startår och slutår kan väljas (interactive_time_range = FALSE ger
# ett handtag och förändring från första året).
karta4 <- func_d3_karta(per_ar, df_value = "andel", geom_col = "geometry",
                        time_column = "ar", legend_position = "bottom",
                        breaks_decimals = 1, data_label_show = FALSE,
                        color_negative = "#b2182b", tooltip_unit = " %",
                        tooltip_unit_change = " procentenheter",
                        legend_title_change = "Förändring (procentenheter)",
                        breaks_custom_change = c(-1, 0, 1))
if (interactive()) karta4

# Etiketter som visas först när man har zoomat in så långt att de inte krockar, med egen
# etikettext (värde och procenttecken) och textfärg som anpassas efter områdets färg.
# Passar kartor med många små områden. data_label_zoom_overlap = 0.1 tillåter bara 10 %
# överlappning mellan etiketterna (standard 40 %), så man behöver zooma in mer.
kommuner$etikett <- paste0(format(kommuner$andel, decimal.mark = ","), " %")
karta5 <- func_d3_karta(kommuner, df_value = "andel", geom_col = "geometry",
                        legend_position = "right", data_label_column = "etikett",
                        data_label_zoom = TRUE, data_label_zoom_overlap = 0.1,
                        data_label_box = FALSE, data_label_text_color = "auto")
if (interactive()) karta5

# Bred karta med förklaringen till vänster. I ett smalt fönster ritas den om i den bredd
# som finns (responsive = TRUE är standard); under 700 px läggs förklaringen under kartan
# och kartan blir högre (responsive_breakpoint). Prova genom att göra fönstret smalare.
karta6 <- func_d3_karta(kommuner, df_value = "andel", geom_col = "geometry",
                        legend_position = "left", legend_title = "Andel (%)",
                        data_label_show = FALSE, width = 1000, height = 700,
                        responsive_breakpoint = 700)
if (interactive()) karta6

# Kartytan går att dra till ny storlek: dra i kartytans hörn nere till höger, så visar en
# streckad ram den nya storleken. Kartan behåller sin skala (resize_keep_scale = TRUE är
# standard), så en större ruta visar mer runt kartan och en mindre ruta visar en del av den;
# panorera för att se resten. Zoom, år och dolda klasser följer med. Dubbelklick på hörnet
# eller Återställ ger tillbaka 600 × 650. Hela kartan, med förklaringen, blir som minst
# 400 × 350 och som störst 1200 × 1000.
karta7 <- func_d3_karta(kommuner, df_value = "andel", geom_col = "geometry",
                        legend_position = "right", data_label_show = FALSE,
                        width = 600, height = 650, interactive_resize = TRUE,
                        resize_min = c(400, 350), resize_max = c(1200, 1000))
if (interactive()) karta7

# Samma karta, men kartan ritas om så att den fyller den dragna storleken
karta8 <- karta7 + d3_size(resize_keep_scale = FALSE)
if (interactive()) karta8

# Summeringsrad under kartan för de kommuner som syns helt i kartytan: summan av
# befolkningen, andelen sysselsatta (kvoten mellan två summor) och antalet kommuner. Raden
# räknas om när man zoomar, byter år eller döljer klasser i förklaringen. Kryssrutan i
# raden (interactive_exclude) tonar ned kommunerna med storstad = TRUE och räknar bort dem.
per_ar$bef <- 5000 + (kod %% 50) * 2000
per_ar$sys <- round(per_ar$bef * (0.70 + (kod %% 11) / 100))
per_ar$storstad <- per_ar$kommun %in% c("Göteborg", "Mölndal", "Partille")
karta9 <- func_d3_karta(per_ar, df_value = "andel", geom_col = "geometry",
                        time_column = "ar", legend_position = "right",
                        data_label_show = FALSE,
                        summary_text = "Kommunerna som visas, {period}: {bef} invånare · {andel} sysselsatta · {antal}",
                        summary_sum = c(bef = "bef"),
                        summary_ratio = list(andel = c("sys", "bef")),
                        summary_count_label = c("kommun", "kommuner"),
                        interactive_exclude = c("Exkludera storstäderna" = "storstad"))
if (interactive()) karta9

# Spara alla kartor ovan i samma fristående HTML-fil (här i en temporär katalog).
# Namnen i listan blir rubriker i filen.
func_d3_spara(list("Koropletkarta" = karta1, "Kvantilklasser" = karta2,
                   "Kategorier" = karta3, "Tidsreglage med nivå och förändring" = karta4,
                   "Etiketter vid zoom" = karta5, "Anpassas till fönstrets bredd" = karta6,
                   "Dra för att visa ett annat utsnitt" = karta7,
                   "Dra för att ändra kartans storlek" = karta8,
                   "Summeringsrad och kryssruta" = karta9),
              tempfile(fileext = ".html"))
