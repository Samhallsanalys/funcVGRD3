#' Rita ett interaktivt stapeldiagram
#'
#' Interaktiv D3-version av `funcVGR::func_graf_barchart()` med samma parametrar och
#' standardvärden. Kan även anropas som `func_d3_barchart()`, originalets namn. Utöver
#' originalets parametrar finns tooltip, växling mellan grupperade, staplade och 100 %
#' staplade staplar, sorteringsmeny, rullista med uppspelning (t.ex. år),
#' klickbar förklaring, markerade kategorier (`highlight_*`), referensvärden per stapel
#' (`reference_*`), egen färg för negativa värden och export till PNG och SVG.
#'
#' @details
#' Diagrammet skalar med fönstret.
#'
#' Funktionen ritar inte diagrammet direkt. Den returnerar ett objekt som sparar de
#' argument du angett, och diagrammet beräknas först när objektet skrivs ut eller sparas.
#' Därför går det att ändra inställningar i efterhand med `+`, t.ex.
#' `p + d3_labs(title = "Ny rubrik")` (se [d3_plot()]).
#'
#' @section Skillnader mot func_graf_barchart():
#' * Etiketternas värden har alltid decimalkomma (originalet ger "5.2" för tal under 1000).
#' * Etiketterna i 100 %-staplar placeras efter andelar (originalet placerar dem efter
#'   råvärdena, utanför skalan).
#' * Liggande staplar med etiketter `"outside"` vänsterjusteras vid stapelns slut
#'   (ggplot2 centrerar). Kategoriaxeln och den automatiska rotationen av x-axelns text
#'   gäller inte liggande staplar.
#' * Med `y_axis_min` inom data klipps staplarna mot panelen.
#' * Dubblettrader (samma panel, x och grupp) summeras och ett meddelande visas.
#' * `NA` i `group` eller `facet_var` blir gruppen "NA", sist och grå.
#' * `custom_theme` (ett ggplot2-tema) används inte; ett meddelande visas.
#'
#' @param df Data frame med data. Obligatorisk.
#' @param x_value Namn på kolumnen med kategorierna (x-axeln), som text, eller en vektor
#'   med värden. Obligatorisk.
#' @param y_value Namn på kolumnen med de numeriska värdena, som text, eller en vektor
#'   med värden. Obligatorisk.
#' @param group Namn på kolumnen som staplarna färgas och grupperas efter, som text.
#'   `NULL` (standard) ger en färg för alla staplar.
#' @param facet_var Namn på kolumnen som delar upp diagrammet i paneler, som text.
#'   `NULL` (standard) ger ett diagram.
#' @param facet_ncol Antal kolumner med paneler. `NULL` (standard) räknas ut
#'   automatiskt.
#' @param facet_scales Panelernas axelskalor: `"fixed"`, `"free"`, `"free_x"` eller
#'   `"free_y"`.
#' @param facet_same_y_scale `TRUE` ger samma värdeaxel i alla paneler, `FALSE`
#'   oberoende skalor. Används bara när `facet_scales` inte anges uttryckligen.
#' @param facet_title_size Panelrubrikernas teckenstorlek i punkter.
#' @param facet_title_wrap Radbryt panelrubrikerna efter så här många tecken; långa ord
#'   avstavas. `NULL` (standard) ger ingen radbrytning.
#' @param bar_type Stapeltyp: `"grouped"` (sida vid sida), `"stacked"` (staplade) eller
#'   `"filled"` (100 % staplade).
#' @param horizontal `TRUE` ger liggande staplar.
#' @param title Huvudrubrik. `NULL` (standard) ger ingen rubrik.
#' @param title_width Antal tecken innan rubriken radbryts.
#' @param subtitle Underrubrik. `NULL` (standard) ger ingen underrubrik.
#' @param subtitle_width Antal tecken innan underrubriken radbryts.
#' @param caption Text längst ner, t.ex. källa och noter. `NULL` (standard) ger ingen
#'   text.
#' @param x_axis_title Rubrik för x-axeln. `NULL` (standard) ger ingen rubrik.
#' @param y_axis_title Rubrik för y-axeln. `NULL` (standard) ger ingen rubrik.
#' @param x_date_format Datumformat (strftime) för en datumaxel, t.ex. `"%Y-%m"` eller
#'   `"%b %Y"`. `NULL` (standard) väljs automatiskt.
#' @param x_date_breaks Avstånd mellan datummarkeringar, t.ex. `"3 months"` eller `"1
#'   year"`. `NULL` (standard) väljs automatiskt.
#' @param x_date_minor_breaks Avstånd mellan mindre datummarkeringar, t.ex. `"1 month"`
#'   när de större är `"3 months"`. `NULL` (standard) ger inga.
#' @param x_number_format Format för en numerisk x-axel: `"whole"` (utan avgränsare,
#'   t.ex. 2000) eller `"thousands"` (med mellanslag, t.ex. 2 000).
#' @param legend_title Förklaringens rubrik. `NULL` (standard) ger namnet på
#'   `group`-kolumnen.
#' @param colors Färger för staplar/grupper. En namnlös vektor fördelas i gruppordning
#'   (faktornivåer utan oanvända nivåer, tal sorterade, annars ordningen i data efter
#'   `sort_by`) och återanvänds om den är för kort. En namngiven vektor matchas mot
#'   gruppnamnen, t.ex. `c("Kvinnor" = "#1a9fb3", "Män" = "#ffc107")`. Standard är
#'   VGR:s färgtema "ru" (samma färger som `func_fargtema("ru")` i funcVGR); `NULL` ger
#'   färger enligt `color_palette`.
#' @param color_palette Färgskala när `colors = NULL`: `"viridis"`, `"plasma"`,
#'   `"blues"` eller `"reds"`.
#' @param font Typsnitt, t.ex. `"Verdana"` eller `"Arial"`. Måste vara installerat på
#'   datorn, annars används `"sans"` (med ett meddelande).
#' @param text_scale Faktor som alla teckenstorlekar multipliceras med (rubriker, axlar,
#'   förklaring, panelrubriker och etiketter). 1 = storlekarna som de är angivna, t.ex.
#'   1.3 för stora exporter.
#' @param title_size Rubrikens teckenstorlek i punkter.
#' @param title_text_size Alias för `title_size` (bakåtkompatibilitet). Vinner om den
#'   anges.
#' @param subtitle_size Underrubrikens teckenstorlek i punkter.
#' @param subtitle_text_size Alias för `subtitle_size` (bakåtkompatibilitet). Vinner om
#'   den anges.
#' @param caption_size Teckenstorlek för `caption` i punkter.
#' @param axis_text_size Standardstorlek i punkter för båda axlarnas skaltext. Används
#'   när `x_axis_text_size`/`y_axis_text_size` inte anges.
#' @param x_axis_text_size Teckenstorlek för x-axelns skaltext i punkter.
#' @param y_axis_text_size Teckenstorlek för y-axelns skaltext i punkter.
#' @param legend_text_size Förklaringens teckenstorlek i punkter.
#' @param legend_title_size Teckenstorlek för förklaringens rubrik i punkter.
#' @param x_axis_title_size Teckenstorlek för x-axelns rubrik i punkter.
#' @param y_axis_title_size Teckenstorlek för y-axelns rubrik i punkter.
#' @param legend_position Förklaringens placering: `"right"`, `"left"`, `"top"`,
#'   `"bottom"` eller `"none"`.
#' @param show_major_grid Större stödlinjer: `TRUE` = i båda riktningarna, `FALSE` =
#'   inga, `"y"` = bara linjer från y-axeln (vågräta), `"x"` = bara linjer från x-axeln
#'   (lodräta). Gäller de axlar som visas, så med `horizontal = TRUE` ger `"x"` linjer
#'   vid värdena.
#' @param show_minor_grid Mindre stödlinjer, med samma val som `show_major_grid`.
#' @param x_axis_line Rita en linje längs x-axeln. Linjen ritas vid panelens kant, inte
#'   vid y = 0.
#' @param y_axis_line Rita en linje längs y-axeln.
#' @param x_angle Rotation av x-axelns text i grader (0 = vågrät, 45 = diagonal, 90 =
#'   lodrät). Sätts automatiskt till 45 vid många kategorier. Gäller inte liggande
#'   staplar.
#' @param x_axis_title_angle Rotation av x-axelns rubrik i grader (0 = vågrät, 90 =
#'   lodrät).
#' @param data_labels Visa värdeetiketter.
#' @param data_label_position Etikettplacering: `"outside"` (utanför stapeln), `"top"`
#'   (strax innanför toppen), `"middle"` (mitten) eller `"inside"` (10 % från toppen).
#' @param data_label_size Etiketternas teckenstorlek i mm (ggplot2:s textenhet, 3,5 mm ≈
#'   10 punkter).
#' @param data_label_text_size Alias för `data_label_size` (bakåtkompatibilitet). Vinner
#'   om den anges.
#' @param data_label_color Etiketternas textfärg, eller `"auto"` för gruppens färg
#'   (utan `group`: stapelns färg; totalsummor: svart).
#' @param data_label_box Visa en ruta bakom etiketterna.
#' @param data_label_box_color Rutans bakgrundsfärg (när `data_label_box = TRUE`).
#' @param data_label_format Etikettformat: `"number"` (1 234), `"percentage"` (12,3 %)
#'   eller `"x_y"` (x- och y-värde, t.ex. "2000: 2 122").
#' @param data_label_decimals Antal decimaler i etiketterna.
#' @param data_label_accuracy Alias för `data_label_decimals` (bakåtkompatibilitet).
#'   Vinner om den anges.
#' @param data_label_width Radbryt etikettexten efter så här många tecken. `NULL`
#'   (standard) ger ingen radbrytning.
#' @param data_label_nudge_x Vågrät förskjutning av etiketterna, i kategorienheter
#'   (positiv = åt höger).
#' @param data_label_nudge_y Lodrät förskjutning av etiketterna, i värdeaxelns enheter
#'   (positiv = uppåt).
#' @param data_label_repel Flytta isär etiketterna så att de inte överlappar.
#'   D3-versionen har en egen algoritm (inte ggrepel). Används inte för staplade
#'   staplar.
#' @param data_label_force Extra avstånd mellan etiketter som flyttats isär; högre ger
#'   mer avstånd (typiskt 0,5–5).
#' @param data_label_max_overlaps Etiketter som fortfarande överlappar fler än så här
#'   många andra döljs.
#' @param data_label_filter Visa etiketter bara för de här kategorierna på x-axeln.
#'   Alias för `data_label_x_values`. `NULL` (standard) visar alla.
#' @param data_label_x_values Visa etiketter bara för de här x-värdena. `NULL`
#'   (standard) visar alla.
#' @param data_label_y_values Visa etiketter bara där y har de här värdena. `NULL`
#'   (standard) visar alla.
#' @param data_label_group_values Visa etiketter bara för de här grupperna (kräver
#'   `group`). `NULL` (standard) visar alla.
#' @param data_label_first_last Visa etiketter bara på första och sista stapeln per
#'   grupp och panel.
#' @param data_label_skip Visa var N:e etikett per grupp och panel (2 = varannan).
#'   `NULL` (standard) visar alla.
#' @param data_label_show_group Skriv gruppnamnet före värdet i etiketten, t.ex. "2023:
#'   5,2".
#' @param data_label_group_separator Avskiljare mellan gruppnamn och värde när
#'   `data_label_show_group = TRUE`.
#' @param sort_by Sortering av staplarna: `"value_asc"`, `"value_desc"`, `"name_asc"`
#'   eller `"name_desc"`. `NULL` (standard) behåller ursprunglig ordning.
#' @param y_axis_min Minvärde för värdeaxeln. Ligger det inom data klipps staplarna mot
#'   panelen. `NULL` (standard) väljs automatiskt.
#' @param y_min Alias för `y_axis_min` (bakåtkompatibilitet). Vinner om den anges.
#' @param y_axis_max Maxvärde för y-axeln. `NULL` (standard) väljs automatiskt.
#' @param y_max Alias för `y_axis_max` (bakåtkompatibilitet). Vinner om den anges.
#' @param y_axis_breaks Egna markeringar på y-axeln, t.ex. `c(0, 50, 100)`. `NULL`
#'   (standard) väljs automatiskt.
#' @param y_breaks Alias för `y_axis_breaks` (bakåtkompatibilitet). Vinner om den anges.
#' @param y_axis_n_breaks Önskat antal markeringar på y-axeln (typiskt 4–8). `NULL`
#'   (standard) väljs automatiskt.
#' @param y_n_breaks Alias för `y_axis_n_breaks` (bakåtkompatibilitet). Vinner om den
#'   anges.
#' @param y_axis_auto_breaks `TRUE` ger "snygga" markeringar som inte trängs, `FALSE`
#'   använder `y_axis_breaks`/`y_axis_n_breaks` exakt.
#' @param y_auto_breaks Alias för `y_axis_auto_breaks` (bakåtkompatibilitet). Vinner om
#'   den anges.
#' @param bar_width Stapelbredd (0,1–1 för kategorier). `NULL` (standard) väljs
#'   automatiskt (0,8 för kategorier, anpassat till avståndet för datum).
#' @param bar_line_color Färg på kantlinjen runt staplarna. Kantlinjen ger bättre
#'   kontrast för personer med nedsatt syn och skiljer delarna åt i staplade diagram.
#'   `NULL` ger ingen kantlinje.
#' @param bar_line_width Kantlinjens tjocklek (ggplot2:s `linewidth`, typiskt 0,2–0,5).
#' @param custom_theme Används inte i D3-versionen (ett ggplot2-tema går inte att
#'   använda här, och ett meddelande visas). Finns kvar så att befintliga anrop
#'   fungerar.
#' @param debug `TRUE` returnerar en lista med diagnostik (data, klassindelning och den
#'   beräknade specifikationen) i stället för diagrammet.
#' @param color_negative Fyllnadsfärg för staplar under noll (bara när alla staplar har
#'   samma färg), t.ex. `"#b2182b"`. `NULL` (standard) ger samma färg som övriga
#'   staplar.
#' @param y_break_line Värde där en linje dras tvärs över diagrammet, t.ex. ett
#'   medelvärde. `NULL` (standard) ger ingen.
#' @param y_break_line_label Text vid linjen från `y_break_line`, t.ex. `"Snitt VG"`.
#'   `NULL` (standard) ger ingen text.
#' @param reference_column Kolumn med ett referensvärde per stapel (t.ex. riket), som
#'   text. Ritas som en markering på varje stapel. `NULL` (standard) ger ingen.
#' @param reference_label Referensvärdets namn i förklaringen och tooltipen, t.ex.
#'   `"Riket"`.
#' @param reference_color Färg på referensmarkeringen.
#' @param highlight_x_values Kategorier som markeras, t.ex. `c("Borås")`. Övriga staplar
#'   tonas ned och axeltexten blir fet. `NULL` (standard) markerar inga.
#' @param highlight_color Fyllnadsfärg för markerade staplar (bara när alla staplar har
#'   samma färg). `NULL` (standard) behåller färgen.
#' @param highlight_other_alpha Opacitet för staplar som inte är markerade (0–1).
#' @param tooltip_show Visa en tooltip med alla grupper i kategorin (och andel av
#'   totalen i staplade staplar).
#' @param tooltip_decimals Antal decimaler i tooltipens värden. `NULL` (standard) ger 0
#'   om alla värden är heltal, annars `data_label_decimals`.
#' @param tooltip_unit Enhet som läggs efter tooltipens värden, t.ex. `" %"` eller `"
#'   kr"`.
#' @param tooltip_columns Extra kolumner som visas i tooltipen för stapeln under
#'   muspekaren, som text. En namngiven vektor ger visningsnamnet, t.ex.
#'   `c("Antal arbetslösa" = "antal")`; utan namn visas kolumnnamnet. Talkolumner får
#'   egna decimaler: heltal utan decimaler, annars `tooltip_decimals` eller
#'   `data_label_decimals`. Har flera rader samma x och grupp visas det första värdet som
#'   inte saknas. `NULL` (standard) ger inga.
#' @param interactive_bar_type_toggle Visa knappar för att växla mellan grupperade,
#'   staplade och 100 % staplade staplar (bara med `group`).
#' @param interactive_sort Visa en sorteringsmeny för kategorierna (bara när x är text
#'   eller faktor).
#' @param interactive_legend Klicka på en grupp i förklaringen för att dölja eller visa
#'   den, håll muspekaren över den för att framhäva den.
#' @param interactive_filter_column Kolumn som visas som en rullista med
#'   uppspelningsknapp, t.ex. `"ar"`; diagrammet visar ett värde i taget. `NULL`
#'   (standard) ger ingen rullista.
#' @param interactive_filter_default Värde som visas först i rullistan. `NULL`
#'   (standard) ger det sista värdet.
#' @param interactive_filter_same_y `TRUE` behåller samma värdeaxel för alla värden i
#'   rullistan, `FALSE` skalar om för varje.
#' @param interactive_export Visa knappar för att ladda ned diagrammet som PNG eller
#'   SVG.
#' @param animation_grow Låt staplarna växa från noll när diagrammet öppnas.
#' @param animation_duration Övergångstid i millisekunder. 0 = ingen animation.
#' @param animation_play_interval Tid i millisekunder per värde i rullistan när
#'   uppspelningsknappen används.
#' @param width Diagrammets bredd i pixlar. Diagrammen (utom kartorna)
#'   krymper för att få plats i fönstret.
#' @param height Diagrammets höjd i pixlar.
#' @param data_label_column Kolumn med etikettexten, som text. Texten visas som den är,
#'   och `\n` ger en ny rad, t.ex. `"2 193 mdr kr\n(90,0 %)"`. Har flera rader samma x och
#'   grupp används den första texten som inte saknas; saknad text ger ingen etikett.
#'   Kräver `data_labels = TRUE`. Används inte för summaetiketten på staplade staplar med
#'   `data_label_position = "outside"`. `NULL` (standard) ger etiketter enligt
#'   `data_label_format`.
#' @param data_label_overflow Vad som händer med en etikett utanför stapeln
#'   (`data_label_position = "outside"`) som inte ryms i panelen: `"expand"` (standard)
#'   ger stående staplars värdeaxel mer luft, så att etiketter med flera rader också ryms
#'   (dock inte förbi en angiven `y_axis_max`/`y_axis_min`), och `"inside"` lägger
#'   etiketten inuti stapelns ände i färgen `data_label_inside_color`, om stapeln är
#'   längre än etiketten. Gäller inte `data_label_repel = TRUE`.
#' @param data_label_inside_color Textfärg för etiketter som `data_label_overflow =
#'   "inside"` har lagt inuti stapeln. Med `data_label_box = TRUE` behålls
#'   `data_label_color`.
#' @param x_axis_text_wrap Radbryt kategoriaxelns text efter så här många tecken. `\n` i
#'   ett kategorinamn ger alltid en ny rad. Utrymmet under (eller till vänster om)
#'   panelen räknas efter antalet rader. `NULL` (standard) ger ingen radbrytning.
#' @param x_axis_breaks Värdena för skalstrecken på en tal- eller datumaxel, t.ex.
#'   `seq(2000, 2024, 2)` eller en vektor med datum. Går före `x_axis_n_breaks` och
#'   `x_date_breaks`. `NULL` (standard) väljs automatiskt.
#' @param x_axis_n_breaks Önskat antal skalstreck på en numerisk x-axel (typiskt 4–10).
#'   `NULL` (standard) ger 5.
#' @param x_axis_minor_breaks Värdena för de mindre stödlinjerna på en tal- eller
#'   datumaxel, t.ex. `2000:2024` (en per år). Går före `x_date_minor_breaks` och syns
#'   med `show_minor_grid`. `NULL` (standard) ger inga på en numerisk axel.
#' @param y_axis_expand Luft på värdeaxeln utanför data och `y_axis_min`/`y_axis_max`,
#'   som andel av spannet: `c(nedre, övre)`, som `ggplot2::expansion(mult = ...)`. Med
#'   `y_axis_max = 100` och `y_axis_expand = c(0, 0)` slutar axeln vid 100. Anges den
#'   får liggande staplars etiketter ingen extra luft. `NULL` (standard) ger 5 % nedtill
#'   och 15 % upptill (20 % med etiketter, 10 och 25 % i paneler med etiketter, mer för
#'   långa etiketter på liggande staplar).
#' @param y_axis_format Värdeaxelns format: `"number"` (standard) eller `"percent"` för
#'   andelar mellan 0 och 1, som visas som procent ("82 %") på axeln, i tooltipen och i
#'   etiketterna. `y_axis_min`, `y_axis_max`, `y_axis_breaks` och `y_break_line` anges
#'   då som andelar.
#' @param grid_major_color Färg på de större stödlinjerna.
#' @param grid_minor_color Färg på de mindre stödlinjerna.
#' @param grid_major_width Tjocklek på de större stödlinjerna (ggplot2:s `linewidth`).
#' @param grid_minor_width Tjocklek på de mindre stödlinjerna (ggplot2:s `linewidth`).
#' @param tooltip_x_format Hur x-värdet skrivs som rubrik i tooltipen: ett datumformat
#'   (strftime) för en datumaxel, t.ex. `"%Y"`, eller en funktion som tar x-värdena och
#'   returnerar text, t.ex. `function(x) paste(x, "år")`. `NULL` (standard) ger
#'   `as.character()` av x-värdet.
#' @param reverse_categories `TRUE` vänder kategoriernas ordning på axeln. Liggande
#'   staplar ritas annars nerifrån, så att den första kategorin (efter faktornivåer eller
#'   sortering) hamnar längst ned; med `TRUE` hamnar den överst, som i en tabell. På
#'   stående staplar hamnar den första kategorin längst till höger. Gäller även
#'   sorteringsmenyn, och bara kategorier (text eller faktor), inte tal- och datumaxlar.
#' @param legend_reverse `TRUE` visar förklaringen i omvänd ordning. Staplade staplar
#'   staplas i omvänd gruppordning (den första gruppen längst ut), så med `TRUE` står
#'   grupperna i förklaringen i samma ordning som de läses i en liggande stapel.
#' @param .rendera Intern. `TRUE` bygger diagrammet och används när objektet skrivs ut.
#'   Ändra inte.
#'
#' @return Ett objekt av klassen `vgr_d3`. Det visas i RStudio:s Viewer när det skrivs ut,
#'   i HTML-dokument från R Markdown och Quarto och på sidan från "Run examples" i
#'   hjälpen. Spara det med [func_d3_spara()].
#'   Med `debug = TRUE` returneras i stället en lista med diagnostik.
#' @seealso [func_d3_linjediagram()], [d3_plot()] och [func_d3_spara()].
#' @export
#' @example inst/examples/func_d3_stapeldiagram.R
func_d3_stapeldiagram <- function(
    # === REQUIRED DATA ===
  df,
  x_value,
  y_value,

  # === GROUPING ===
  group = NULL,

  # === FACETING (SMALL MULTIPLES) ===
  facet_var = NULL,
  facet_ncol = NULL,
  facet_scales = "free_y",
  facet_same_y_scale = TRUE,
  facet_title_size = 12,
  facet_title_wrap = NULL,

  # === BAR ARRANGEMENT ===
  bar_type = "grouped",
  horizontal = FALSE,

  # === TEXT - TITLES ===
  title = NULL,
  title_width = 100,
  subtitle = NULL,
  subtitle_width = 60,
  caption = NULL,

  # === TEXT - AXES ===
  x_axis_title = NULL,
  y_axis_title = NULL,

  # === DATE X-AXIS ===
  x_date_format = NULL,
  x_date_breaks = NULL,
  x_date_minor_breaks = NULL,
  x_number_format = "whole",

  # === LEGEND ===
  legend_title = NULL,

  # === COLORS ===
  colors = func_d3_farger_ru(),
  color_palette = "viridis",

  # === FONTS AND SIZES ===
  font = "Verdana",
  text_scale = 1,
  title_size = 16,
  title_text_size = 16,
  subtitle_size = 13,
  subtitle_text_size = 13,
  caption_size = 10,
  axis_text_size = 11,
  x_axis_text_size = 11,
  y_axis_text_size = 11,
  legend_text_size = 11,
  legend_title_size = 11,
  x_axis_title_size = 11,
  y_axis_title_size = 11,

  # === LAYOUT ===
  legend_position = "right",
  show_major_grid = FALSE,
  show_minor_grid = FALSE,
  x_axis_line = FALSE,
  y_axis_line = FALSE,
  x_angle = 0,
  x_axis_title_angle = 0,

  # === DATA LABELS - DISPLAY ===
  data_labels = FALSE,
  data_label_position = "outside",
  data_label_size = 3.5,
  data_label_text_size = 3.5,
  data_label_color = "black",
  data_label_box = FALSE,
  data_label_box_color = "white",

  # === DATA LABELS - FORMATTING ===
  data_label_format = "number",
  data_label_decimals = 1,
  data_label_accuracy = 1,
  data_label_width = NULL,

  # === DATA LABELS - POSITIONING ===
  data_label_nudge_x = 0,
  data_label_nudge_y = 0,
  data_label_repel = FALSE,
  data_label_force = 1,
  data_label_max_overlaps = 10,

  # === DATA LABELS - FILTERING ===
  data_label_filter = NULL,
  data_label_x_values = NULL,
  data_label_y_values = NULL,
  data_label_group_values = NULL,
  data_label_first_last = FALSE,
  data_label_skip = NULL,

  # === DATA LABELS - GROUP NAMES ===
  data_label_show_group = FALSE,
  data_label_group_separator = ": ",

  # === SORTING ===
  sort_by = NULL,

  # === AXIS LIMITS AND BREAKS ===
  y_axis_min = NULL,
  y_min = NULL,
  y_axis_max = NULL,
  y_max = NULL,
  y_axis_breaks = NULL,
  y_breaks = NULL,
  y_axis_n_breaks = NULL,
  y_n_breaks = NULL,
  y_axis_auto_breaks = TRUE,
  y_auto_breaks = TRUE,

  # === BAR STYLING ===
  bar_width = NULL,
  bar_line_color = "black",
  bar_line_width = 0.3,

  # === ADVANCED ===
  custom_theme = NULL,
  debug = FALSE,

  # ------------------------------------------------------------------
  # D3-tillägg: nya parametrar ligger efter originalets, så positionella
  # anrop träffar samma parametrar som i func_graf-originalet
  # ------------------------------------------------------------------

  # === D3: COLORS ===
  color_negative = NULL,

  # === D3: REFERENCE VALUES ===
  y_break_line = NULL,
  y_break_line_label = NULL,
  reference_column = NULL,
  reference_label = "Referens",
  reference_color = "#263238",

  # === D3: HIGHLIGHTING ===
  highlight_x_values = NULL,
  highlight_color = NULL,
  highlight_other_alpha = 0.35,

  # === D3: TOOLTIP ===
  tooltip_show = TRUE,
  tooltip_decimals = NULL,
  tooltip_unit = "",
  tooltip_columns = NULL,

  # === D3: INTERACTIVITY ===
  interactive_bar_type_toggle = TRUE,
  interactive_sort = TRUE,
  interactive_legend = TRUE,
  interactive_filter_column = NULL,
  interactive_filter_default = NULL,
  interactive_filter_same_y = TRUE,
  interactive_export = TRUE,

  # === D3: ANIMATION ===
  animation_grow = TRUE,
  animation_duration = 700,
  animation_play_interval = 1500,

  # === D3: SIZE ===
  width = 1400,
  height = 800,

  # --- Tillagda i 0.15.0 (sist, så att positionella anrop inte flyttas) ---
  # === D3: DATA LABELS ===
  data_label_column = NULL,
  data_label_overflow = "expand",
  data_label_inside_color = "white",

  # === D3: AXES ===
  x_axis_text_wrap = NULL,
  x_axis_breaks = NULL,
  x_axis_n_breaks = NULL,
  x_axis_minor_breaks = NULL,
  y_axis_expand = NULL,
  y_axis_format = "number",
  reverse_categories = FALSE,

  # === D3: GRID LINES ===
  grid_major_color = "#e5e5e5",
  grid_minor_color = "#f2f2f2",
  grid_major_width = 0.5,
  grid_minor_width = 0.25,

  # === D3: TOOLTIP AND LEGEND ===
  tooltip_x_format = NULL,
  legend_reverse = FALSE,
  .rendera = FALSE
) {

  if (!.rendera) {
    angivna <- setdiff(names(as.list(match.call()))[-1], ".rendera")
    obj <- func_d3_nytt_objekt("stapeldiagram", mget(angivna, envir = environment()))
    if (debug) return(attr(func_d3_spec(obj), "debug"))
    return(obj)
  }

  # Resolve alternative parameter names (last-one-wins if both are supplied) — same rules as func_graf_barchart
  title_size          <- if (!missing(title_text_size))      title_text_size      else title_size
  subtitle_size       <- if (!missing(subtitle_text_size))   subtitle_text_size   else subtitle_size
  data_label_size     <- if (!missing(data_label_text_size)) data_label_text_size else data_label_size
  x_axis_text_size    <- if (!missing(x_axis_text_size))     x_axis_text_size     else axis_text_size
  y_axis_text_size    <- if (!missing(y_axis_text_size))     y_axis_text_size     else axis_text_size
  y_axis_min          <- if (!missing(y_min))                y_min                else y_axis_min
  y_axis_max          <- if (!missing(y_max))                y_max                else y_axis_max
  y_axis_breaks       <- if (!missing(y_breaks))             y_breaks             else y_axis_breaks
  y_axis_n_breaks     <- if (!missing(y_n_breaks))           y_n_breaks           else y_axis_n_breaks
  y_axis_auto_breaks  <- if (!missing(y_auto_breaks))        y_auto_breaks        else y_axis_auto_breaks
  data_label_decimals <- if (!missing(data_label_accuracy))  data_label_accuracy  else data_label_decimals
  # text_scale: en faktor för alla textstorlekar (efter aliasen, som i func_graf_barchart)
  title_size        <- title_size        * text_scale
  subtitle_size     <- subtitle_size     * text_scale
  caption_size      <- caption_size      * text_scale
  x_axis_text_size  <- x_axis_text_size  * text_scale
  y_axis_text_size  <- y_axis_text_size  * text_scale
  x_axis_title_size <- x_axis_title_size * text_scale
  y_axis_title_size <- y_axis_title_size * text_scale
  legend_text_size  <- legend_text_size  * text_scale
  legend_title_size <- legend_title_size * text_scale
  facet_title_size  <- facet_title_size  * text_scale
  data_label_size   <- data_label_size   * text_scale
  # facet_scales wins when explicitly set — facet_same_y_scale only applies otherwise
  facet_scales        <- if (!missing(facet_scales)) facet_scales else if (facet_same_y_scale) "fixed" else facet_scales

  p <- mget(setdiff(names(formals()), c(".rendera", "title_text_size", "subtitle_text_size", "data_label_text_size",
                                        "axis_text_size", "y_min", "y_max", "y_breaks", "y_n_breaks", "y_auto_breaks",
                                        "data_label_accuracy", "facet_same_y_scale")),
            envir = environment())
  func_d3_stapeldiagram_bygg(p)
}

# Samma funktion under func_graf_barchart:s namn
#' @rdname func_d3_stapeldiagram
#' @export
func_d3_barchart <- func_d3_stapeldiagram

func_d3_stapeldiagram_bygg <- function(p) {

  df_work <- if (inherits(p$df, "sf")) sf::st_drop_geometry(p$df) else as.data.frame(p$df)
  x_value <- p$x_value; y_value <- p$y_value
  group <- p$group; facet_var <- p$facet_var; filt <- p$interactive_filter_column

  if (!is.character(x_value) && length(x_value) > 1) { df_work$.tmp_x <- x_value; x_value <- ".tmp_x" }
  if (!is.character(y_value) && length(y_value) > 1) { df_work$.tmp_y <- y_value; y_value <- ".tmp_y" }

  missing_cols <- setdiff(c(x_value, y_value, group, facet_var, filt, p$reference_column, p$data_label_column), names(df_work))
  if (length(missing_cols) > 0) stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  if (!is.numeric(df_work[[y_value]])) stop("func_d3_stapeldiagram: y_value ('", y_value, "') måste vara numerisk.")
  func_d3_kontrollera_yaxel(p)
  if (!identical(p$data_label_overflow, "expand") && !identical(p$data_label_overflow, "inside"))
    stop("data_label_overflow must be \"expand\" or \"inside\"")
  if (!p$bar_type %in% c("grouped", "stacked", "filled")) stop("bar_type must be one of: grouped, stacked, filled")
  if (!p$legend_position %in% c("none", "top", "bottom", "left", "right"))
    stop("legend_position must be one of: right, left, top, bottom, none")
  grid_ok <- function(v) isTRUE(v) || isFALSE(v) || identical(v, "x") || identical(v, "y")
  if (!grid_ok(p$show_major_grid)) stop("show_major_grid must be TRUE, FALSE, \"x\" or \"y\"")
  if (!grid_ok(p$show_minor_grid)) stop("show_minor_grid must be TRUE, FALSE, \"x\" or \"y\"")
  if (!is.null(p$custom_theme))
    message("func_d3_stapeldiagram: custom_theme är ett ggplot2-tema och används inte i D3-versionen.")
  if (nrow(df_work) == 0) warning("func_d3_stapeldiagram: data frame has 0 rows.")

  if (!is.null(group) && is.numeric(df_work[[group]])) df_work[[group]] <- factor(df_work[[group]])
  # Färg- och staplingsordning (samma regel som func_graf_linjediagram/barchart/karta): faktornivåer
  # utan oanvända, annars ordning i data; NA får ingen färgplats. Räknas före NA-omvandlingen nedan,
  # som annars gör om en textkolumn till en faktor med alfabetiska nivåer. func_graf_barchart räknar
  # ordningen efter sort_by — här sorteras staplarna i JS, så radordningen efterliknas bara för ordningen
  farg_ordning <- NULL
  if (!is.null(group)) {
    g <- df_work[[group]]
    if (!is.factor(g) && !is.null(p$sort_by) && !inherits(df_work[[x_value]], c("Date", "POSIXct"))) {
      rad <- switch(p$sort_by,
                    "value_asc"  = order(df_work[[y_value]]),
                    "value_desc" = order(-df_work[[y_value]]),
                    "name_asc"   = order(df_work[[x_value]]),
                    "name_desc"  = order(df_work[[x_value]], decreasing = TRUE),
                    seq_along(g))
      g <- g[rad]
    }
    farg_ordning <- if (is.factor(g)) setdiff(levels(droplevels(g)), NA) else unique(stats::na.omit(as.character(g)))
  }
  # Saknad grupp blir en egen grupp "NA" (grå, sist), som scale_fill_manual:s na.value i ggplot2
  # as.character: anyNA() ser inte en uttrycklig NA-nivå i en faktor (addNA), bara NA-koder
  na_grupp <- !is.null(group) && anyNA(as.character(df_work[[group]]))
  if (na_grupp) df_work[[group]] <- func_d3_na_som_niva(df_work[[group]])
  # Saknad facett blir en egen panel "NA" sist, som facet_wrap
  if (!is.null(facet_var) && anyNA(as.character(df_work[[facet_var]]))) {
    df_work[[facet_var]] <- if (is.factor(df_work[[facet_var]])) func_d3_na_som_niva(df_work[[facet_var]])
                            else { f <- as.character(df_work[[facet_var]]); f[is.na(f)] <- "NA"
                                   factor(f, levels = c(setdiff(unique(f), "NA"), "NA")) }
  }

  # --- Facet text wrapping (same rules as func_graf_barchart) ---
  if (!is.null(facet_var) && !is.null(p$facet_title_wrap) && p$facet_title_wrap > 0) {
    bredd <- p$facet_title_wrap
    df_work[[facet_var]] <- vapply(as.character(df_work[[facet_var]]), function(x) {
      wrapped <- str_wrap(x, width = bredd)
      if (any(nchar(strsplit(wrapped, "\n")[[1]]) > bredd)) {
        chars <- strsplit(x, "")[[1]]; chunks <- character(0); start <- 1
        while (start <= length(chars)) {
          end <- min(start + bredd - 2, length(chars))
          chunk <- paste0(chars[start:end], collapse = "")
          if (end < length(chars)) chunk <- paste0(chunk, "-")
          chunks <- c(chunks, chunk); start <- end + 1
        }
        paste(chunks, collapse = "\n")
      } else wrapped
    }, character(1), USE.NAMES = FALSE)
  }
  if (!is.null(facet_var) && !is.factor(df_work[[facet_var]]))
    df_work[[facet_var]] <- factor(df_work[[facet_var]], levels = unique(df_work[[facet_var]]))

  # --- Data types ---
  x_is_date <- inherits(df_work[[x_value]], "Date") || inherits(df_work[[x_value]], "POSIXct")
  x_is_continuous <- is.numeric(df_work[[x_value]])
  x_typ <- if (x_is_date) "date" else if (x_is_continuous) "number" else "discrete"

  # --- Category order: ggplot uses factor levels, otherwise alphabetical; sort_by is applied in JS ---
  x_levels <- NULL
  if (x_typ == "discrete") {
    x_levels <- if (is.factor(df_work[[x_value]])) levels(droplevels(df_work[[x_value]]))
                else sort(unique(as.character(df_work[[x_value]])))
  }
  enc_x <- function(v) {
    if (x_typ == "date") { if (inherits(v, "POSIXct")) as.numeric(v) * 1000 else as.numeric(as.Date(v)) * 86400000 }
    else if (x_typ == "discrete") as.character(v)
    else as.numeric(v)
  }

  # --- Colors (same mapping as func_graf_barchart) ---
  grupp_nyckel <- if (!is.null(group)) as.character(df_work[[group]]) else rep("__alla", nrow(df_work))
  colors <- p$colors
  if (!is.null(group)) {
    farg_grupper <- farg_ordning
    n_groups <- max(1, length(farg_grupper))
    if (is.null(colors)) {
      colors <- switch(p$color_palette,
                       "viridis" = viridisLite::viridis(n_groups, option = "D"),
                       "plasma"  = viridisLite::viridis(n_groups, option = "C"),   # "C" = plasma ("E" gave cividis before funcVGRD3 0.7.0)
                       "blues"   = scales::brewer_pal(type = "seq", palette = "Blues")(n_groups),
                       "reds"    = scales::brewer_pal(type = "seq", palette = "Reds")(n_groups),
                       viridisLite::viridis(n_groups, option = "D"))
    }
    if (!is.null(names(colors))) {
      # Namngivna färger: matchas mot gruppnamnet
      saknas_farg <- setdiff(farg_grupper, names(colors))
      if (length(saknas_farg) > 0) stop("colors is named but has no color for: ", toString(saknas_farg))
      color_mapping <- colors[farg_grupper]
    } else {
      # Onamngivna färger: i gruppordning, återanvänds om de är för få
      color_mapping <- stats::setNames(rep(colors, length.out = length(farg_grupper)), farg_grupper)
    }
    if (na_grupp) color_mapping[["NA"]] <- "grey50"
    # Staplade diagram: gruppordning = färgordningen (som i originalet, NA sist); annars faktornivåer/alfabetiskt
    group_order <- if (p$bar_type %in% c("stacked", "filled")) c(farg_grupper, if (na_grupp) "NA") else levels(factor(df_work[[group]]))
  } else {
    color_mapping <- c("__alla" = if (!is.null(colors)) colors[1] else "#2E86AB")
    group_order <- "__alla"
  }
  grupper <- lapply(group_order, function(g) list(key = g, label = if (g == "__alla") y_value else g,
                                                  color = func_d3_farg(color_mapping[[g]])))

  # --- Bar width (same smart default as func_graf_barchart) ---
  bar_width <- p$bar_width
  if (is.null(bar_width)) {
    if (x_is_date) {
      date_vals <- sort(unique(df_work[[x_value]]))
      # POSIXct mäts i sekunder, Date i dagar — diff() väljer annars enhet själv (t.ex. dagar för POSIXct)
      enhet <- if (inherits(df_work[[x_value]], "POSIXct")) "secs" else "days"
      bar_width <- if (length(date_vals) > 1) as.numeric(min(diff(date_vals)), units = enhet) * 0.8
                   else if (enhet == "secs") 20 * 86400 else 20
    } else bar_width <- 0.8
  }
  bar_width_enc <- if (x_is_date) (if (inherits(df_work[[x_value]], "POSIXct")) bar_width * 1000 else bar_width * 86400000) else bar_width

  # --- X-axis (dates and numbers) ---
  x_angle <- p$x_angle
  x_ticks <- list(); x_tick_labels <- list(); x_domain <- NULL; x_minor <- NULL
  if (nrow(df_work) > 0 && x_typ != "discrete") {
    xr <- range(df_work[[x_value]], na.rm = TRUE)
    lo <- enc_x(xr[1]) - bar_width_enc / 2; hi <- enc_x(xr[2]) + bar_width_enc / 2
    mult <- if (x_is_date) 0.02 else 0.05
    x_domain <- c(lo - (hi - lo) * mult, hi + (hi - lo) * mult)
    if (x_is_date) {
      date_range_days <- as.numeric(diff(range(as.Date(df_work[[x_value]]), na.rm = TRUE)))
      x_date_breaks <- p$x_date_breaks %||% (if (date_range_days <= 90) "1 month" else if (date_range_days <= 365) "2 months"
                                              else if (date_range_days <= 730) "3 months" else if (date_range_days <= 1825) "6 months" else "1 year")
      x_date_format <- p$x_date_format %||% (if (grepl("year", x_date_breaks)) "%Y" else if (grepl("(6|3) month", x_date_breaks)) "%b %Y"
                                              else if (date_range_days <= 90) "%Y-%m-%d" else "%b %Y")
      # Samma brytpunkter som scale_x_date(date_breaks = ...)
      brk <- scales::breaks_width(x_date_breaks)(as.Date(xr))
      brk <- brk[enc_x(brk) >= x_domain[1] & enc_x(brk) <= x_domain[2]]
      x_ticks <- as.list(enc_x(brk)); x_tick_labels <- as.list(format(brk, x_date_format))
      if (!is.null(p$x_date_minor_breaks)) {
        mbrk <- scales::breaks_width(p$x_date_minor_breaks)(as.Date(xr))
        x_minor <- as.list(enc_x(mbrk[enc_x(mbrk) >= x_domain[1] & enc_x(mbrk) <= x_domain[2]]))
      }
      # Egna skalstreck och mindre stödlinjer (x_axis_breaks/x_axis_minor_breaks) går före x_date_*
      if (!is.null(p$x_axis_breaks)) {
        brk <- sort(as.Date(p$x_axis_breaks))
        brk <- brk[enc_x(brk) >= x_domain[1] & enc_x(brk) <= x_domain[2]]
        x_ticks <- as.list(enc_x(brk)); x_tick_labels <- as.list(format(brk, x_date_format))
      }
      if (!is.null(p$x_axis_minor_breaks)) {
        mbrk <- as.Date(p$x_axis_minor_breaks)
        x_minor <- as.list(enc_x(mbrk[enc_x(mbrk) >= x_domain[1] & enc_x(mbrk) <= x_domain[2]]))
      }
      if (x_angle == 0 && nchar(x_date_format) > 4) x_angle <- 45
    } else {
      brk <- if (!is.null(p$x_axis_breaks)) sort(as.numeric(p$x_axis_breaks))
             else scales::extended_breaks(n = p$x_axis_n_breaks %||% 5)(xr)
      # "whole" skriver as.integer(x): brutna värden (2002,5) skulle få fel årtal, som i linjediagrammet
      if (p$x_number_format == "whole") brk <- brk[brk == round(brk)]
      brk <- brk[brk >= x_domain[1] & brk <= x_domain[2]]
      x_ticks <- as.list(brk)
      x_tick_labels <- as.list(if (p$x_number_format == "whole") as.character(as.integer(brk))
                               else scales::label_number(big.mark = " ", decimal.mark = ",")(brk))
      if (!is.null(p$x_axis_minor_breaks)) {
        mbrk <- as.numeric(p$x_axis_minor_breaks)
        x_minor <- as.list(mbrk[mbrk >= x_domain[1] & mbrk <= x_domain[2]])
      }
    }
  }
  if (x_typ == "discrete" && x_angle == 0 && length(x_levels) > 8 && !p$horizontal) x_angle <- 45

  # --- Rader till JS: filter | facett | x | grupp | värde | referens ---
  filt_nyckel  <- if (!is.null(filt)) as.character(df_work[[filt]]) else rep("__alla", nrow(df_work))
  facet_nyckel <- if (!is.null(facet_var)) as.character(df_work[[facet_var]]) else rep("__alla", nrow(df_work))
  df_work$.k <- filt_nyckel; df_work$.f <- facet_nyckel; df_work$.g <- grupp_nyckel
  df_work$.x <- enc_x(df_work[[x_value]])
  df_work$.o <- seq_len(nrow(df_work))   # radordning, används för att bryta lika värden vid sortering
  # Extra tooltipkolumner: formateras per kolumn över alla rader (decimalerna hoppar inte mellan filtervärden)
  tt_kol <- func_d3_tooltip_kolumner(df_work, p$tooltip_columns, p$tooltip_decimals, p$data_label_decimals)
  tt_namn <- tt_kol$namn
  df_work[tt_namn] <- tt_kol$values
  # tooltip_x_format: egen rubrik i tooltipen (.xt behålls, den används för markering och "x_y"-etiketter)
  har_xl <- !is.null(p$tooltip_x_format)
  if (har_xl) df_work$.xl <- func_d3_x_tooltip(df_work[[x_value]], p$tooltip_x_format)
  # data_label_column: etiketten som den är (tom text = ingen etikett)
  har_lt <- !is.null(p$data_label_column)
  if (har_lt) {
    lt <- as.character(df_work[[p$data_label_column]])
    lt[!is.na(lt) & !nzchar(lt)] <- NA_character_
    df_work$.lt <- lt
  }
  df_rows <- df_work[!is.na(df_work$.x), , drop = FALSE]
  # En stapel per filter/facett/x/grupp. Flera rader med samma kombination summeras (geom_col staplar
  # dem på varandra i ggplot2, så totalen blir densamma).
  dubbletter <- sum(duplicated(df_rows[c(".k", ".f", ".x", ".g")]))
  if (dubbletter > 0) message("func_d3_stapeldiagram: ", dubbletter, " dubblettrader (samma x och grupp) summeras.")
  summa <- function(v) if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
  df_rows <- df_rows %>% group_by(.k, .f, .x, .g) %>%
    summarise(.v = summa(.data[[y_value]]), .o = min(.o), .xt = as.character(first(.data[[x_value]])),
              .ref = if (!is.null(p$reference_column)) first(.data[[p$reference_column]]) else NA,
              .xl = if (har_xl) first(.xl) else NA, .lt = if (har_lt) func_d3_forsta_varde(.lt) else NA,
              across(all_of(tt_namn), func_d3_forsta_varde), .groups = "drop") %>%
    arrange(.o)
  rader <- lapply(seq_len(nrow(df_rows)), function(i) {
    r <- list(k = df_rows$.k[i], f = df_rows$.f[i], x = df_rows$.x[i], g = df_rows$.g[i],
              v = df_rows$.v[i], o = df_rows$.o[i], xt = df_rows$.xt[i],
              ref = if (!is.null(p$reference_column)) df_rows$.ref[i] else NULL)
    if (har_xl) r$xl <- df_rows$.xl[i]
    if (har_lt) r$lt <- df_rows$.lt[i]   # NA -> null (ingen etikett)
    if (length(tt_namn)) r$tt <- lapply(tt_namn, function(n) df_rows[[n]][i])   # NA -> null (visas inte)
    r
  })
  filter_values <- if (!is.null(filt)) {
    fv <- df_work[[filt]]
    as.character(if (is.factor(fv)) levels(droplevels(fv)) else sort(unique(fv)))
  } else "__alla"
  filter_default <- if (!is.null(p$interactive_filter_default) && as.character(p$interactive_filter_default) %in% filter_values)
    as.character(p$interactive_filter_default) else filter_values[length(filter_values)]

  # Visningsordning för first/last och skip: med sort_by sorterar JS kategorierna (categoriesFor: summa per x,
  # lika värden i dataordning; eller namn) på raderna MED värde (rowsFor tar bort saknade) — över alla paneler
  # när kategoriaxeln är gemensam, annars per panel. Urvalet görs i samma ordning som visas; ett x utan något
  # värde ritas inte och får ingen rang (och ingen etikett)
  sort_ok <- c("value_asc", "value_desc", "name_asc", "name_desc")
  sort_req <- if (!is.null(p$sort_by) && p$sort_by %in% sort_ok && x_typ == "discrete") p$sort_by else NULL
  fri_kategori <- !is.null(facet_var) && p$facet_scales %in% c("free", if (isTRUE(p$horizontal)) "free_y" else "free_x")
  rang_per <- c(".k", if (fri_kategori) ".f")
  df_work <- df_work %>% group_by(across(all_of(rang_per))) %>% mutate(.rank = {
    har <- !is.na(.data[[y_value]]) & !is.na(.x)
    # Lika värden: JS använder radordningen .o från df_rows = minsta .o per (x, grupp) över ALLA rader
    # (även dubbletter med saknat värde), men bara för grupper som har ett värde
    o_grp <- stats::ave(.o, .x, .g, FUN = min)
    if (is.null(sort_req)) match(.x, unique(.x[order(.data[[x_value]])]))
    else if (!any(har)) rep(NA_integer_, n())
    else if (sort_req %in% c("value_asc", "value_desc")) {
      s <- tapply(.data[[y_value]][har], .x[har], sum); o <- tapply(o_grp[har], .x[har], min)
      ux <- names(s)[order((if (sort_req == "value_desc") -1 else 1) * s, o)]
      match(.x, ux)
    } else {
      ux <- sort(unique(.x[har])); if (sort_req == "name_desc") ux <- rev(ux)
      match(.x, ux)
    }
  }) %>% ungroup()

  # --- Dataetiketter: samma urval som func_graf_barchart, per filtervärde ---
  etiketter <- list()
  x_filter <- p$data_label_filter %||% p$data_label_x_values   # används även av summaetiketterna nedan
  if (p$data_labels && nrow(df_work) > 0) {
    grp_vars <- c(".k", if (!is.null(group)) ".g", if (!is.null(facet_var)) ".f")
    # Utan sort_by: dataordningen, som func_graf_barchart. Med sort_by: den visade ordningen och bara staplar
    # som ritas (en stapel utan värde — i en panel eller för en grupp — tar annars first/last- eller skip-platsen)
    df_labels <- if (is.null(sort_req)) df_work
                 else df_work %>% filter(!is.na(.rank), !is.na(.data[[y_value]])) %>% arrange(.rank)
    if (p$data_label_first_last) df_labels <- df_labels %>% group_by(across(all_of(grp_vars))) %>% slice(c(1, n())) %>% ungroup()
    if (!is.null(x_filter)) df_labels <- df_labels %>% filter(.data[[x_value]] %in% x_filter)
    if (!is.null(p$data_label_y_values)) df_labels <- df_labels %>% filter(.data[[y_value]] %in% p$data_label_y_values)
    if (!is.null(p$data_label_group_values) && !is.null(group)) df_labels <- df_labels %>% filter(.data[[group]] %in% p$data_label_group_values)
    df_labels <- df_labels %>% group_by(.k, .f, .x, .g) %>% slice(1) %>% ungroup()
    if (!is.null(p$data_label_skip) && p$data_label_skip > 1) {
      df_labels <- df_labels %>% group_by(across(all_of(grp_vars))) %>%
        arrange(if (is.null(sort_req)) .data[[x_value]] else .rank, .by_group = TRUE) %>%
        filter(row_number() %% p$data_label_skip == 1) %>% ungroup()
    }
    etiketter <- lapply(seq_len(nrow(df_labels)), function(i) {
      list(k = df_labels$.k[i], f = df_labels$.f[i], x = df_labels$.x[i], g = df_labels$.g[i])
    })
  }
  # Summaetiketter (staplad/100 % + "outside", en per stapel) — samma urval som func_graf_barchart:
  # staplar med minst ett värde, x-filter, y_values matchar summan, first/last och skip per facett (inte grupp).
  # Används av JS i staplat läge; grupperat läge använder etiketter ovan
  summa_etiketter <- list()
  if (p$data_labels && nrow(df_work) > 0 && !is.null(group)) {
    df_tot <- df_work %>% group_by(.k, .f, .x) %>%
      summarise(.tot = sum(.data[[y_value]], na.rm = TRUE), .n = sum(!is.na(.data[[y_value]])),
                .xs = first(.data[[x_value]]), .rank = first(.rank), .groups = "drop") %>%
      filter(.n > 0) %>%
      arrange(.rank)   # visningsordningen (sort_by), annars x-ordningen
    tot_vars <- c(".k", if (!is.null(facet_var)) ".f")
    if (p$data_label_first_last) df_tot <- df_tot %>% group_by(across(all_of(tot_vars))) %>% slice(c(1, n())) %>% ungroup()
    if (!is.null(x_filter)) df_tot <- df_tot %>% filter(.xs %in% x_filter)
    if (!is.null(p$data_label_y_values)) df_tot <- df_tot %>% filter(.tot %in% p$data_label_y_values)
    if (!is.null(p$data_label_skip) && p$data_label_skip > 1) {
      df_tot <- df_tot %>% group_by(across(all_of(tot_vars))) %>% filter(row_number() %% p$data_label_skip == 1) %>% ungroup()
    }
    summa_etiketter <- lapply(seq_len(nrow(df_tot)), function(i) list(k = df_tot$.k[i], f = df_tot$.f[i], x = df_tot$.x[i]))
  }

  # --- Tooltip-decimaler och y-axel ---
  y_varden <- df_work[[y_value]]
  tt_decimaler <- func_d3_tooltip_decimaler(p$tooltip_decimals, c(y_varden, if (!is.null(p$reference_column)) df_work[[p$reference_column]]),
                                           p$data_label_decimals)

  facet_levels <- if (!is.null(facet_var)) levels(df_work[[facet_var]]) else "__alla"
  facet_levels <- facet_levels[facet_levels %in% facet_nyckel]
  n_facets <- length(facet_levels)
  ncol_val <- if (!is.null(facet_var)) (p$facet_ncol %||% (if (n_facets <= 2) n_facets else if (n_facets <= 4) 2 else 3)) else 1

  expansion_mult <- if (p$data_labels) { if (!is.null(facet_var)) c(0.1, 0.25) else c(0.05, 0.20) } else c(0.05, 0.15)
  # "outside"-etiketter på negativa staplar hänger nedanför stapeln — samma utrymme nedtill som upptill (som originalet)
  if (p$data_labels && p$data_label_position == "outside" && any(df_work[[y_value]] < 0, na.rm = TRUE)) {
    expansion_mult[1] <- expansion_mult[2]
  }
  # y_axis_expand ersätter standardluften (liggande staplars etikettutrymme nedan kan fortfarande öka den)
  if (!is.null(p$y_axis_expand)) expansion_mult <- as.numeric(p$y_axis_expand)
  # Liggande staplar: utrymmet växer med den visade etikettens längd och textstorlek (samma uppskattning som originalet:
  # summor vid staplat "outside", x100 och "%" vid procent, gruppnamn/x-värde som prefix, ~200 pt bred panel)
  # Inte när y_axis_expand anges (luften är då användarens) eller med data_label_overflow = "inside"
  if (isTRUE(p$horizontal) && p$data_labels && p$data_label_position == "outside" &&
      is.null(p$y_axis_expand) && p$data_label_overflow != "inside") {
    shows_totals <- p$bar_type %in% c("stacked", "filled") && !is.null(group)
    shown <- if (shows_totals) {
      df_work %>% group_by(.k, .f, .x) %>% summarise(v = sum(.data[[y_value]], na.rm = TRUE), .groups = "drop") %>% pull(v)
    } else df_work[[y_value]]
    # Procentetiketter: data_label_format = "percentage" ("12,3%") eller y_axis_format = "percent" ("12,3 %")
    procent_lab <- p$data_label_format == "percentage" || (p$y_axis_format == "percent" && p$data_label_format == "number")
    if (procent_lab) shown <- 100 * shown
    num_char <- max(nchar(format(round(abs(shown), p$data_label_decimals),
                                 nsmall = p$data_label_decimals, big.mark = " ", scientific = FALSE, trim = TRUE)),
                    na.rm = TRUE) + 1 +
                (if (p$data_label_format == "percentage") 1 else if (procent_lab) 2 else 0)
    prefix_char <- (if (p$data_label_format == "x_y") max(nchar(as.character(df_work[[x_value]])), na.rm = TRUE) +
                      nchar(p$data_label_group_separator) else 0) +
                   (if (!is.null(group) && p$data_label_show_group && !shows_totals)
                      max(nchar(as.character(df_work[[group]])), na.rm = TRUE) + nchar(p$data_label_group_separator) else 0)
    n_char <- num_char + prefix_char
    # Textetiketter radbryts vid data_label_width — men själva talet bryts aldrig (hårt mellanslag)
    if (!is.null(p$data_label_width) && prefix_char > 0) n_char <- max(min(n_char, p$data_label_width), num_char)
    # data_label_column: den längsta raden i texten (summaetiketterna på staplade staplar är fortfarande tal)
    if (har_lt && !shows_totals && any(!is.na(df_work$.lt)))
      n_char <- max(nchar(unlist(strsplit(stats::na.omit(df_work$.lt), "\n", fixed = TRUE))), 1)
    need <- n_char * p$data_label_size * ggplot2::.pt * 0.65 / 200 + 0.03   # 0.65 em: Verdana är bred (samma ~200 pt som originalet)
    if (any(df_work[[y_value]] >= 0, na.rm = TRUE)) expansion_mult[2] <- max(expansion_mult[2], need)
    if (any(df_work[[y_value]] < 0,  na.rm = TRUE)) expansion_mult[1] <- max(expansion_mult[1], need)
  }

  sort_start <- sort_req %||% "original"

  spec <- list(
    typ = "stapeldiagram", js_fil = "stapeldiagram",
    empty = nrow(df_work) == 0,
    width = p$width, height = p$height, font = func_d3_typsnitt(p$font),
    horizontal = p$horizontal, bar_type = p$bar_type, bar_width = bar_width_enc,
    text = list(title = func_d3_rader(p$title, p$title_width),
                subtitle = func_d3_rader(p$subtitle, p$subtitle_width),
                caption = func_d3_rader(p$caption),
                title_size = func_d3_pt_till_px(p$title_size),
                subtitle_size = func_d3_pt_till_px(p$subtitle_size),
                caption_size = func_d3_pt_till_px(p$caption_size),
                x_axis_title = p$x_axis_title, y_axis_title = p$y_axis_title,
                x_axis_title_size = func_d3_pt_till_px(p$x_axis_title_size),
                y_axis_title_size = func_d3_pt_till_px(p$y_axis_title_size),
                x_axis_title_angle = p$x_axis_title_angle,
                x_axis_text_size = func_d3_pt_till_px(p$x_axis_text_size),
                y_axis_text_size = func_d3_pt_till_px(p$y_axis_text_size),
                facet_title_size = func_d3_pt_till_px(p$facet_title_size)),
    x = list(type = x_typ, levels = as.list(x_levels), domain = x_domain, ticks = x_ticks, tick_labels = x_tick_labels,
             minor = x_minor,
             angle = x_angle, number_format = p$x_number_format,
             # Kategoriaxelns text med radbrytningar (x_axis_text_wrap, "\n"), parallell med levels; bara när den skiljer sig
             level_labels = if (x_typ == "discrete") {
               ll <- func_d3_radbryt(x_levels, p$x_axis_text_wrap)
               if (!identical(ll, as.character(x_levels))) as.list(ll)
             },
             reverse = isTRUE(p$reverse_categories) && x_typ == "discrete"),
    y = list(min = p$y_axis_min, max = p$y_axis_max, breaks = if (!is.null(p$y_axis_breaks)) as.list(p$y_axis_breaks) else NULL,
             n_breaks = p$y_axis_n_breaks %||% (if (!is.null(facet_var)) 4 else 6), auto = p$y_axis_auto_breaks,
             expand = expansion_mult, format = p$y_axis_format),
    facets = list(levels = as.list(facet_levels), ncol = ncol_val, scales = p$facet_scales,
                  wrapped = !is.null(facet_var) && any(grepl("\n", facet_levels))),
    groups = grupper, has_groups = !is.null(group),
    legend = list(position = p$legend_position, title = if (!is.null(group)) (p$legend_title %||% group) else NULL,
                  text_size = func_d3_pt_till_px(p$legend_text_size), title_size = func_d3_pt_till_px(p$legend_title_size),
                  reverse = isTRUE(p$legend_reverse)),
    rows = rader,
    filter = list(enabled = !is.null(filt), label = filt, values = as.list(filter_values), default = filter_default,
                  same_y = p$interactive_filter_same_y),
    labels = list(show = p$data_labels, keys = etiketter, total_keys = summa_etiketter, position = p$data_label_position,
                  size = func_d3_mm_till_px(p$data_label_size),
                  color = if (p$data_label_color == "auto") "auto" else func_d3_farg(p$data_label_color),
                  box = p$data_label_box, box_color = func_d3_farg(p$data_label_box_color),
                  format = p$data_label_format, decimals = p$data_label_decimals, width = p$data_label_width,
                  nudge_x = p$data_label_nudge_x, nudge_y = p$data_label_nudge_y,
                  repel = p$data_label_repel, force = p$data_label_force, max_overlaps = p$data_label_max_overlaps,
                  show_group = p$data_label_show_group && !is.null(group), separator = p$data_label_group_separator,
                  column = har_lt, overflow = p$data_label_overflow,
                  inside_color = func_d3_farg(p$data_label_inside_color)),
    highlight = list(x = as.list(as.character(p$highlight_x_values %||% character(0))),
                     # Datum-/tal-x: samma värden i JS-kodningen, så axeletiketten kan göras fet
                     x_enc = if (x_typ != "discrete" && length(p$highlight_x_values)) as.list(enc_x(p$highlight_x_values)) else list(),
                     color = func_d3_farg(p$highlight_color), other_alpha = p$highlight_other_alpha),
    reference = list(enabled = !is.null(p$reference_column), label = p$reference_label, color = func_d3_farg(p$reference_color)),
    ref = list(y = as.list(p$y_break_line), y_label = as.list(p$y_break_line_label)),   # en etikett per linje
    color_negative = func_d3_farg(p$color_negative),
    text_scale = p$text_scale,   # för D3-egna etiketter (referenslinjer)
    grid = func_d3_stodlinjer(p),   # major/minor: TRUE/FALSE/"x"/"y" (visade axlar), färg och tjocklek
    axis_line = list(x = p$x_axis_line, y = p$y_axis_line),
    bar_line = list(color = if (is.null(p$bar_line_color)) NULL else func_d3_farg(p$bar_line_color),
                    width = p$bar_line_width * 72.27 / 25.4),   # ggplot linewidth -> px
    tooltip = list(show = p$tooltip_show, decimals = tt_decimaler, unit = p$tooltip_unit %||% "", set_decimals = p$tooltip_decimals,
                   columns = tt_kol$labels),
    interactive = list(type_toggle = p$interactive_bar_type_toggle && !is.null(group),
                       sort = p$interactive_sort && x_typ == "discrete", sort_start = sort_start,
                       legend = p$interactive_legend, export = p$interactive_export),
    animation = list(grow = p$animation_grow, duration = p$animation_duration, play_interval = p$animation_play_interval)
  )

  if (isTRUE(p$debug)) {
    attr(spec, "debug") <- list(data = df_work, x_type = class(df_work[[x_value]]), y_type = class(df_work[[y_value]]),
                                x_is_date = x_is_date, x_is_continuous = x_is_continuous,
                                y_is_continuous = is.numeric(df_work[[y_value]]),
                                group_levels = farg_ordning, spec = spec)
  }
  spec
}
