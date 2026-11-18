#' Rita en interaktiv karta över förändring mellan två år
#'
#' Interaktiv D3-version av `funcVGR::func_graf_karta_change()` med samma parametrar och
#' standardvärden. Kartan visar förändringen från första till senaste året i
#' `year_column`. Ett tidsreglage visar förändringen till mellanliggande år, och knappar
#' växlar till nivåkartan. Tooltipen visar startvärde, slutvärde, förändring och
#' rangordning.
#'
#' @details
#' Förändringen räknas som absolut skillnad eller, med `percentage_growth = TRUE`, i
#' procent. Kartan byggs med samma motor som [func_d3_karta()] och har samma interaktiva
#' tillägg (zoom, sökruta, bubblor, överlagrade gränser och export).
#'
#' Funktionen ritar inte diagrammet direkt. Den returnerar ett objekt som sparar de
#' argument du angett, och diagrammet beräknas först när objektet skrivs ut eller sparas.
#' Därför går det att ändra inställningar i efterhand med `+`, t.ex.
#' `p + d3_labs(title = "Ny rubrik")` (se [d3_plot()]).
#'
#' **Pekskärm.** På en pekskärm visar det första trycket på ett område dess tooltip, och
#' nästa tryck på samma område zoomar dit. Ett tryck utanför områdena stänger tooltipen.
#' Nyp för att zooma och dra med ett finger för att flytta kartan.
#'
#' **Flera kartor med samma underlag.** Geometrin skrivs bara en gång per dokument eller fil,
#' även när flera kartor har samma underlag (se [func_d3_karta()]).
#'
#' @section Skillnader mot func_graf_karta_change():
#' * `breaks_method = "equal"` använder samma rättade logik som `func_graf_karta()`.
#'   Originalet behåller maxvärdet som en egen brytpunkt, vilket ger en extra översta
#'   klass med bara maxvärdet.
#' * Klassetiketterna har decimalkomma och tankstreck när en gräns är negativ.
#' * Etiketter som flyttas isär använder en egen algoritm i stället för ggrepel, och glöd
#'   ritas som en textkontur.
#'
#' @param df Data frame eller `sf`-objekt med tidsseriedata. Varje område finns en gång
#'   per år och har en geometrikolumn. Obligatorisk.
#' @param df_value Namn på kolumnen med värden, som text, t.ex. `"befolkning"`.
#'   Förändringen räknas från första till senaste året. Obligatorisk.
#' @param basemap `sf`-objekt som bakgrundskarta, t.ex. läns- eller regiongräns. `NULL`
#'   (standard) ger ingen bakgrundskarta.
#' @param geom_col Namn på geometrikolumnen i `df`, som text, t.ex. `"geom"` eller
#'   `"geometry"`. Obligatorisk.
#' @param year_column Namn på kolumnen med år eller period, som text.
#' @param basemap_color Fyllnadsfärg för bakgrundskartan.
#' @param basemap_color_line Linjefärg för bakgrundskartans gränser.
#' @param data_color_line Linjefärg mellan dataområdena.
#' @param data_line_size Linjetjocklek för dataområdenas gränser (typiskt 0,1–1).
#' @param title Huvudrubrik. `NULL` (standard) ger ingen rubrik.
#' @param text_scale Faktor som alla teckenstorlekar multipliceras med (rubriker, axlar,
#'   förklaring, panelrubriker och etiketter). 1 = storlekarna som de är angivna, t.ex.
#'   1.3 för stora exporter.
#' @param title_size Rubrikens teckenstorlek i punkter.
#' @param title_text_size Alias för `title_size` (bakåtkompatibilitet). Vinner om den
#'   anges.
#' @param title_width Antal tecken innan rubriken radbryts.
#' @param title_margin_top Marginal ovanför rubriken i punkter.
#' @param title_position Rubrikens placering: `"plot"` (hela bredden) eller `"panel"`
#'   (över kartan).
#' @param subtitle Underrubrik. Årsintervallet (t.ex. "2015-2024") läggs alltid till.
#'   `NULL` (standard) ger bara årsintervallet.
#' @param subtitle_text_size Underrubrikens teckenstorlek i punkter.
#' @param caption Text längst ner, t.ex. källa och noter. `NULL` (standard) ger ingen
#'   text.
#' @param caption_size Teckenstorlek för `caption` i punkter.
#' @param caption_text_size Alias för `caption_size` (bakåtkompatibilitet). Vinner om
#'   den anges.
#' @param margin_top Marginal ovanför kartan i punkter.
#' @param margin_right Marginal till höger om kartan i punkter.
#' @param margin_bottom Marginal under kartan i punkter.
#' @param margin_left Marginal till vänster om kartan i punkter.
#' @param legend_title Förklaringens rubrik. `NULL` (standard) ger ingen rubrik.
#' @param legend_name Alias för `legend_title` (bakåtkompatibilitet). Vinner om den
#'   anges.
#' @param legend_position Förklaringens placering: `"right"`, `"left"`, `"top"`,
#'   `"bottom"` eller `"none"`.
#' @param legend_key_width Bredd på förklaringens symboler i mm (typiskt 5–30).
#' @param legend_key_height Höjd på förklaringens symboler i mm (typiskt 5–30).
#' @param legend_text_size Förklaringens teckenstorlek i punkter.
#' @param legend_key_shape Symbolform i förklaringen: `"square"` eller `"circle"`.
#' @param color Basfärg för färgskalan (den mörkaste färgen).
#' @param color_low Färg för den ljusa änden av färgskalan.
#' @param color_position Vilka värden som får mörkast färg: `"highest"` (höga värden
#'   mörka) eller `"lowest"`.
#' @param breaks_method Klassindelning: `"equal"` (lika breda intervall), `"quantile"`
#'   (lika många i varje klass) eller `"custom"` (egna gränser i `breaks_custom`).
#' @param breaks_custom Numerisk vektor med egna klassgränser. Används bara när
#'   `breaks_method = "custom"`.
#' @param breaks_num_group Antal klasser (1–20).
#' @param breaks_decimals Antal decimaler i klassgränsernas etiketter. Klassen omfattar sin
#'   nedre gräns men inte sin övre. Med heltalsgränser och heltalsvärden skrivs klassen
#'   [0, 2) som "0-1"; har värdena decimaler skrivs den med minst en decimal, "0,0-1,9", så
#'   att etiketten visar vilka värden som ingår.
#' @param breaks_round_labels `TRUE` flyttar upp den nedre gränsen i varje intervall ett
#'   steg för renare etiketter (t.ex. "5,0-5,9" i stället för "4,9-5,9").
#' @param percentage_growth `TRUE` räknar procentuell förändring i stället för absolut
#'   förändring.
#' @param data_label_show Visa etiketter på kartan.
#' @param data_label_text_size Etiketternas teckenstorlek i mm (ggplot2:s textenhet, 3
#'   mm ≈ 8,5 punkter).
#' @param data_label_text_color Etiketternas textfärg. `"auto"` ger mörk text på ljusa
#'   områden och vit text på mörka, efter områdets färg; passar med
#'   `data_label_box = FALSE`.
#' @param data_label_box Visa en ruta bakom etiketterna.
#' @param data_label_box_color Rutans bakgrundsfärg (när `data_label_box = TRUE`).
#' @param data_label_box_alpha Rutans opacitet (0 = genomskinlig, 1 = helt täckande).
#' @param data_label_decimals Antal decimaler i etiketterna.
#' @param data_label_glow Lägg en glöd runt etiketterna (ritas som en mjuk textkontur).
#' @param data_label_glow_color Glödens färg (när `data_label_glow = TRUE`).
#' @param data_label_glow_sigma Glödens bredd; högre ger mer spridning (typiskt 1–10).
#' @param data_label_glow_expand Extra bredd som läggs till `data_label_glow_sigma`
#'   (typiskt 1–5).
#' @param data_label_column Kolumn med etikettext, som text. `NULL` (standard) ger
#'   förändringsvärdet.
#' @param data_label_show_value Om `TRUE` och `data_label_column` anges visas både text
#'   och förändringsvärde, t.ex. "Borås: 123".
#' @param data_label_separator Avskiljare mellan text och värde när
#'   `data_label_show_value = TRUE`.
#' @param data_label_thousands_sep Använd tusentalsavgränsare i etiketterna.
#' @param data_label_thousands_mark Tusentalsavgränsare, t.ex. `" "` (hårt mellanslag),
#'   `","` eller `"."`. `NULL` ger ingen.
#' @param data_label_zoom `TRUE` visar etiketterna först när man har zoomat in så långt
#'   att de flesta inte krockar: minst 75 % av etiketterna i kartutsnittet ska gå fri från
#'   sina grannar (två etiketter får överlappa varandra med högst `data_label_zoom_overlap`
#'   av bredden eller höjden). Innan dess visas texten "Zooma in för att visa värden".
#'   Passar kartor med många små områden, t.ex. RegSO. `FALSE` (standard) visar
#'   etiketterna direkt.
#' @param data_label_zoom_overlap Hur mycket två etiketter får överlappa varandra och ändå
#'   räknas som fria när `data_label_zoom = TRUE`, som andel av etikettens bredd eller höjd
#'   (0–0,9). Lägre värde ger mindre överlappning men kräver att man zoomar in mer innan
#'   etiketterna visas; `0` tillåter ingen överlappning. Standard är `0.4`.
#' @param data_label_repel Flytta isär etiketterna så att de inte överlappar.
#'   D3-versionen har en egen algoritm (inte ggrepel).
#' @param data_label_force Extra avstånd mellan etiketter som flyttats isär; högre ger
#'   mer avstånd (typiskt 0,5–5).
#' @param data_label_max_overlaps Etiketter som fortfarande överlappar fler än så här
#'   många andra döljs.
#' @param data_label_nudge_x Vågrät förskjutning av etiketterna i kartans enheter
#'   (positiv = åt höger).
#' @param data_label_nudge_y Lodrät förskjutning av etiketterna i kartans enheter
#'   (positiv = uppåt).
#' @param data_label_line Visa linjer från flyttade etiketter till deras område.
#' @param data_label_line_color Färg på linjerna från etiketterna.
#' @param data_label_line_size Tjocklek på linjerna från etiketterna (typiskt 0,1–1).
#' @param data_label_outside_map `TRUE` placerar etiketterna i kolumner till vänster och
#'   höger om kartan, med linjer till områdena (kräver `data_label_repel = TRUE`).
#' @param data_label_expand_factor Utrymme för etiketter utanför kartan, som andel av
#'   kartans bredd (0,5 = 50 %, fördelat på båda sidor).
#' @param data_label_filter Visa etiketter bara för de här områdena (teckenvektor med
#'   namn eller koder). `NULL` (standard) visar alla.
#' @param data_label_filter_column Kolumn som `data_label_filter` matchas mot, som text.
#'   `NULL` (standard) väljer `"region_kod"` eller `"kommun"` om de finns.
#' @param debug `TRUE` returnerar en lista med diagnostik (data, klassindelning och den
#'   beräknade specifikationen) i stället för diagrammet.
#' @param font Typsnitt, t.ex. `"Verdana"` eller `"Arial"`. Måste vara installerat på
#'   datorn, annars används `"sans"` (med ett meddelande).
#' @param time_default År som visas när kartan öppnas (förändringen från första året
#'   till detta år). `NULL` (standard) ger det senaste året.
#' @param time_label_show Visa vald period med stor text i kartans hörn.
#' @param basemap_fit `TRUE` gör kartutsnittet så stort att hela bakgrundskartan syns
#'   (som i ggplot2-versionen), `FALSE` zoomar till dataområdena.
#' @param geom_id_column Kolumn som identifierar varje område över åren, som text.
#'   `NULL` (standard) letar efter t.ex. `"region_kod"`, `"kommunkod"` eller `"kommun"`,
#'   annars används geometrins position.
#' @param geom_simplify_tolerance Förenkla geometrierna innan de skickas till
#'   webbläsaren, i kartans enheter (t.ex. 50 = 50 m i SWEREF99 TM). Ger en mindre
#'   HTML-fil. `NULL` (standard) förenklar inte.
#' @param overlay `sf`-objekt som ritas som konturer ovanpå data, t.ex. kommunalförbund.
#'   `NULL` (standard) ger inget.
#' @param overlay_color Linjefärg för `overlay`.
#' @param overlay_line_size Linjetjocklek för `overlay`, i samma enhet som
#'   `data_line_size`.
#' @param overlay_label_column Kolumn i `overlay` med namn som skrivs ut i varje område,
#'   som text. `NULL` (standard) ger inga namn.
#' @param overlay_label_size Teckenstorlek för namnen i `overlay`, i mm (som ggplot2:s
#'   `geom_sf_text`).
#' @param bubble_column Kolumn med värden som visas som proportionella cirklar i varje
#'   område, som text, t.ex. `"befolkning"`. `NULL` (standard) ger inga bubblor.
#' @param bubble_color Bubblornas fyllnadsfärg.
#' @param bubble_alpha Bubblornas opacitet (0–1).
#' @param bubble_max_size Radie i pixlar för den största bubblan.
#' @param bubble_line_color Bubblornas kantfärg.
#' @param legend_title_level Förklaringens rubrik när kartan växlas till nivåläget.
#'   `NULL` (standard) ger samma som `legend_title`.
#' @param color_negative Färg för klasser som helt ligger under noll, vilket ger en
#'   tvåfärgad skala för minskning och ökning. `NULL` (standard) ger en färgskala.
#' @param color_na Fyllnadsfärg för områden som saknar värde.
#' @param tooltip_show Visa en tooltip med startvärde, slutvärde och förändring.
#' @param tooltip_name_column Kolumn med områdenas namn, som text. `NULL` (standard)
#'   letar efter `"kommunnamn"`, `"kommun"`, `"region"` eller `"namn"`.
#' @param tooltip_columns Extra kolumner som visas i tooltipen, gärna namngivna för
#'   visning, t.ex. `c("Befolkning" = "bef")`. `NULL` (standard) ger inga.
#' @param tooltip_rank Visa rangordning, t.ex. "Plats 12 av 49".
#' @param tooltip_rank_order Rangordning: `"desc"` (störst förändring = plats 1) eller
#'   `"asc"`.
#' @param tooltip_class Visa raden med områdets klass (förklaringens etikett, t.ex.
#'   "0,0-1,9"). `FALSE` tar bort raden.
#' @param tooltip_value_label Etikett för förändringsraden. `NULL` (standard) ger
#'   "Förändring" eller "Förändring (%)".
#' @param tooltip_unit Enhet som läggs efter nivåvärden och absolut förändring, t.ex. `"
#'   kr"`. Med `percentage_growth = TRUE` visas förändringen med " %". `NULL`
#'   (standard) ger ingen enhet.
#' @param tooltip_unit_change Enhet för förändringsvärdet, t.ex. `" procentenheter"` när
#'   `df_value` är en andel i procent: skillnaden mellan två andelar är då
#'   procentenheter, inte procent. `NULL` (standard) ger samma enhet som `tooltip_unit`,
#'   eller " %" när `percentage_growth = TRUE`.
#' @param tooltip_decimals Antal decimaler i tooltipens värden. `NULL` (standard) ger 0
#'   om alla värden är heltal, annars `data_label_decimals`, men aldrig färre än
#'   `breaks_decimals` eller klassetiketterna. Räknas på förändringsvärdena; raderna för start- och slutår på
#'   nivåvärdena.
#' @param interactive_zoom Zooma och panorera med mushjulet, och klicka på ett område
#'   för att zooma dit. Man kan zooma in så långt att även det minsta området fyller
#'   kartan (högst 1 000 gånger).
#' @param interactive_zoom_group_column Kolumn som grupperar områdena, som text (t.ex.
#'   `"kf"`). Ett klick på ett område zoomar då till hela gruppen. `NULL` (standard)
#'   zoomar till området.
#' @param interactive_search Visa en sökruta för områdenas namn. Medan man skriver visas en
#'   lista, som går att scrolla, med namn som börjar med eller innehåller texten (högst 250). När man väljer ett namn
#'   i listan, eller skriver hela namnet, zoomar kartan direkt till området; Enter behövs
#'   inte. Ett kryss i rutan tömmer den.
#' @param interactive_legend Håll muspekaren över en klass i förklaringen för att
#'   framhäva den, klicka för att dölja eller visa den.
#' @param interactive_export Visa knappar för att ladda ned diagrammet som PNG eller
#'   SVG.
#' @param interactive_mode_toggle Visa knappar för att växla mellan förändring och nivå.
#' @param interactive_mode_labels Knapptexterna för de två lägena, som en namngiven
#'   teckenvektor med `niva` och `forandring`.
#' @param interactive_time_slider Visa ett tidsreglage som visar förändringen från
#'   första året till varje senare år.
#' @param interactive_time_range `TRUE` (standard) ger tidsreglaget två handtag i
#'   förändringsläget, så att både startår och slutår kan väljas; förändringen räknas om
#'   för den valda perioden. I nivåläget har reglaget ett handtag. `FALSE` ger ett handtag
#'   och förändring från första året, som i tidigare versioner. Klassgränserna räknas på
#'   första till sista året.
#' @param interactive_resize `TRUE` visar ett draghandtag i kartytans nedre högra hörn (ovanför
#'   en förklaring eller bildtext under kartan), så att användaren kan ändra kartytans bredd
#'   och höjd med muspekaren eller fingret. Medan man drar visar en streckad ram kartytans
#'   nya storlek, och kartan ritas om när man släpper. Med `resize_keep_scale = TRUE`
#'   (standard) behåller kartan sin skala och rutan visar ett större eller mindre utsnitt;
#'   med `FALSE` ritas kartan om så att den fyller den nya rutan. Rubriker, förklaring,
#'   bildtext och knappar behåller sin storlek, och läge, år, dolda klasser och zoom följer
#'   med. Dubbelklick på handtaget eller Återställ ger tillbaka
#'   `width` × `height`. Export till PNG och SVG får den dragna storleken. Gränserna sätts
#'   med `resize_min` och `resize_max`. `FALSE` (standard) ger inget handtag.
#' @param animation_duration Övergångstid i millisekunder när färger eller zoom ändras.
#'   0 = ingen animation.
#' @param animation_play_interval Tid i millisekunder per år när uppspelningsknappen
#'   används.
#' @param width Kartans bredd i pixlar. Med `responsive = TRUE` är det största bredden.
#' @param height Kartans höjd i pixlar vid bredden `width`.
#' @param responsive `TRUE` (standard) ritar om kartan i den bredd som finns när den inte
#'   får plats i `width`, t.ex. på en mobil eller i en smal spalt. Text, förklaring och
#'   knappar behåller då sin storlek i stället för att krympa, och höjden minskar i samma
#'   proportion. Kartan ritas om när fönstret ändrar storlek. `FALSE` ger en karta med fast
#'   storlek, som i tidigare versioner.
#' @param responsive_breakpoint Bredd i pixlar under vilken kartan räknas som smal (t.ex.
#'   en mobil i stående läge): en förklaring till vänster eller höger läggs då under kartan,
#'   så att kartan får hela bredden, och kartan blir högre än proportionellt (högst
#'   `height`). Gäller bara med `responsive = TRUE`. Standard är `600`.
#' @param resize_min Minsta bredd och höjd i pixlar när kartan dras till ny storlek, som en
#'   numerisk vektor av längd 2. Gränsen gäller hela kartan, med rubriker, förklaring och
#'   bildtext, inte bara kartytan i ramen. Gäller bara med `interactive_resize = TRUE`. Standard är
#'   `c(300, 250)`.
#' @param resize_max Största bredd och höjd i pixlar när kartan dras till ny storlek, som en
#'   numerisk vektor av längd 2, för hela kartan som `resize_min`. `NULL` (standard) begränsar kartan till ytan runt den: sidans
#'   eller spaltens bredd och, i en reveal.js-presentation, bildytans höjd. Kartan blir aldrig
#'   bredare än ytan runt den, även om `resize_max` är större. Gäller bara med
#'   `interactive_resize = TRUE`.
#' @param resize_keep_scale `TRUE` (standard) gör att kartan behåller sin skala när rutan dras
#'   till ny storlek: rutan visar då ett annat utsnitt av kartan, med övre vänstra hörnet kvar.
#'   En större ruta visar mer runt kartan, en mindre visar en del av den, och man kan panorera
#'   för att se resten. `FALSE` ritar om kartan så att den fyller den nya storleken (som i
#'   version 0.9.0). Gäller bara med `interactive_resize = TRUE`.
#' @param .rendera Intern. `TRUE` bygger diagrammet och används när objektet skrivs ut.
#'   Ändra inte.
#'
#' @return Ett objekt av klassen `vgr_d3`. Det visas i RStudio:s Viewer när det skrivs ut,
#'   i HTML-dokument från R Markdown och Quarto och på sidan från "Run examples" i
#'   hjälpen. Spara det med [func_d3_spara()].
#'   Med `debug = TRUE` returneras i stället en lista med diagnostik.
#' @seealso [func_d3_karta()], [d3_plot()] och [func_d3_spara()].
#' @export
#' @example inst/examples/func_d3_karta_change.R
func_d3_karta_change <- function(

  # === REQUIRED PARAMETERS ===
  df,
  df_value,
  basemap = NULL,
  geom_col,
  year_column = "year",

  # === MAP STYLING ===
  basemap_color = "#d3d3d3",
  basemap_color_line = "white",
  data_color_line = "black",
  data_line_size = 0.2,

  # === TITLE AND TEXT ===
  title = NULL,
  text_scale = 1,
  title_size = 16,
  title_text_size = 16,
  title_width = 50,
  title_margin_top = 2,
  title_position = "plot",
  subtitle = NULL,
  subtitle_text_size = 13,
  caption = NULL,
  caption_size = 10,
  caption_text_size = 10,

  # === MAP MARGINS ===
  margin_top = 10,
  margin_right = 30,
  margin_bottom = 10,
  margin_left = 10,

  # === LEGEND ===
  legend_title = NULL,
  legend_name = NULL,
  legend_position = "none",
  legend_key_width = 7,
  legend_key_height = 7,
  legend_text_size = 11,
  legend_key_shape = "square",

  # === COLOR SCHEME ===
  color = "#005b89",
  color_low = "white",
  color_position = "highest",

  # === DATA BREAKS/CATEGORIES ===
  breaks_method = "equal",
  breaks_custom = NULL,
  breaks_num_group = 7,
  breaks_decimals = 0,
  breaks_round_labels = FALSE,
  percentage_growth = FALSE,

  # === DATA LABELS - BASIC ===
  data_label_show = TRUE,
  data_label_text_size = 3,
  data_label_text_color = "black",
  data_label_box = TRUE,
  data_label_box_color = "white",
  data_label_box_alpha = 1,
  data_label_decimals = 1,

  # === DATA LABELS - GLOW EFFECT ===
  data_label_glow = FALSE,
  data_label_glow_color = "white",
  data_label_glow_sigma = 3,
  data_label_glow_expand = 1,

  # === DATA LABELS - CONTENT ===
  data_label_column = NULL,
  data_label_show_value = FALSE,
  data_label_separator = ": ",
  data_label_thousands_sep = TRUE,
  data_label_zoom = FALSE,
  data_label_zoom_overlap = 0.4,
  data_label_thousands_mark = " ",

  # === DATA LABELS - SMART POSITIONING ===
  data_label_repel = FALSE,
  data_label_force = 2,
  data_label_max_overlaps = 10,
  data_label_nudge_x = 0,
  data_label_nudge_y = 0,

  # === DATA LABELS - CONNECTING LINES ===
  data_label_line = TRUE,
  data_label_line_color = "gray50",
  data_label_line_size = 0.3,

  # === DATA LABELS - OUTSIDE MAP ===
  data_label_outside_map = FALSE,
  data_label_expand_factor = 0.5,

  # === DATA LABELS - FILTERING ===
  data_label_filter = NULL,
  data_label_filter_column = NULL,

  # === ADVANCED ===
  debug = FALSE,
  font = "Verdana",

  # ------------------------------------------------------------------
  # D3-tillägg: nya parametrar ligger efter originalets, så positionella
  # anrop träffar samma parametrar som i func_graf-originalet
  # ------------------------------------------------------------------

  # === D3: TIME / SLIDER ===
  time_default = NULL,
  time_label_show = FALSE,

  # === D3: MAP STYLING ===
  basemap_fit = TRUE,
  geom_id_column = NULL,
  geom_simplify_tolerance = NULL,

  # === D3: OVERLAY BOUNDARIES ===
  overlay = NULL,
  overlay_color = "#37474F",
  overlay_line_size = 0.6,
  overlay_label_column = NULL,
  overlay_label_size = 4,

  # === D3: BUBBLES ===
  bubble_column = NULL,
  bubble_color = "#ffc107",
  bubble_alpha = 0.8,
  bubble_max_size = 25,
  bubble_line_color = "white",

  # === D3: LEGEND ===
  legend_title_level = NULL,

  # === D3: COLOR SCHEME ===
  color_negative = NULL,
  color_na = "grey80",

  # === D3: TOOLTIP ===
  tooltip_show = TRUE,
  tooltip_name_column = NULL,
  tooltip_columns = NULL,
  tooltip_rank = TRUE,
  tooltip_rank_order = "desc",
  tooltip_class = TRUE,
  tooltip_value_label = NULL,
  tooltip_unit = NULL,
  tooltip_unit_change = NULL,
  tooltip_decimals = NULL,

  # === D3: INTERACTIVITY ===
  interactive_zoom = TRUE,
  interactive_zoom_group_column = NULL,
  interactive_search = TRUE,
  interactive_legend = TRUE,
  interactive_export = TRUE,
  interactive_mode_toggle = TRUE,
  interactive_mode_labels = c(niva = "Nivå", forandring = "Förändring"),
  interactive_time_slider = TRUE,
  interactive_time_range = TRUE,
  interactive_resize = FALSE,

  # === D3: ANIMATION ===
  animation_duration = 600,
  animation_play_interval = 1500,

  # === D3: SIZE ===
  width = 800,
  height = 900,
  responsive = TRUE,
  responsive_breakpoint = 600,
  resize_min = c(300, 250),
  resize_max = NULL,
  resize_keep_scale = TRUE,
  .rendera = FALSE

) {

  if (!.rendera) {
    angivna <- setdiff(names(as.list(match.call()))[-1], ".rendera")
    obj <- func_d3_nytt_objekt("karta_change", mget(angivna, envir = environment()))
    if (debug) return(attr(func_d3_spec(obj), "debug"))
    return(obj)
  }

  # Resolve alternative parameter names (last-one-wins if both are supplied)
  title_size   <- if (!missing(title_text_size))   title_text_size   else title_size
  caption_size <- if (!missing(caption_text_size)) caption_text_size else caption_size
  legend_title <- if (!missing(legend_name))       legend_name       else legend_title
  # text_scale: en faktor för alla textstorlekar (som func_graf_karta_change)
  title_size           <- title_size           * text_scale
  caption_size         <- caption_size         * text_scale
  subtitle_text_size   <- subtitle_text_size   * text_scale
  legend_text_size     <- legend_text_size     * text_scale
  data_label_text_size <- data_label_text_size * text_scale

  p <- mget(setdiff(names(formals()), c(".rendera", "title_text_size", "caption_text_size", "legend_name")),
            envir = environment())

  # Översätt till func_d3_karta_bygg:s namn
  p$time_column               <- year_column
  p$legend_title_change       <- legend_title
  p$legend_title              <- legend_title_level %||% legend_title
  p$data_label_thousands_mark <- if (data_label_thousands_sep) data_label_thousands_mark else NULL

  func_d3_karta_bygg(p, standardlage = "forandring")
}
