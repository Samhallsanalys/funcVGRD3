#' Rita ett interaktivt linjediagram
#'
#' Interaktiv D3-version av `funcVGR::func_graf_linjediagram()` med samma parametrar och
#' standardvärden, så att ett befintligt anrop fungerar om man bara byter funktionsnamn.
#' Utöver originalets parametrar finns tooltip med hårkors, klickbar förklaring, växling
#' mellan värde, index och förändring, prognosstil med osäkerhetsband (`line_*`),
#' skuggade perioder (`x_shade_*`), zoom, namn vid linjeslutet (`legend_direct`) och
#' export till PNG och SVG.
#'
#' @details
#' x-axeln kan vara tal, datum eller kategorier. Diagrammet skalar med fönstret.
#'
#' Funktionen ritar inte diagrammet direkt. Den returnerar ett objekt som sparar de
#' argument du angett, och diagrammet beräknas först när objektet skrivs ut eller sparas.
#' Därför går det att ändra inställningar i efterhand med `+`, t.ex.
#' `p + d3_labs(title = "Ny rubrik")` (se [d3_plot()]).
#'
#' @section Skillnader mot func_graf_linjediagram():
#' * `custom_theme` (ett ggplot2-tema) används inte; ett meddelande visas.
#' * I paneler ritas x-axeln i alla paneler (ggplot2 ritar den bara i nedersta raden).
#' * y-axelns markeringar väljs av D3 och kan skilja sig från ggplot2:s.
#' * `NA` i `group` eller `facet_var` blir gruppen "NA", sist och grå.
#' * Etiketter som flyttas isär använder en egen algoritm i stället för ggrepel, och glöd
#'   ritas som en textkontur.
#'
#' @param df Data frame med data. Obligatorisk.
#' @param x_value Namn på kolumnen för x-axeln, som text (kategorier, tal eller datum).
#'   Obligatorisk.
#' @param y_value Namn på kolumnen med de numeriska värdena, som text. Obligatorisk.
#' @param group Namn på kolumnen som delar upp data i linjer med egen färg, som text.
#'   `NULL` (standard) ger en linje.
#' @param facet_var Namn på kolumnen som delar upp diagrammet i paneler, som text.
#'   `NULL` (standard) ger ett diagram.
#' @param facet_ncol Antal kolumner med paneler. `NULL` (standard) räknas ut
#'   automatiskt.
#' @param facet_same_y_scale `TRUE` ger samma y-skala i alla paneler, `FALSE` oberoende
#'   skalor.
#' @param facet_title_size Panelrubrikernas teckenstorlek i punkter.
#' @param facet_title_wrap Radbryt panelrubrikerna efter så här många tecken; långa ord
#'   avstavas. `NULL` (standard) ger ingen radbrytning.
#' @param title Huvudrubrik. `NULL` (standard) ger ingen rubrik.
#' @param title_width Antal tecken innan rubriken radbryts.
#' @param subtitle Underrubrik. `NULL` (standard) ger ingen underrubrik.
#' @param subtitle_width Antal tecken innan underrubriken radbryts.
#' @param caption Text längst ner, t.ex. källa och noter. `NULL` (standard) ger ingen
#'   text.
#' @param caption_x x-koordinat för en fritt placerad `caption`, i diagrammets enheter
#'   (tal eller datum). Används tillsammans med `caption_y`. `NULL` (standard) ger
#'   standardplaceringen nere till höger.
#' @param caption_y y-koordinat för en fritt placerad `caption`, i diagrammets enheter.
#'   Används tillsammans med `caption_x`. `NULL` (standard) ger standardplaceringen nere
#'   till höger.
#' @param caption_hjust Vågrät justering av en fritt placerad `caption`: 0 = vänster, 1
#'   = höger.
#' @param caption_vjust Lodrät justering av en fritt placerad `caption`: 0 = nederkant,
#'   1 = överkant.
#' @param x_axis_title Rubrik för x-axeln. `NULL` (standard) ger ingen rubrik.
#' @param y_axis_title Rubrik för y-axeln. `NULL` (standard) ger ingen rubrik.
#' @param legend_title Förklaringens rubrik. `NULL` (standard) ger namnet på
#'   `group`-kolumnen.
#' @param colors Färger för grupperna. En namnlös vektor fördelas i gruppordning
#'   (faktornivåer utan oanvända nivåer, tal sorterade, annars ordningen i data) och
#'   återanvänds om den är för kort. En namngiven vektor matchas mot gruppnamnen, t.ex.
#'   `c("Göteborg" = "#1a9fb3", "Borås" = "#ffc107")`, och alla grupper måste då ha en
#'   färg. Standard är VGR:s färgtema "ru" (samma färger som `func_fargtema("ru")` i
#'   funcVGR); `NULL` ger viridis-färger.
#' @param line_size Linjetjocklek (ggplot2:s enhet).
#' @param line_type Linjetyp: `"solid"`, `"dashed"`, `"dotted"` eller `"dashdot"`.
#' @param font Typsnitt, t.ex. `"Verdana"` eller `"Arial"`. Måste vara installerat på
#'   datorn, annars används `"sans"` (med ett meddelande).
#' @param text_scale Faktor som alla teckenstorlekar multipliceras med (rubriker, axlar,
#'   förklaring, panelrubriker och etiketter). 1 = storlekarna som de är angivna, t.ex.
#'   1.3 för stora exporter.
#' @param title_size Rubrikens teckenstorlek i punkter.
#' @param subtitle_size Underrubrikens teckenstorlek i punkter.
#' @param caption_size Teckenstorlek för `caption` i punkter.
#' @param x_axis_text_size Teckenstorlek för x-axelns skaltext i punkter.
#' @param y_axis_text_size Teckenstorlek för y-axelns skaltext i punkter.
#' @param legend_text_size Förklaringens teckenstorlek i punkter.
#' @param legend_position Förklaringens placering: `"right"`, `"left"`, `"top"`,
#'   `"bottom"` eller `"none"`.
#' @param show_major_grid Större stödlinjer: `TRUE` = i båda riktningarna, `FALSE` =
#'   inga, `"y"` = bara linjer från y-axeln (vågräta), `"x"` = bara linjer från x-axeln
#'   (lodräta).
#' @param show_minor_grid Mindre stödlinjer, med samma val som `show_major_grid`.
#' @param x_axis_line Rita en linje längs x-axeln.
#' @param y_axis_line Rita en linje längs y-axeln.
#' @param x_angle Rotation av x-axelns text i grader (0–90).
#' @param x_angle_auto `TRUE` roterar x-axelns text 45 grader automatiskt när det finns
#'   fler än 8 värden.
#' @param x_axis_n_breaks Antal markeringar på x-axeln. Fungerar för tal, datum och
#'   kategorier; första och sista värdet behålls alltid. `NULL` (standard) väljs
#'   automatiskt (alla värden för kategorier).
#' @param x_break_line Position för en lodrät, streckad referenslinje (tal eller datum,
#'   flera går bra). `NULL` (standard) ger ingen.
#' @param y_break_line Position för en vågrät referenslinje (flera går bra). `NULL`
#'   (standard) ger ingen.
#' @param data_labels Visa värdeetiketter.
#' @param data_label_text_size Etiketternas teckenstorlek i mm (ggplot2:s textenhet, 3,5
#'   mm ≈ 10 punkter).
#' @param data_label_color Etiketternas textfärg, eller `"auto"` för linjens färg.
#' @param data_label_format Etikettformat: `"value_only"` eller `"x_y"` (visar "x: y").
#' @param data_label_filter Alias för `data_label_x_values` (behålls för
#'   kompatibilitet).
#' @param data_label_x_values Visa etiketter bara för de här x-värdena. `NULL`
#'   (standard) visar alla.
#' @param data_label_y_values Visa etiketter bara där y har de här värdena. `NULL`
#'   (standard) visar alla.
#' @param data_label_first_last Visa etiketter bara på första och sista punkten per
#'   grupp och panel.
#' @param data_label_skip Visa var N:e etikett per grupp och panel (2 = varannan).
#'   `NULL` (standard) visar alla.
#' @param data_label_repel Flytta isär etiketterna så att de inte överlappar.
#'   D3-versionen har en egen algoritm (inte ggrepel).
#' @param data_label_box Visa en ruta bakom etiketterna.
#' @param data_label_box_color Rutans bakgrundsfärg (när `data_label_box = TRUE`).
#' @param data_label_box_alpha Rutans opacitet (0 = genomskinlig, 1 = helt täckande).
#' @param data_label_nudge_x Vågrät förskjutning av etiketterna, i x-axelns enheter
#'   (dagar för datum, kategorier för text).
#' @param data_label_nudge_y Lodrät förskjutning av etiketterna, i y-axelns enheter.
#' @param data_label_direction Riktning när etiketterna flyttas isär: `"auto"` (utgår
#'   från nudge-värdena), `"both"`, `"x"` eller `"y"`.
#' @param data_label_decimals Antal decimaler i etiketterna.
#' @param data_label_show_group Skriv gruppnamnet före värdet i etiketten, t.ex. "2023:
#'   5,2".
#' @param data_label_group_separator Avskiljare mellan gruppnamn och värde när
#'   `data_label_show_group = TRUE`.
#' @param data_label_glow Lägg en glöd runt etiketterna (ritas som en mjuk textkontur).
#' @param data_label_glow_color Glödens färg (när `data_label_glow = TRUE`).
#' @param data_label_glow_sigma Glödens bredd; högre ger mer spridning (typiskt 1–10).
#' @param data_label_glow_expand Extra bredd som läggs till `data_label_glow_sigma`
#'   (typiskt 1–5).
#' @param highlight_groups Grupper som ritas i `highlight_color` med tjockare linje.
#'   `NULL` (standard) markerar inga.
#' @param highlight_color Färg för markerade grupper.
#' @param highlight_line_size Linjetjocklek för markerade grupper.
#' @param highlight_bold Visa markerade grupper med fet text i förklaringen.
#' @param group_highlight Grupper som behåller sin färg medan övriga tonas ned i grått,
#'   t.ex. `c("Göteborg", "Borås")`. Kan inte kombineras med `facet_var`. `NULL`
#'   (standard) tonar inte ned något.
#' @param group_highlight_label Skriv gruppnamnet i slutet av varje framhävd linje.
#' @param group_highlight_label_size Teckenstorlek för slutetiketterna i mm (ggplot2:s
#'   textenhet, 3,5 mm ≈ 10 punkter).
#' @param group_highlight_label_box Visa en ruta bakom slutetiketterna.
#' @param group_highlight_label_color Färg för slutetiketterna. `"inherit"` ger linjens
#'   färg. En vektor ger en färg per grupp och färgar då även linjerna.
#' @param group_highlight_unhighlighted_color Färg för de nedtonade linjerna.
#' @param group_highlight_unhighlighted_alpha Opacitet för de nedtonade linjerna (0–1).
#' @param x_axis_min Minvärde för x-axeln (tal eller datum). `NULL` (standard) väljs
#'   automatiskt.
#' @param x_axis_max Maxvärde för x-axeln (tal eller datum). `NULL` (standard) väljs
#'   automatiskt.
#' @param y_axis_min Minvärde för y-axeln. `NULL` (standard) väljs automatiskt.
#' @param y_axis_max Maxvärde för y-axeln. `NULL` (standard) väljs automatiskt.
#' @param x_axis_clip_off `FALSE` (standard) ger x-axeln 5 % luft till vänster och 10 %
#'   till höger, mer när etiketter behöver det, och klipper vid panelens kant. `TRUE`
#'   ritar etiketter även utanför panelen (tal- och datumaxel utökas då 25 % åt höger).
#' @param x_date_labels Datumformat (strftime) för en datumaxel, t.ex. `"%Y"` eller `"%b
#'   %Y"`.
#' @param x_date_breaks Avstånd mellan datummarkeringar: `"auto"` eller t.ex. `"1
#'   year"`, `"6 months"`.
#' @param x_number_format Format för en numerisk x-axel: `"whole"` (utan avgränsare,
#'   t.ex. 2000) eller `"thousands"` (med mellanslag, t.ex. 2 000).
#' @param custom_theme Används inte i D3-versionen (ett ggplot2-tema går inte att
#'   använda här, och ett meddelande visas). Finns kvar så att befintliga anrop
#'   fungerar.
#' @param debug `TRUE` returnerar en lista med diagnostik (data, klassindelning och den
#'   beräknade specifikationen) i stället för diagrammet.
#' @param line_forecast_from x-värde där linjerna blir streckade (prognos). `NULL`
#'   (standard) ger ingen prognosstil.
#' @param line_forecast_label Text vid prognosens startlinje. `NULL` ger ingen text.
#' @param line_band_lower Kolumn med nedre gräns, som text, för ett skuggat band runt
#'   varje linje. `NULL` (standard) ger inget band.
#' @param line_band_upper Kolumn med övre gräns, som text. Används tillsammans med
#'   `line_band_lower`.
#' @param line_band_alpha Bandets opacitet (0–1).
#' @param legend_direct `TRUE` skriver varje linjes namn vid dess sista punkt, i stället
#'   för eller utöver förklaringen.
#' @param x_break_line_label Text vid de lodräta referenslinjerna, en per linje
#'   (återanvänds om de är färre). `NULL` (standard) ger ingen text.
#' @param y_break_line_label Text vid de vågräta referenslinjerna, en per linje
#'   (återanvänds om de är färre), t.ex. `"Riket"`. `NULL` (standard) ger ingen text.
#' @param x_shade_from Start för skuggade perioder på x-axeln (vektor med tal, datum
#'   eller text), t.ex. `as.Date("2020-03-01")`. `NULL` (standard) ger inga.
#' @param x_shade_to Slut för skuggade perioder, lika lång som `x_shade_from`.
#' @param x_shade_label Text överst i varje skuggad period, t.ex. `"Pandemin"`. `NULL`
#'   (standard) ger ingen text.
#' @param x_shade_color Fyllnadsfärg för skuggade perioder.
#' @param tooltip_show Visa en tooltip när muspekaren hålls över diagrammet.
#' @param tooltip_mode `"all"` (hårkors som listar alla linjer vid x-värdet),
#'   `"nearest"` (bara linjen under muspekaren) eller `"auto"` (`"nearest"` när det
#'   finns fler än 12 linjer).
#' @param tooltip_decimals Antal decimaler i tooltipens värden. `NULL` (standard) ger 0
#'   om alla värden är heltal, annars `data_label_decimals`.
#' @param tooltip_unit Enhet som läggs efter tooltipens värden, t.ex. `" %"` eller `"
#'   kr"`.
#' @param tooltip_columns Extra kolumner som visas i tooltipen för varje punkt, som text.
#'   En namngiven vektor ger visningsnamnet, t.ex. `c("Antal" = "antal")`; utan namn
#'   visas kolumnnamnet. Talkolumner får egna decimaler: heltal utan decimaler, annars
#'   `tooltip_decimals` eller `data_label_decimals`. `NULL` (standard) ger inga.
#' @param interactive_legend Klicka på en grupp i förklaringen för att dölja eller visa
#'   linjen, dubbelklicka för att bara visa den linjen, håll muspekaren över den för att
#'   framhäva den.
#' @param interactive_zoom Dra över diagrammet för att zooma x-axeln, dubbelklicka för
#'   att återställa (bara för numerisk x-axel och datumaxel).
#' @param interactive_index_toggle Visa knappar för att växla mellan värde, index (bas =
#'   100) och procentuell förändring.
#' @param interactive_index_base x-värde som används som indexbas. `NULL` (standard) ger
#'   varje linjes första värde.
#' @param interactive_index_labels Knapptexterna för de tre lägena, som en namngiven
#'   teckenvektor med `varde`, `index` och `forandring` (en namnlös vektor tas i den
#'   ordningen). Saknade texter får standardtexten.
#' @param interactive_export Visa knappar för att ladda ned diagrammet som PNG eller
#'   SVG.
#' @param animation_draw Rita in linjerna från vänster till höger när diagrammet öppnas.
#' @param animation_duration Animationens längd i millisekunder. 0 = ingen animation.
#' @param width Diagrammets bredd i pixlar. Diagrammen (utom kartorna)
#'   krymper för att få plats i fönstret.
#' @param height Diagrammets höjd i pixlar.
#' @param .rendera Intern. `TRUE` bygger diagrammet och används när objektet skrivs ut.
#'   Ändra inte.
#'
#' @return Ett objekt av klassen `vgr_d3`. Det visas i RStudio:s Viewer när det skrivs ut,
#'   i HTML-dokument från R Markdown och Quarto och på sidan från "Run examples" i
#'   hjälpen. Spara det med [func_d3_spara()].
#'   Med `debug = TRUE` returneras i stället en lista med diagnostik.
#' @seealso [func_d3_stapeldiagram()], [d3_plot()] och [func_d3_spara()].
#' @export
#' @example inst/examples/func_d3_linjediagram.R
func_d3_linjediagram <- function(
    # === REQUIRED PARAMETERS ===
  df,
  x_value,
  y_value,

  # === GROUPING AND FACETING ===
  group = NULL,
  facet_var = NULL,
  facet_ncol = NULL,
  facet_same_y_scale = FALSE,
  facet_title_size = 12,
  facet_title_wrap = NULL,

  # === TITLES AND LABELS ===
  title = NULL,
  title_width = 100,
  subtitle = NULL,
  subtitle_width = 100,
  caption = NULL,
  caption_x = NULL,
  caption_y = NULL,
  caption_hjust = 0,
  caption_vjust = 0,
  x_axis_title = NULL,
  y_axis_title = NULL,
  legend_title = NULL,

  # === STYLING ===
  colors = func_d3_farger_ru(),
  line_size = 1.2,
  line_type = "solid",
  font = "Verdana",

  # === TEXT SIZING (in points) ===
  text_scale = 1,
  title_size = 16,
  subtitle_size = 13,
  caption_size = 10,
  x_axis_text_size = 11,
  y_axis_text_size = 11,
  legend_text_size = 11,

  # === LAYOUT ===
  legend_position = "none",
  show_major_grid = FALSE,
  show_minor_grid = FALSE,
  x_axis_line = TRUE,
  y_axis_line = TRUE,
  x_angle = 0,
  x_angle_auto = FALSE,
  x_axis_n_breaks = NULL,

  # === REFERENCE LINES ===
  x_break_line = NULL,
  y_break_line = NULL,

  # === DATA LABELS ===
  data_labels = FALSE,
  data_label_text_size = 3.5,
  data_label_color = "black",
  data_label_format = "value_only",
  data_label_filter = NULL,
  data_label_x_values = NULL,
  data_label_y_values = NULL,
  data_label_first_last = FALSE,
  data_label_skip = NULL,
  data_label_repel = TRUE,
  data_label_box = TRUE,
  data_label_box_color = "white",
  data_label_box_alpha = 0.8,
  data_label_nudge_x = 0,
  data_label_nudge_y = 0,
  data_label_direction = "auto",
  data_label_decimals = 0,
  data_label_show_group = FALSE,
  data_label_group_separator = ": ",
  data_label_glow = FALSE,
  data_label_glow_color = "white",
  data_label_glow_sigma = 3,
  data_label_glow_expand = 1,

  # === GROUP HIGHLIGHTING (color override method) ===
  highlight_groups = NULL,
  highlight_color = "red",
  highlight_line_size = 2,
  highlight_bold = TRUE,

  # === GGHIGHLIGHT (fade-out method) ===
  group_highlight = NULL,
  group_highlight_label = TRUE,
  group_highlight_label_size = 3.5,
  group_highlight_label_box = FALSE,
  group_highlight_label_color = "inherit",
  group_highlight_unhighlighted_color = "grey80",
  group_highlight_unhighlighted_alpha = 0.6,

  # === AXIS LIMITS ===
  x_axis_min = NULL,
  x_axis_max = NULL,
  y_axis_min = NULL,
  y_axis_max = NULL,
  x_axis_clip_off = FALSE,

  # === DATE X-AXIS FORMATTING ===
  x_date_labels = "%b %Y",
  x_date_breaks = "auto",

  # === NUMERIC X-AXIS FORMATTING ===
  x_number_format = "whole",

  # === ADVANCED ===
  custom_theme = NULL,
  debug = FALSE,

  # ------------------------------------------------------------------
  # D3-tillägg: nya parametrar ligger efter originalets, så positionella
  # anrop träffar samma parametrar som i func_graf-originalet
  # ------------------------------------------------------------------

  # === D3: FORECAST AND UNCERTAINTY ===
  line_forecast_from = NULL,
  line_forecast_label = "Prognos",
  line_band_lower = NULL,
  line_band_upper = NULL,
  line_band_alpha = 0.2,

  # === D3: LAYOUT ===
  legend_direct = FALSE,

  # === D3: REFERENCE LINES ===
  x_break_line_label = NULL,
  y_break_line_label = NULL,

  # === D3: SHADED PERIODS ===
  x_shade_from = NULL,
  x_shade_to = NULL,
  x_shade_label = NULL,
  x_shade_color = "grey90",

  # === D3: TOOLTIP ===
  tooltip_show = TRUE,
  tooltip_mode = "auto",
  tooltip_decimals = NULL,
  tooltip_unit = "",
  tooltip_columns = NULL,

  # === D3: INTERACTIVITY ===
  interactive_legend = TRUE,
  interactive_zoom = TRUE,
  interactive_index_toggle = TRUE,
  interactive_index_base = NULL,
  interactive_index_labels = c(varde = "Värde", index = "Index", forandring = "Förändring (%)"),
  interactive_export = TRUE,

  # === D3: ANIMATION ===
  animation_draw = TRUE,
  animation_duration = 900,

  # === D3: SIZE ===
  width = 1400,
  height = 800,
  .rendera = FALSE
) {

  if (!.rendera) {
    angivna <- setdiff(names(as.list(match.call()))[-1], ".rendera")
    obj <- func_d3_nytt_objekt("linjediagram", mget(angivna, envir = environment()))
    if (debug) return(attr(func_d3_spec(obj), "debug"))
    return(obj)
  }

  # text_scale: en faktor för alla textstorlekar (som func_graf_linjediagram)
  title_size       <- title_size       * text_scale
  subtitle_size    <- subtitle_size    * text_scale
  caption_size     <- caption_size     * text_scale
  x_axis_text_size <- x_axis_text_size * text_scale
  y_axis_text_size <- y_axis_text_size * text_scale
  legend_text_size <- legend_text_size * text_scale
  facet_title_size <- facet_title_size * text_scale
  data_label_text_size       <- data_label_text_size       * text_scale
  group_highlight_label_size <- group_highlight_label_size * text_scale
  p <- mget(setdiff(names(formals()), ".rendera"), envir = environment())
  func_d3_linjediagram_bygg(p)
}

func_d3_linjediagram_bygg <- function(p) {

  # --- Input handling ---
  df_work <- if (inherits(p$df, "sf")) sf::st_drop_geometry(p$df) else as.data.frame(p$df)
  x_value <- p$x_value
  y_value <- p$y_value
  group   <- p$group
  facet_var <- p$facet_var

  # Allow vector inputs by injecting them as temporary columns
  if (!is.character(x_value) && length(x_value) > 1) { df_work$.tmp_x <- x_value; x_value <- ".tmp_x" }
  if (!is.character(y_value) && length(y_value) > 1) { df_work$.tmp_y <- y_value; y_value <- ".tmp_y" }

  required_cols <- c(x_value, y_value, group, facet_var, p$line_band_lower, p$line_band_upper)
  missing_cols  <- setdiff(required_cols, names(df_work))
  if (length(missing_cols) > 0) stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  if (!is.numeric(df_work[[y_value]])) stop("func_d3_linjediagram: y_value ('", y_value, "') måste vara numerisk.")
  grid_ok <- function(v) isTRUE(v) || isFALSE(v) || identical(v, "x") || identical(v, "y")
  if (!grid_ok(p$show_major_grid)) stop("show_major_grid must be TRUE, FALSE, \"x\" or \"y\"")
  if (!grid_ok(p$show_minor_grid)) stop("show_minor_grid must be TRUE, FALSE, \"x\" or \"y\"")
  if (!p$legend_position %in% c("none", "top", "bottom", "left", "right"))
    stop("legend_position must be one of: right, left, top, bottom, none")
  if (!is.null(p$custom_theme))
    message("func_d3_linjediagram: custom_theme är ett ggplot2-tema och används inte i D3-versionen.")

  if (nrow(df_work) == 0) {
    warning("func_d3_linjediagram: data frame has 0 rows. Check your filter and factor levels.")
  }

  # --- Type detection ---
  is_date       <- inherits(df_work[[x_value]], c("Date", "POSIXct", "POSIXt"))
  is_discrete_x <- !is.numeric(df_work[[x_value]]) && !is_date
  x_typ         <- if (is_date) "date" else if (is_discrete_x) "discrete" else "number"

  if (!is.null(group) && is.numeric(df_work[[group]])) df_work[[group]] <- factor(df_work[[group]])
  # Färgordning (samma regel som func_graf_linjediagram/barchart/karta): faktornivåer utan oanvända,
  # annars ordning i data; NA får ingen färgplats. Räknas före NA-omvandlingen nedan, som annars
  # gör om en textkolumn till en faktor med alfabetiska nivåer
  farg_ordning <- NULL
  if (!is.null(group)) {
    farg_ordning <- if (is.factor(df_work[[group]])) setdiff(levels(droplevels(df_work[[group]])), NA)
                    else unique(stats::na.omit(as.character(df_work[[group]])))
  }
  # Saknad grupp blir en egen grupp "NA" (grå, sist i legenden) — som ggplot2 visar den
  # as.character: anyNA() ser inte en uttrycklig NA-nivå i en faktor (addNA), bara NA-koder
  na_grupp <- !is.null(group) && anyNA(as.character(df_work[[group]]))
  if (na_grupp) df_work[[group]] <- func_d3_na_som_niva(df_work[[group]])
  # Saknad facett blir en egen panel "NA" sist, som facet_wrap
  if (!is.null(facet_var) && anyNA(as.character(df_work[[facet_var]]))) {
    df_work[[facet_var]] <- if (is.factor(df_work[[facet_var]])) func_d3_na_som_niva(df_work[[facet_var]])
                            else { f <- as.character(df_work[[facet_var]]); f[is.na(f)] <- "NA"
                                   factor(f, levels = c(setdiff(unique(f), "NA"), "NA")) }
  }

  # --- Facet text wrapping (same rules as func_graf_linjediagram) ---
  if (!is.null(facet_var) && !is.null(p$facet_title_wrap) && p$facet_title_wrap > 0) {
    bredd <- p$facet_title_wrap
    df_work[[facet_var]] <- vapply(as.character(df_work[[facet_var]]), function(t) {
      w <- str_wrap(t, bredd)
      if (any(nchar(strsplit(w, "\n")[[1]]) > bredd)) {
        chars  <- strsplit(t, "")[[1]]
        chunks <- vapply(seq(1, length(chars), bredd - 1), function(i) {
          chunk <- paste0(chars[i:min(i + bredd - 2, length(chars))], collapse = "")
          if (i + bredd - 1 <= length(chars)) paste0(chunk, "-") else chunk
        }, character(1))
        paste(chunks, collapse = "\n")
      } else w
    }, character(1), USE.NAMES = FALSE)
  }
  if (!is.null(facet_var) && !is.factor(df_work[[facet_var]]))
    df_work[[facet_var]] <- factor(df_work[[facet_var]], levels = unique(df_work[[facet_var]]))

  # --- Auto x-axis angle ---
  x_angle <- p$x_angle
  if (p$x_angle_auto && x_angle == 0 && length(unique(df_work[[x_value]])) > 8) x_angle <- 45

  # --- Auto date breaks / default numeric breaks ---
  x_date_breaks   <- p$x_date_breaks
  x_axis_n_breaks <- p$x_axis_n_breaks
  if (is.null(x_axis_n_breaks) && nrow(df_work) > 0) {
    if (is_date) {
      date_range <- as.numeric(difftime(max(df_work[[x_value]], na.rm = TRUE),
                                        min(df_work[[x_value]], na.rm = TRUE), units = "days"))
      if (x_date_breaks == "auto") x_date_breaks <- dplyr::case_when(
        date_range > 3650 ~ "2 years", date_range > 1825 ~ "1 year",
        date_range > 730  ~ "6 months", date_range > 365  ~ "3 months",
        TRUE ~ "2 months")
    } else if (!is_discrete_x) {
      x_axis_n_breaks <- 5
    }
  }

  # --- x-kodning: allt blir tal i JavaScript (datum = ms sedan 1970, kategorier = position) ---
  x_levels <- NULL
  if (is_discrete_x) {
    x_levels <- if (is.factor(df_work[[x_value]])) levels(droplevels(df_work[[x_value]]))
                else sort(unique(as.character(df_work[[x_value]])))
  }
  enc_x <- function(v) {
    if (is.null(v)) return(NULL)
    if (x_typ == "date") {
      if (inherits(v, "POSIXct")) as.numeric(v) * 1000
      else if (inherits(df_work[[x_value]], "POSIXct")) {
        # Datum mot POSIXct-data: midnatt i datans tidszon (annars missar t.ex. indexbasen med en timme)
        tz <- attr(df_work[[x_value]], "tzone") %||% ""
        as.numeric(as.POSIXct(format(as.Date(v)), tz = if (length(tz) && nzchar(tz[1])) tz[1] else "")) * 1000
      }
      else as.numeric(as.Date(v)) * 86400000
    } else if (x_typ == "discrete") {
      match(as.character(v), x_levels) - 1
    } else as.numeric(v)
  }
  df_work$.x <- enc_x(df_work[[x_value]])

  # --- X-axis limits and breaks (same logic as func_graf_linjediagram) ---
  x_ticks <- NULL; x_tick_labels <- NULL; x_domain <- NULL
  if (nrow(df_work) > 0) {
    if (!is_discrete_x) {
      x_min_raw <- if (!is.null(p$x_axis_min)) p$x_axis_min else min(df_work[[x_value]], na.rm = TRUE)
      x_max_raw <- if (!is.null(p$x_axis_max)) p$x_axis_max else max(df_work[[x_value]], na.rm = TRUE)
      x_min <- enc_x(x_min_raw); x_max <- enc_x(x_max_raw)
      right_pct <- if (p$x_axis_clip_off) 0.25 else 0.10
      x_domain <- c(x_min - (x_max - x_min) * 0.05, x_max + (x_max - x_min) * right_pct)
      if (x_max == x_min) x_domain <- x_domain + c(-1, 1) * (if (is_date) 86400000 * 180 else 0.5)
      if (is_date) {
        # x_axis_n_breaks gäller även datum (som func_graf_linjediagram; var alltid n = 8)
        brk <- if (x_date_breaks == "auto")
          as.Date(scales::breaks_pretty(n = x_axis_n_breaks %||% 8)(c(as.numeric(as.Date(x_min_raw)), as.numeric(as.Date(x_max_raw)))), origin = "1970-01-01")
        else seq(as.Date(x_min_raw), as.Date(x_max_raw), by = x_date_breaks)
        if (!p$x_axis_clip_off) brk <- brk[brk >= as.Date(x_min_raw) & brk <= as.Date(x_max_raw)]
        x_ticks <- enc_x(brk)
        x_tick_labels <- format(brk, p$x_date_labels)
      } else {
        brk <- scales::breaks_pretty(n = x_axis_n_breaks %||% 5)(c(x_min, x_max))
        # "whole" skriver as.integer(x), så brutna brytpunkter (2022.5) gav dubbla årtal
        if (p$x_number_format == "whole") brk <- brk[brk == round(brk)]
        if (!p$x_axis_clip_off) brk <- brk[brk >= x_min & brk <= x_max]
        x_ticks <- brk
        x_tick_labels <- if (p$x_number_format == "whole") as.character(as.integer(brk))
                         else scales::label_number(big.mark = " ", decimal.mark = ",")(brk)
      }
    } else {
      n_lev   <- length(x_levels)
      n_keep  <- min(x_axis_n_breaks %||% n_lev, n_lev)
      keep_idx <- if (n_keep >= 2) unique(round(seq(1, n_lev, length.out = n_keep))) else 1
      x_ticks <- keep_idx - 1
      x_tick_labels <- x_levels[keep_idx]
      x_domain <- if (n_lev > 1) c(-0.05, 1.05) * (n_lev - 1) else c(-0.5, 0.5)
    }
  }

  # --- Groups and colors (same mapping as func_graf_linjediagram) ---
  grupp_nyckel <- if (!is.null(group)) as.character(df_work[[group]]) else rep("__alla", nrow(df_work))
  facet_nyckel <- if (!is.null(facet_var)) as.character(df_work[[facet_var]]) else rep("__alla", nrow(df_work))
  df_work$.g <- grupp_nyckel
  df_work$.f <- facet_nyckel

  group_levels  <- unique(grupp_nyckel)
  legend_order  <- if (!is.null(group)) levels(factor(df_work[[group]])) else group_levels
  colors        <- p$colors
  if (!is.null(group)) {
    farg_nivaer <- farg_ordning
    if (is.null(colors)) { colors <- viridis::viridis(max(1, length(farg_nivaer)), option = "D") }
    if (!is.null(names(colors))) {
      # Namngivna färger: matchas mot gruppnamnet
      saknas_farg <- setdiff(farg_nivaer, names(colors))
      if (length(saknas_farg) > 0) stop("colors is named but has no color for: ", toString(saknas_farg))
      color_mapping <- colors[farg_nivaer]
    } else {
      # Onamngivna färger: i gruppordning, återanvänds om de är för få
      color_mapping <- stats::setNames(rep(colors, length.out = length(farg_nivaer)), farg_nivaer)
    }
    if (na_grupp) color_mapping[["NA"]] <- "grey50"   # scale_color_manual:s na.value
    for (hg in intersect(p$highlight_groups, names(color_mapping))) color_mapping[[hg]] <- p$highlight_color
  } else {
    color_mapping <- c("__alla" = if (!is.null(colors)) colors[1] else "#2E86AB")
  }

  # gghighlight-metoden: fungerar inte med facetter (samma varning som originalet)
  group_highlight <- p$group_highlight
  if (!is.null(group_highlight) && !is.null(facet_var)) {
    warning("group_highlight is not compatible with facet_var and will be skipped. Use highlight_groups instead.")
    group_highlight <- NULL
  }
  if (!is.null(group_highlight) && is.null(group)) group_highlight <- NULL
  gh_label_colors <- NULL
  if (!is.null(group_highlight) && length(p$group_highlight_label_color) > 1) {
    gh_label_colors <- stats::setNames(rep(p$group_highlight_label_color, length.out = length(group_highlight)), group_highlight)
    for (g in intersect(names(gh_label_colors), names(color_mapping))) color_mapping[[g]] <- gh_label_colors[[g]]
  }

  linje_px <- function(lw) lw * 72.27 / 25.4
  grupper <- lapply(legend_order, function(g) {
    highlighted <- g %in% p$highlight_groups
    faded <- !is.null(group_highlight) && !g %in% group_highlight
    list(key = g,
         label = if (g == "__alla") (p$legend_title %||% y_value) else g,
         color = func_d3_farg(if (faded) p$group_highlight_unhighlighted_color else color_mapping[[g]]),
         base_color = func_d3_farg(color_mapping[[g]]),
         opacity = if (faded) p$group_highlight_unhighlighted_alpha else 1,
         line_size = linje_px(if (highlighted) p$highlight_line_size else p$line_size),
         bold = highlighted && isTRUE(p$highlight_bold),
         faded = faded)
  })

  # --- Serier: en per facett och grupp, sorterade på x ---
  facet_levels <- if (!is.null(facet_var)) levels(df_work[[facet_var]]) else "__alla"
  facet_levels <- facet_levels[facet_levels %in% facet_nyckel]
  # Extra tooltipkolumner (formaterade per kolumn över all data)
  tt_kol <- func_d3_tooltip_kolumner(df_work, p$tooltip_columns, p$tooltip_decimals, p$data_label_decimals)
  tt_namn <- tt_kol$namn
  df_work[tt_namn] <- tt_kol$values
  df_sorted <- df_work[order(df_work$.f, df_work$.g, df_work$.x), , drop = FALSE]
  har_band <- !is.null(p$line_band_lower) && !is.null(p$line_band_upper)
  serier <- list()
  for (f in facet_levels) for (g in group_levels) {
    d <- df_sorted[df_sorted$.f == f & df_sorted$.g == g & !is.na(df_sorted$.x), , drop = FALSE]
    if (nrow(d) == 0) next
    serier[[length(serier) + 1]] <- list(
      facet = f, group = g,
      x = as.list(d$.x),
      y = as.list(d[[y_value]]),
      x_text = as.list(as.character(d[[x_value]])),
      lo = if (har_band) as.list(d[[p$line_band_lower]]) else NULL,
      hi = if (har_band) as.list(d[[p$line_band_upper]]) else NULL)
    # En lista per kolumn, parallell med x (NA -> null, visas inte)
    if (length(tt_namn)) serier[[length(serier)]]$tt <- lapply(tt_namn, function(n) as.list(d[[n]]))
  }

  # --- Dataetiketter: samma urval som func_graf_linjediagram (görs i R, texten skapas i JS) ---
  etiketter <- list()
  if (p$data_labels && nrow(df_work) > 0) {
    df_labels <- df_work   # originalordning, som i ggplot-versionen
    x_filter  <- p$data_label_filter %||% p$data_label_x_values
    grp_vars  <- c(if (!is.null(group)) ".g", if (!is.null(facet_var)) ".f")
    if (p$data_label_first_last) {
      df_labels <- if (length(grp_vars) > 0)
        df_labels %>% group_by(across(all_of(grp_vars))) %>% slice(c(1, n())) %>% ungroup()
      else df_labels %>% slice(c(1, n()))
    }
    if (!is.null(x_filter))              df_labels <- df_labels %>% filter(.data[[x_value]] %in% x_filter)
    if (!is.null(p$data_label_y_values)) df_labels <- df_labels %>% filter(.data[[y_value]] %in% p$data_label_y_values)
    if (!is.null(p$data_label_skip) && p$data_label_skip > 1) {
      df_labels <- if (length(grp_vars) > 0)
        df_labels %>% group_by(across(all_of(grp_vars))) %>% arrange(.x, .by_group = TRUE) %>%
          filter(row_number() %% p$data_label_skip == 1) %>% ungroup()
      else df_labels %>% arrange(.x) %>% filter(row_number() %% p$data_label_skip == 1)
    }
    df_labels <- df_labels %>% group_by(.g, .f, .x) %>% slice(1) %>% ungroup() %>% filter(!is.na(.data[[y_value]]))
    etiketter <- lapply(seq_len(nrow(df_labels)), function(i) {
      list(facet = df_labels$.f[i], group = df_labels$.g[i], x = df_labels$.x[i],
           x_text = as.character(df_labels[[x_value]][i]))
    })
  }

  # --- Tooltip-decimaler ---
  y_varden <- df_work[[y_value]]
  tt_decimaler <- func_d3_tooltip_decimaler(p$tooltip_decimals, c(y_varden, if (har_band) c(df_work[[p$line_band_lower]], df_work[[p$line_band_upper]])),
                                           p$data_label_decimals)

  # --- Referenslinjer, perioder och bildtext i x-koordinater ---
  enc_x_safe <- function(v) if (is.null(v)) list() else as.list(enc_x(v))
  if (length(p$x_shade_from) != length(p$x_shade_to)) stop("x_shade_from och x_shade_to måste vara lika långa.")
  perioder <- if (length(p$x_shade_from) > 0) lapply(seq_along(p$x_shade_from), function(i) {
    list(from = enc_x(p$x_shade_from[i]), to = enc_x(p$x_shade_to[i]),
         label = if (!is.null(p$x_shade_label)) p$x_shade_label[(i - 1) %% length(p$x_shade_label) + 1] else NULL)
  }) else list()

  dash <- switch(p$line_type, solid = NULL, dashed = c(4, 4), dotted = c(1, 3),
                 dashdot = c(1, 3, 4, 3), dotdash = c(1, 3, 4, 3), longdash = c(7, 3), twodash = c(2, 2, 6, 2), NULL)

  n_facets <- length(facet_levels)
  ncol_val <- if (!is.null(facet_var)) (p$facet_ncol %||% max(1L, if (n_facets <= 2) n_facets else if (n_facets <= 4) 2L else 3L)) else 1L

  spec <- list(
    typ = "linjediagram", js_fil = "linjediagram",
    empty = nrow(df_work) == 0,
    width = p$width, height = p$height, font = func_d3_typsnitt(p$font),
    text = list(title = func_d3_rader(p$title, p$title_width),
                subtitle = func_d3_rader(p$subtitle, p$subtitle_width),
                caption = if (!is.null(p$caption_x) && !is.null(p$caption_y)) list() else func_d3_rader(p$caption),
                title_size = func_d3_pt_till_px(p$title_size),
                subtitle_size = func_d3_pt_till_px(p$subtitle_size),
                caption_size = func_d3_pt_till_px(p$caption_size),
                x_axis_title = p$x_axis_title, y_axis_title = p$y_axis_title,
                axis_title_size = func_d3_pt_till_px(11 * p$text_scale),
                x_axis_text_size = func_d3_pt_till_px(p$x_axis_text_size),
                y_axis_text_size = func_d3_pt_till_px(p$y_axis_text_size),
                facet_title_size = func_d3_pt_till_px(p$facet_title_size)),
    free_caption = if (!is.null(p$caption) && !is.null(p$caption_x) && !is.null(p$caption_y))
      list(text = p$caption, x = enc_x(p$caption_x), y = p$caption_y, hjust = p$caption_hjust, vjust = p$caption_vjust) else NULL,
    x = list(type = x_typ, domain = x_domain, ticks = as.list(x_ticks), tick_labels = as.list(x_tick_labels),
             levels = as.list(x_levels), angle = x_angle, date_format = p$x_date_labels,
             number_format = p$x_number_format, n_breaks = x_axis_n_breaks %||% 5, clip_off = p$x_axis_clip_off),
    y = list(min = p$y_axis_min, max = p$y_axis_max),
    facets = list(levels = as.list(facet_levels), ncol = ncol_val, same_y = p$facet_same_y_scale || is.null(facet_var),
                  wrapped = !is.null(facet_var) && any(grepl("\n", facet_levels))),
    groups = grupper,
    has_groups = !is.null(group),
    series = serier,
    line = list(dash = dash, forecast_from = enc_x(p$line_forecast_from), forecast_label = p$line_forecast_label,
                band = har_band, band_alpha = p$line_band_alpha),
    legend = list(position = p$legend_position, title = if (!is.null(group)) (p$legend_title %||% group) else NULL,
                  text_size = func_d3_pt_till_px(p$legend_text_size), title_size = func_d3_pt_till_px(11 * p$text_scale),
                  direct = p$legend_direct),
    text_scale = p$text_scale,   # för D3-egna etiketter (referenslinjer, skuggade perioder, prognos)
    grid = list(major = p$show_major_grid, minor = p$show_minor_grid),   # TRUE/FALSE/"x"/"y"
    axis_line = list(x = p$x_axis_line, y = p$y_axis_line),
    # Etiketter som listor: en per linje (återanvänds om de är färre än linjerna)
    ref = list(x = enc_x_safe(p$x_break_line), x_label = as.list(p$x_break_line_label),
               y = as.list(p$y_break_line), y_label = as.list(p$y_break_line_label)),
    shades = perioder,
    shade_color = func_d3_farg(p$x_shade_color),
    labels = list(show = p$data_labels, points = etiketter,
                  size = func_d3_mm_till_px(p$data_label_text_size),
                  color = if (p$data_label_color == "auto") "auto" else func_d3_farg(p$data_label_color),
                  format = p$data_label_format, decimals = p$data_label_decimals,
                  show_group = p$data_label_show_group && !is.null(group), separator = p$data_label_group_separator,
                  repel = p$data_label_repel, direction = p$data_label_direction,
                  box = p$data_label_box, box_color = func_d3_farg(p$data_label_box_color), box_alpha = p$data_label_box_alpha,
                  nudge_x = p$data_label_nudge_x, nudge_y = p$data_label_nudge_y,
                  glow = p$data_label_glow, glow_color = func_d3_farg(p$data_label_glow_color),
                  glow_width = p$data_label_glow_sigma + p$data_label_glow_expand),
    highlight = list(groups = as.list(group_highlight %||% character(0)),
                     label = p$group_highlight_label && !is.null(group_highlight),
                     label_size = func_d3_mm_till_px(p$group_highlight_label_size),
                     label_box = p$group_highlight_label_box,
                     label_color = if (length(p$group_highlight_label_color) == 1 && p$group_highlight_label_color != "inherit")
                       func_d3_farg(p$group_highlight_label_color) else "inherit"),
    tooltip = list(show = p$tooltip_show, mode = p$tooltip_mode, decimals = tt_decimaler, unit = p$tooltip_unit %||% "",
                   set_decimals = p$tooltip_decimals, columns = tt_kol$labels),
    interactive = list(legend = p$interactive_legend,
                       zoom = p$interactive_zoom && !is_discrete_x,
                       index_toggle = p$interactive_index_toggle,
                       index_base = enc_x(p$interactive_index_base),
                       index_labels = {
                         # Saknade knapptexter fylls i med standardtexterna; onamngiven vektor = ordningen värde, index, förändring
                         std <- c(varde = "Värde", index = "Index", forandring = "Förändring (%)")
                         lab <- p$interactive_index_labels
                         if (is.null(names(lab))) names(lab) <- names(std)[seq_along(lab)]
                         std[intersect(names(lab), names(std))] <- lab[intersect(names(lab), names(std))]
                         as.list(std)
                       },
                       export = p$interactive_export),
    animation = list(draw = p$animation_draw, duration = p$animation_duration)
  )

  if (isTRUE(p$debug)) {
    attr(spec, "debug") <- list(
      data = df_work, x_type = class(df_work[[x_value]]), y_type = class(df_work[[y_value]]),
      is_date = is_date, is_discrete_x = is_discrete_x, x_angle = x_angle,
      x_axis_n_breaks = x_axis_n_breaks, x_date_breaks = if (is_date) x_date_breaks else NULL,
      group_levels = farg_ordning,
      highlighted_groups = p$highlight_groups, spec = spec)
  }
  spec
}
