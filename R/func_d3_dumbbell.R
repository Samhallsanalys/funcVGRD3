#' Rita ett interaktivt hanteldiagram
#'
#' Interaktiv D3-version av `funcVGR::func_graf_dumbbell()` med samma parametrar och
#' standardvärden. Ett hanteldiagram (dumbbell) visar värdet för två perioder per
#' kategori, förbundna med en linje. Utöver originalets parametrar finns tooltip med
#' förändring, sorteringsmeny, animation från start- till slutvärde, färg på linjen efter
#' ökning eller minskning (`line_color_by_change`) och export till PNG och SVG.
#'
#' @details
#' Diagrammet skalar med fönstret.
#'
#' Funktionen ritar inte diagrammet direkt. Den returnerar ett objekt som sparar de
#' argument du angett, och diagrammet beräknas först när objektet skrivs ut eller sparas.
#' Därför går det att ändra inställningar i efterhand med `+`, t.ex.
#' `p + d3_labs(title = "Ny rubrik")` (se [d3_plot()]).
#'
#' @section Skillnader mot func_graf_dumbbell():
#' * När panelerna ligger i en kolumn med fria skalor står panelrubrikerna ovanför
#'   panelerna (ggplot2 lägger dem till höger).
#' * `data_label_width` finns kvar men används inte, som i originalet.
#' * Etiketter som flyttas isär använder en egen algoritm i stället för ggrepel, och glöd
#'   ritas som en textkontur.
#' * `custom_theme` (ett ggplot2-tema) används inte; ett meddelande visas.
#'
#' @param df Data frame med data. Obligatorisk.
#' @param x_value Namn på kolumnen med kategorierna, som text, t.ex. `"kommun"`.
#'   Obligatorisk.
#' @param y_value Namn på kolumnen med de numeriska värdena, som text. Obligatorisk.
#' @param group Namn på kolumnen med perioderna, som text, t.ex. `"ar"`. Minst två
#'   värden krävs. Perioderna ordnas i tidsordning: faktorer efter nivåordning, tal och
#'   datum stigande, och text som börjar med en siffra sorteras. Annan text, t.ex.
#'   `"Före"`/`"Efter"`, behåller ordningen i data. Har data fler än två perioder
#'   jämförs den första och den sista (med en varning). Obligatorisk.
#' @param date_column Valfri datumkolumn, som text. Om den anges används bara raderna
#'   med det tidigaste och det senaste datumet. `NULL` (standard) använder alla rader.
#' @param sort_axis_by Sortering av kategorierna: `"original"`, `"alphabetical"`,
#'   `"range"`, `"start_value"`, `"end_value"` eller `"change"`.
#' @param sort_direction Sorteringsordning: `"desc"` (högst först) eller `"asc"` (lägst
#'   först).
#' @param dumbbell_horizontal `TRUE` ger liggande hantlar, `FALSE` stående.
#' @param point_size Ändpunkternas storlek (typiskt 2–8).
#' @param point_shape Ändpunkternas form: 16 = cirkel, 15 = kvadrat, 17 = triangel, 18 =
#'   romb.
#' @param line_size Tjocklek på linjen mellan punkterna (ggplot2:s enhet).
#' @param line_type Linjetyp: `"solid"`, `"dashed"`, `"dotted"` eller `"dotdash"`.
#' @param line_color Färg på linjen mellan punkterna.
#' @param point_alpha Punkternas opacitet (0–1).
#' @param baseline_show Visa en referenslinje vid noll.
#' @param baseline_color Referenslinjens färg.
#' @param baseline_size Referenslinjens tjocklek (typiskt 0,3–1).
#' @param show_direction_arrow Visa pilar för ökning eller minskning.
#' @param change_threshold Minsta absoluta förändring för att en pil ska visas.
#' @param title Huvudrubrik. `NULL` (standard) ger ingen rubrik.
#' @param text_scale Faktor som alla teckenstorlekar multipliceras med (rubriker, axlar,
#'   förklaring, panelrubriker och etiketter). 1 = storlekarna som de är angivna, t.ex.
#'   1.3 för stora exporter.
#' @param title_size Rubrikens teckenstorlek i punkter.
#' @param title_text_size Alias för `title_size` (bakåtkompatibilitet). Vinner om den
#'   anges.
#' @param title_width Antal tecken innan rubriken radbryts.
#' @param subtitle Underrubrik. `NULL` (standard) ger ingen underrubrik.
#' @param subtitle_text_size Underrubrikens teckenstorlek i punkter.
#' @param x_axis_title Rubrik för x-axeln. `NULL` (standard) ger ingen rubrik.
#' @param y_axis_title Rubrik för y-axeln. `NULL` (standard) ger ingen rubrik.
#' @param x_axis_title_angle Rotation av x-axelns rubrik i grader (0 = vågrät, 90 =
#'   lodrät).
#' @param x_axis_text_size Teckenstorlek för x-axelns skaltext i punkter.
#' @param y_axis_text_size Teckenstorlek för y-axelns skaltext i punkter.
#' @param x_axis_clip_off `TRUE` visar alla etiketter, även utanför panelens kant, men
#'   de kan då överlappa axeltext eller en intilliggande panel. `FALSE` klipper vid
#'   panelkanten. Utrymmet på axeln styrs av `data_label_axis_padding`.
#' @param caption Text längst ner, t.ex. källa och noter. `NULL` (standard) ger ingen
#'   text.
#' @param caption_size Teckenstorlek för `caption` i punkter.
#' @param caption_text_size Alias för `caption_size` (bakåtkompatibilitet). Vinner om
#'   den anges.
#' @param colors Två färger, för första och andra perioden.
#' @param color_darkest Mörk färg i ett alternativt färgpar. Används i stället för
#'   `colors` om både `color_darkest` och `color_lightest` anges. `NULL` (standard)
#'   används inte.
#' @param color_lightest Ljus färg i ett alternativt färgpar. Används i stället för
#'   `colors` om både `color_darkest` och `color_lightest` anges. `NULL` (standard)
#'   används inte.
#' @param before_color Färg för första perioden. Går före övriga färgargument. `NULL`
#'   (standard) ger `colors[1]`.
#' @param after_color Färg för andra perioden. Går före övriga färgargument. `NULL`
#'   (standard) ger `colors[2]`.
#' @param highlight_x_values Kategorier som markeras. Måste matcha värdena i `x_value`
#'   exakt, t.ex. `c("Göteborg", "Borås")`. `NULL` (standard) markerar inga.
#' @param highlight_background Färga bakgrunden för markerade kategorier.
#' @param highlight_background_color Bakgrundsfärg för markeringen.
#' @param highlight_background_alpha Markeringens opacitet (0–1).
#' @param highlight_background_full_width `TRUE` låter markeringen gå över hela
#'   diagrammet, `FALSE` bara över dataområdet.
#' @param highlight_bar Visa en färgad sidostapel för markerade kategorier.
#' @param highlight_bar_color Sidostapelns färg. `NULL` (standard) ger samma som
#'   `highlight_background_color`.
#' @param highlight_bar_width Sidostapelns bredd som andel av diagrammet (0,01–0,1).
#' @param highlight_bar_alpha Sidostapelns opacitet (0–1).
#' @param legend_title Förklaringens rubrik. `NULL` (standard) ger namnet på
#'   `group`-kolumnen.
#' @param legend_position Förklaringens placering: `"right"`, `"left"`, `"top"`,
#'   `"bottom"` eller `"none"`.
#' @param legend_text_size Förklaringens teckenstorlek i punkter.
#' @param legend_title_size Teckenstorlek för förklaringens rubrik i punkter.
#' @param data_labels Visa värdeetiketter.
#' @param data_label_position Vilka punkter som får etikett:
#'   `"end"`/`"latest"`/`"right"` (sista perioden), `"start"`/`"first"`/`"left"` (första
#'   perioden) eller `"both"`/`"all"`. Väljer bara punkterna; vilken sida etiketten
#'   hamnar på styrs av `data_label_side`.
#' @param data_label_color Etiketternas textfärg.
#' @param data_label_box Visa en ruta bakom etiketterna.
#' @param data_label_text_size Etiketternas teckenstorlek i mm (ggplot2:s textenhet, 3,5
#'   mm ≈ 10 punkter).
#' @param data_label_nudge_x Vågrät förskjutning av etiketterna. I liggande diagram
#'   anger den bara avståndet (riktningen styrs av `data_label_side`, minst 3 % av
#'   dataområdet); i stående diagram anges den i kategorienheter och gäller bara med
#'   `data_label_repel = TRUE` (utan repel står etiketterna centrerade på punkten).
#' @param data_label_nudge_y Lodrät förskjutning av etiketterna. I liggande diagram
#'   anges den i kategorienheter (1 = en rad, t.ex. 0,3 = strax ovanför punkten); i
#'   stående diagram anger den bara avståndet (riktningen styrs av `data_label_side`).
#' @param data_label_side Vilken sida om punkten etiketten hamnar på: `"value"` lägger
#'   etiketten för den lägre punkten till vänster (liggande) eller under (stående) och
#'   den högre punktens etikett till höger eller över, så att etiketterna inte hamnar på
#'   linjen. `"period"` lägger första periodens etikett till vänster/under och sista
#'   periodens till höger/över.
#' @param data_label_axis_padding Skalar utrymmet som läggs till på värdeaxeln för
#'   etiketterna när `data_label_repel = FALSE`. D3 mäter etiketternas verkliga storlek:
#'   1 = precis tillräckligt, > 1 mer och 0 = av.
#' @param data_label_width Finns kvar för kompatibilitet men används inte (som i
#'   `funcVGR::func_graf_dumbbell()`); siffror radbryts aldrig.
#' @param data_label_angle Etiketternas rotation i grader (0 = vågrät, 90 = lodrät).
#' @param data_label_repel Flytta isär etiketterna så att de inte överlappar.
#'   D3-versionen har en egen algoritm (inte ggrepel).
#' @param data_label_skip Visa var N:e etikett per grupp och panel (2 = varannan).
#'   `NULL` (standard) visar alla.
#' @param data_label_only_first_last Visa etiketter bara för första och sista kategorin.
#' @param data_label_x_values Visa etiketter bara för de här kategorierna, t.ex.
#'   `c("Göteborg", "Borås")`. `NULL` (standard) visar alla.
#' @param data_label_y_values Visa etiketter bara där y har de här värdena. `NULL`
#'   (standard) visar alla.
#' @param data_label_box_color Rutans bakgrundsfärg (när `data_label_box = TRUE`).
#' @param data_label_box_alpha Rutans opacitet (0 = genomskinlig, 1 = helt täckande).
#' @param data_label_glow Lägg en glöd runt etiketterna (ritas som en mjuk textkontur).
#' @param data_label_glow_color Glödens färg (när `data_label_glow = TRUE`).
#' @param data_label_glow_sigma Glödens bredd; högre ger mer spridning (typiskt 1–10).
#' @param data_label_glow_expand Extra bredd som läggs till `data_label_glow_sigma`
#'   (typiskt 1–5).
#' @param facet_var Namn på kolumnen som delar upp diagrammet i paneler, som text.
#'   `NULL` (standard) ger ett diagram.
#' @param facet_same_y_scale `TRUE` ger samma värdeaxel i alla paneler (jämförbara),
#'   `FALSE` en egen skala per panel.
#' @param facet_title_size Panelrubrikernas teckenstorlek i punkter.
#' @param facet_ncol Antal kolumner med paneler. `NULL` (standard) räknas ut
#'   automatiskt.
#' @param show_major_grid Större stödlinjer: `TRUE` = i båda riktningarna, `FALSE` =
#'   inga, `"y"` = bara linjer från y-axeln (vågräta), `"x"` = bara linjer från x-axeln
#'   (lodräta).
#' @param show_minor_grid Mindre stödlinjer, med samma val som `show_major_grid`.
#' @param x_axis_line Rita en linje längs x-axeln.
#' @param y_axis_line Rita en linje längs y-axeln.
#' @param custom_theme Används inte i D3-versionen (ett ggplot2-tema går inte att
#'   använda här, och ett meddelande visas). Finns kvar så att befintliga anrop
#'   fungerar.
#' @param font Typsnitt, t.ex. `"Verdana"` eller `"Arial"`. Måste vara installerat på
#'   datorn, annars används `"sans"` (med ett meddelande).
#' @param debug `TRUE` returnerar en lista med diagnostik (data, klassindelning och den
#'   beräknade specifikationen) i stället för diagrammet.
#' @param line_color_by_change `TRUE` färgar varje linje efter riktningen
#'   (`line_color_increase` respektive `line_color_decrease`) i stället för
#'   `line_color`.
#' @param line_color_increase Linjefärg för ökning. Används bara när
#'   `line_color_by_change = TRUE`.
#' @param line_color_decrease Linjefärg för minskning. Används bara när
#'   `line_color_by_change = TRUE`.
#' @param tooltip_show Visa en tooltip med start, slut och förändring.
#' @param tooltip_decimals Antal decimaler i tooltipens värden. `NULL` (standard) ger 0
#'   om alla värden är heltal, annars 1 (som etiketterna).
#' @param tooltip_unit Enhet som läggs efter tooltipens värden, t.ex. `" %"` eller `"
#'   kr"`.
#' @param tooltip_change_percent Visa även förändringen i procent.
#' @param tooltip_columns Extra kolumner som visas i tooltipen för varje period, som text.
#'   En namngiven vektor ger visningsnamnet, t.ex. `c("Antal" = "antal")`; utan namn
#'   visas kolumnnamnet. En rad om båda perioderna har samma värde, annars t.ex.
#'   "Antal (2023)" och "Antal (2024)". Talkolumner får egna decimaler: heltal utan
#'   decimaler, annars `tooltip_decimals` eller 1. `NULL` (standard) ger inga.
#' @param interactive_sort Visa en sorteringsmeny ovanför diagrammet.
#' @param interactive_legend Klicka på en period i förklaringen för att dölja eller visa
#'   dess punkter, håll muspekaren över den för att framhäva den.
#' @param interactive_export Visa knappar för att ladda ned diagrammet som PNG eller
#'   SVG.
#' @param animation_draw Låt ändpunkterna glida ut från startvärdet när diagrammet
#'   öppnas.
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
#' @example inst/examples/func_d3_dumbbell.R
func_d3_dumbbell <- function(
    # === REQUIRED DATA ===
  df,
  x_value,
  y_value,
  group,

  # === DATA PROCESSING ===
  date_column = NULL,
  sort_axis_by = "end_value",
  sort_direction = "desc",
  dumbbell_horizontal = TRUE,

  # === DUMBBELL STYLING ===
  point_size = 4,
  point_shape = 16,
  line_size = 2,
  line_type = "solid",
  line_color = "#37474F",
  point_alpha = 1,

  # === REFERENCE LINES ===
  baseline_show = FALSE,
  baseline_color = "#37474F",
  baseline_size = 0.5,
  show_direction_arrow = FALSE,
  change_threshold = 0,

  # === TEXT - TITLES ===
  title = NULL,
  text_scale = 1,
  title_size = 16,
  title_text_size = 16,
  title_width = 80,
  subtitle = NULL,
  subtitle_text_size = 13,

  # === TEXT - AXES ===
  x_axis_title = NULL,
  y_axis_title = NULL,
  x_axis_title_angle = 0,
  x_axis_text_size = 11,
  y_axis_text_size = 11,
  x_axis_clip_off = FALSE,

  # === TEXT - CAPTION ===
  caption = NULL,
  caption_size = 10,
  caption_text_size = 10,

  # === COLORS - ENDPOINTS ===
  colors = c("#1a9fb3", "#ffc107"),
  color_darkest = NULL,
  color_lightest = NULL,
  before_color = NULL,
  after_color = NULL,

  # === HIGHLIGHTING - SELECTION ===
  highlight_x_values = NULL,

  # === HIGHLIGHTING - BACKGROUND ===
  highlight_background = TRUE,
  highlight_background_color = "#005b89",
  highlight_background_alpha = 0.15,
  highlight_background_full_width = TRUE,

  # === HIGHLIGHTING - SIDEBAR ===
  highlight_bar = FALSE,
  highlight_bar_color = NULL,
  highlight_bar_width = 0.02,
  highlight_bar_alpha = 0.8,

  # === LEGEND ===
  legend_title = NULL,
  legend_position = "right",
  legend_text_size = 11,
  legend_title_size = 11,

  # === DATA LABELS - DISPLAY ===
  data_labels = TRUE,
  data_label_position = "latest",
  data_label_color = "black",
  data_label_box = TRUE,
  data_label_text_size = 3.5,

  # === DATA LABELS - POSITIONING ===
  data_label_nudge_x = 0.1,
  data_label_nudge_y = 0,
  data_label_side = "value",
  data_label_axis_padding = 1,
  data_label_width = 20,
  data_label_angle = 0,
  data_label_repel = TRUE,

  # === DATA LABELS - FILTERING ===
  data_label_skip = NULL,
  data_label_only_first_last = FALSE,
  data_label_x_values = NULL,
  data_label_y_values = NULL,

  # === DATA LABELS - STYLING ===
  data_label_box_color = "white",
  data_label_box_alpha = 0.8,

  # === DATA LABELS - GLOW EFFECT ===
  data_label_glow = FALSE,
  data_label_glow_color = "white",
  data_label_glow_sigma = 3,
  data_label_glow_expand = 1,

  # === FACETING (SMALL MULTIPLES) ===
  facet_var = NULL,
  facet_same_y_scale = TRUE,
  facet_title_size = 12,
  facet_ncol = NULL,

  # === GRID LINES ===
  show_major_grid = TRUE,
  show_minor_grid = FALSE,
  x_axis_line = TRUE,
  y_axis_line = TRUE,

  # === ADVANCED ===
  custom_theme = NULL,
  font = "Verdana",
  debug = FALSE,

  # ------------------------------------------------------------------
  # D3-tillägg: nya parametrar ligger efter originalets, så positionella
  # anrop träffar samma parametrar som i func_graf-originalet
  # ------------------------------------------------------------------

  # === D3: DUMBBELL STYLING ===
  line_color_by_change = FALSE,
  line_color_increase = "#2e7d32",
  line_color_decrease = "#b2182b",

  # === D3: TOOLTIP ===
  tooltip_show = TRUE,
  tooltip_decimals = NULL,
  tooltip_unit = "",
  tooltip_change_percent = TRUE,
  tooltip_columns = NULL,

  # === D3: INTERACTIVITY ===
  interactive_sort = TRUE,
  interactive_legend = TRUE,
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
    obj <- func_d3_nytt_objekt("dumbbell", mget(angivna, envir = environment()))
    if (debug) return(attr(func_d3_spec(obj), "debug"))
    return(obj)
  }

  # Resolve alternative parameter names (last-one-wins if both are supplied)
  title_size   <- if (!missing(title_text_size))   title_text_size   else title_size
  caption_size <- if (!missing(caption_text_size)) caption_text_size else caption_size
  # text_scale: en faktor för alla textstorlekar (efter aliasen, som func_graf_dumbbell)
  title_size           <- title_size           * text_scale
  caption_size         <- caption_size         * text_scale
  subtitle_text_size   <- subtitle_text_size   * text_scale
  x_axis_text_size     <- x_axis_text_size     * text_scale
  y_axis_text_size     <- y_axis_text_size     * text_scale
  legend_text_size     <- legend_text_size     * text_scale
  legend_title_size    <- legend_title_size    * text_scale
  facet_title_size     <- facet_title_size     * text_scale
  data_label_text_size <- data_label_text_size * text_scale

  p <- mget(setdiff(names(formals()), c(".rendera", "title_text_size", "caption_text_size")), envir = environment())
  func_d3_dumbbell_bygg(p)
}

func_d3_dumbbell_bygg <- function(p) {
  df <- if (inherits(p$df, "sf")) sf::st_drop_geometry(p$df) else as.data.frame(p$df)
  x_value <- p$x_value; y_value <- p$y_value; group <- p$group; facet_var <- p$facet_var

  # --- Validering (samma som func_graf_dumbbell) ---
  if (nrow(df) == 0) stop("'df' must be a non-empty data.frame")
  missing_cols <- setdiff(c(x_value, y_value, group, facet_var), names(df))
  if (length(missing_cols) > 0) stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  if (length(table(df[[group]])) < 2) stop("At least 2 groups required")
  if (!is.numeric(df[[y_value]])) stop(paste("Column", y_value, "must be numeric"))
  if (!p$legend_position %in% c("none", "top", "bottom", "left", "right"))
    stop("legend_position must be one of: right, left, top, bottom, none")
  grid_ok <- function(v) isTRUE(v) || isFALSE(v) || identical(v, "x") || identical(v, "y")
  if (!grid_ok(p$show_major_grid)) stop("show_major_grid must be TRUE, FALSE, \"x\" or \"y\"")
  if (!grid_ok(p$show_minor_grid)) stop("show_minor_grid must be TRUE, FALSE, \"x\" or \"y\"")
  if (!is.null(p$custom_theme))
    message("func_d3_dumbbell: custom_theme är ett ggplot2-tema och används inte i D3-versionen.")
  p$data_label_side <- match.arg(p$data_label_side, c("value", "period"))

  # --- Första/sista datum ---
  if (!is.null(p$date_column) && p$date_column %in% names(df)) {
    if (!inherits(df[[p$date_column]], c("Date", "POSIXct", "POSIXt"))) df[[p$date_column]] <- as.Date(df[[p$date_column]])
    datum <- range(df[[p$date_column]], na.rm = TRUE)
    df <- df[df[[p$date_column]] %in% datum, , drop = FALSE]
  }

  # --- Två perioder, i tidsordning som func_graf_dumbbell ---
  # Faktorer behåller nivåordningen, tal/datum och text som börjar med en siffra ("2014", "2024K1") sorteras,
  # annan text (t.ex. "Före"/"Efter") behåller ordningen i data. Fler än två: den första och den sista
  g <- df[[group]]
  group_values <- if (is.factor(g)) levels(droplevels(g))
    else if (is.numeric(g) || inherits(g, c("Date", "POSIXt"))) as.character(sort(unique(g)))
    else { u <- unique(as.character(g[!is.na(g)])); if (all(grepl("^[0-9]", u))) sort(u) else u }
  group_values <- group_values[!is.na(group_values)]
  if (length(group_values) < 2) stop("Need at least 2 periods")
  if (length(group_values) > 2) {
    group_values <- group_values[c(1, length(group_values))]
    warning(paste("More than two periods - comparing the first and last:",
                  paste(group_values, collapse = " and ")))
  }
  nyckel <- c(x_value, facet_var)
  wide <- df %>%
    mutate(.grupp = as.character(.data[[group]])) %>%
    filter(.grupp %in% group_values) %>%
    group_by(across(all_of(c(nyckel, ".grupp")))) %>%
    summarise(.v = mean(.data[[y_value]]), .groups = "drop") %>%
    pivot_wider(names_from = .grupp, values_from = .v)
  # pivot_wider sorterar på nyckeln — återställ ordningen efter första förekomst i data
  forsta <- df %>% mutate(.o = row_number()) %>% group_by(across(all_of(nyckel))) %>% summarise(.o = min(.o), .groups = "drop")
  wide <- wide %>% left_join(forsta, by = nyckel) %>% arrange(.o)
  wide$start_value <- wide[[group_values[1]]]
  wide$end_value   <- wide[[group_values[2]]]
  wide$change      <- wide$end_value - wide$start_value
  wide$range       <- abs(wide$change)
  wide$.x <- as.character(wide[[x_value]])
  wide$.f <- if (!is.null(facet_var)) as.character(wide[[facet_var]]) else "__alla"

  # --- Extra tooltipkolumner: värdet för start- och slutperioden (första raden per kategori och period) ---
  # Decimalerna avgörs på de två perioderna som visas (en ignorerad tredje period påverkar inte)
  df_tt <- df[!is.na(df[[group]]) & as.character(df[[group]]) %in% group_values, , drop = FALSE]
  tt_kol <- func_d3_tooltip_kolumner(df_tt, p$tooltip_columns, p$tooltip_decimals, 1)
  tt_namn <- tt_kol$namn
  tt_rad <- NULL
  if (length(tt_namn)) {
    df_tt[tt_namn] <- tt_kol$values
    df_tt <- df_tt %>% mutate(.grupp = as.character(.data[[group]])) %>%
      group_by(across(all_of(c(nyckel, ".grupp")))) %>% summarise(across(all_of(tt_namn), func_d3_forsta_varde), .groups = "drop")
    # Värdena för en period i wide:s radordning (en rad per nyckel efter summarise; saknas = NA)
    period_varden <- function(period) {
      wide %>% select(all_of(nyckel)) %>%
        left_join(df_tt %>% filter(.grupp == period) %>% select(-.grupp), by = nyckel)
    }
    s <- period_varden(group_values[1]); e <- period_varden(group_values[2])
    som_json <- function(v) if (is.na(v)) NULL else v
    tt_rad <- lapply(seq_len(nrow(wide)), function(i) {
      lapply(tt_namn, function(n) list(som_json(s[[n]][i]), som_json(e[[n]][i])))   # [start, slut] per kolumn
    })
  }

  highlight_clean <- trimws(as.character(p$highlight_x_values %||% character(0)))
  wide$is_highlighted <- trimws(wide$.x) %in% highlight_clean

  # --- Sortering (samma regler som originalet) ---
  sort_nyckel <- function(data, by, dir) {
    use_desc <- tolower(dir) == "desc"
    v <- switch(by, alphabetical = data$.x, range = data$range, start_value = data$start_value,
                end_value = data$end_value, change = data$change, data$.x)
    ord <- if (is.character(v)) order(v, decreasing = use_desc, method = "radix") else order(if (use_desc) -v else v, na.last = TRUE)
    unique(data$.x[ord])
  }
  kategori_ordning <- if (p$sort_axis_by != "original") sort_nyckel(wide, p$sort_axis_by, p$sort_direction) else unique(wide$.x)

  # --- Färger (samma prioritet som originalet) ---
  farger <- if (!is.null(p$before_color) && !is.null(p$after_color)) c(p$before_color, p$after_color)
            else if (!is.null(p$color_darkest) && !is.null(p$color_lightest)) c(p$color_darkest, p$color_lightest)
            else if (length(p$colors) >= 2) p$colors[1:2]
            else c("#F69541", "#699DC6")

  # --- Dataetiketter: urval i originalets ordning (lång form: start, slut per rad) ---
  etiketter <- list()
  if (p$data_labels) {
    ansats <- tolower(p$data_label_position)
    vilka <- if (ansats %in% c("start", "first", "left")) "start" else if (ansats %in% c("both", "all")) c("start", "end") else "end"
    lang <- bind_rows(lapply(seq_len(nrow(wide)), function(i) {
      data.frame(rad = i, typ = c("start", "end"), value = c(wide$start_value[i], wide$end_value[i]),
                 x = wide$.x[i], stringsAsFactors = FALSE)
    }))
    lang <- lang[lang$typ %in% vilka, , drop = FALSE]
    if (!is.null(p$data_label_skip) && p$data_label_skip > 0) lang <- lang[seq(1, nrow(lang), by = p$data_label_skip), , drop = FALSE]
    if (!is.null(p$data_label_x_values)) lang <- lang[lang$x %in% p$data_label_x_values, , drop = FALSE]
    if (!is.null(p$data_label_y_values)) lang <- lang[lang$value %in% p$data_label_y_values, , drop = FALSE]
    if (p$data_label_only_first_last) lang <- lang[lang$x %in% c(kategori_ordning[1], kategori_ordning[length(kategori_ordning)]), , drop = FALSE]
    lang <- lang[!is.na(lang$value), , drop = FALSE]
    # Sida som i func_graf_dumbbell: -1 = vänster/under, 1 = höger/över. data_label_side = "value" går
    # efter värdet (lägre punkt -1, högre 1); "period", lika värden och saknat värde går efter perioden
    sida <- vapply(seq_len(nrow(lang)), function(i) {
      s <- wide$start_value[lang$rad[i]]; e <- wide$end_value[lang$rad[i]]
      period_sida <- if (lang$typ[i] == "start") -1 else 1
      if (p$data_label_side == "period" || is.na(s) || is.na(e) || s == e) return(period_sida)
      eget <- if (lang$typ[i] == "start") s else e
      if (eget == min(s, e)) -1 else 1
    }, numeric(1))
    etiketter <- lapply(seq_len(nrow(lang)), function(i) list(rad = lang$rad[i] - 1, typ = lang$typ[i], s = sida[i]))
  }

  rader <- lapply(seq_len(nrow(wide)), function(i) {
    r <- list(x = wide$.x[i], f = wide$.f[i], start = wide$start_value[i], end = wide$end_value[i],
         change = wide$change[i], hl = wide$is_highlighted[i], o = i)
    if (!is.null(tt_rad)) r$tt <- tt_rad[[i]]
    r
  })

  facet_levels <- if (!is.null(facet_var)) {
    fv <- df[[facet_var]]
    as.character(if (is.factor(fv)) levels(droplevels(fv)) else unique(fv))
  } else "__alla"
  n_f <- length(facet_levels)
  # facet_wrap utan ncol använder ggplot2:s wrap_dims(), dvs. ncol = n2mfrow(n)[1]
  ncol_val <- min(p$facet_ncol %||% grDevices::n2mfrow(n_f)[1], n_f)

  punkt_px <- function(storlek) 0.375 * (storlek * 72.27 / 25.4 + 0.5 * 96 / 25.4 / 2) * 96 / 72

  spec <- list(
    typ = "dumbbell", js_fil = "dumbbell",
    width = p$width, height = p$height, font = func_d3_typsnitt(p$font),
    horizontal = p$dumbbell_horizontal,
    text = list(title = if (!is.null(p$title) && nchar(p$title) > p$title_width) func_d3_rader(p$title, p$title_width) else func_d3_rader(p$title),
                subtitle = func_d3_rader(p$subtitle), caption = func_d3_rader(p$caption),
                title_size = func_d3_pt_till_px(p$title_size),
                subtitle_size = func_d3_pt_till_px(p$subtitle_text_size),
                caption_size = func_d3_pt_till_px(p$caption_size),
                x_axis_title = p$x_axis_title, y_axis_title = p$y_axis_title,
                axis_title_size = func_d3_pt_till_px(11 * p$text_scale), x_axis_title_angle = p$x_axis_title_angle,
                x_axis_text_size = func_d3_pt_till_px(p$x_axis_text_size),
                y_axis_text_size = func_d3_pt_till_px(p$y_axis_text_size),
                facet_title_size = func_d3_pt_till_px(p$facet_title_size)),
    periods = as.list(group_values),
    colors = as.list(func_d3_farg(farger)),
    rows = rader,
    order = as.list(kategori_ordning),
    sort = list(by = p$sort_axis_by, direction = tolower(p$sort_direction)),
    facets = list(levels = as.list(facet_levels), ncol = ncol_val, same = p$facet_same_y_scale,
                  grid_rows = !is.null(facet_var) && !is.null(p$facet_ncol) && p$facet_ncol == 1 && !p$facet_same_y_scale),
    point = list(r = punkt_px(p$point_size), shape = p$point_shape, alpha = p$point_alpha),
    line = list(size = p$line_size * 72.27 / 25.4, color = func_d3_farg(p$line_color),
                dash = switch(p$line_type, dashed = c(4, 4), dotted = c(1, 3), dotdash = c(1, 3, 4, 3),
                              dashdot = c(1, 3, 4, 3), longdash = c(7, 3), twodash = c(2, 2, 6, 2), NULL),
                by_change = p$line_color_by_change,
                increase = func_d3_farg(p$line_color_increase), decrease = func_d3_farg(p$line_color_decrease)),
    baseline = list(show = p$baseline_show, color = func_d3_farg(p$baseline_color), size = p$baseline_size * 72.27 / 25.4),
    arrows = list(show = p$show_direction_arrow, threshold = p$change_threshold),
    highlight = list(background = p$highlight_background && length(highlight_clean) > 0,
                     color = func_d3_farg(p$highlight_background_color), alpha = p$highlight_background_alpha,
                     full_width = p$highlight_background_full_width,
                     bar = p$highlight_bar && length(highlight_clean) > 0,
                     bar_color = func_d3_farg(p$highlight_bar_color %||% p$highlight_background_color),
                     bar_width = p$highlight_bar_width, bar_alpha = p$highlight_bar_alpha),
    legend = list(position = p$legend_position, title = p$legend_title %||% group,
                  text_size = func_d3_pt_till_px(p$legend_text_size), title_size = func_d3_pt_till_px(p$legend_title_size)),
    labels = list(show = p$data_labels, items = etiketter,
                  size = func_d3_mm_till_px(p$data_label_text_size), color = func_d3_farg(p$data_label_color),
                  box = p$data_label_box, box_color = func_d3_farg(p$data_label_box_color), box_alpha = p$data_label_box_alpha,
                  nudge_x = p$data_label_nudge_x, nudge_y = p$data_label_nudge_y, angle = p$data_label_angle,
                  repel = p$data_label_repel, both = tolower(p$data_label_position) %in% c("both", "all"),
                  axis_padding = p$data_label_axis_padding, clip_off = p$x_axis_clip_off,
                  glow = p$data_label_glow, glow_color = func_d3_farg(p$data_label_glow_color),
                  glow_width = p$data_label_glow_sigma + p$data_label_glow_expand),
    grid = list(major = p$show_major_grid, minor = p$show_minor_grid),   # TRUE/FALSE/"x"/"y"
    axis_line = list(x = p$x_axis_line, y = p$y_axis_line),
    text_scale = p$text_scale,
    tooltip = list(show = p$tooltip_show,
                   decimals = func_d3_tooltip_decimaler(p$tooltip_decimals, c(wide$start_value, wide$end_value), 1),
                   unit = p$tooltip_unit %||% "", set_decimals = p$tooltip_decimals,
                   percent = p$tooltip_change_percent, columns = tt_kol$labels),
    interactive = list(sort = p$interactive_sort, legend = p$interactive_legend, export = p$interactive_export),
    animation = list(draw = p$animation_draw, duration = p$animation_duration)
  )

  if (isTRUE(p$debug)) {
    attr(spec, "debug") <- list(data_wide = wide, group_values = group_values, order = kategori_ordning,
                                colors = farger, spec = spec)
  }
  spec
}
