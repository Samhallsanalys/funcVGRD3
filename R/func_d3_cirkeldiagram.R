#' Rita ett interaktivt cirkel- eller munkdiagram
#'
#' Ritar ett cirkeldiagram, eller ett munkdiagram med `pie_inner_radius`. Det finns ingen
#' ggplot2-motsvarighet i funcVGR. Parametrarna har samma namn som i
#' [func_d3_linjediagram()] där de är meningsfulla: `x_value` är tårtbitens namn och
#' `y_value` dess storlek, och `facet_*`, `title_*`, `colors`, `legend_*`, `data_label_*`,
#' `highlight_groups`, `group_highlight*`, `tooltip_*`, `interactive_*` och `animation_*`
#' fungerar som där. Det som bara finns i cirkeldiagram ligger i familjen `pie_*`.
#'
#' @details
#' Små bitar kan slås ihop till "Övrigt" med `pie_other_threshold`. Med `facet_var` ritas
#' en cirkel per värde, och med `interactive_filter_column` får diagrammet en rullista
#' med uppspelning, t.ex. för år.
#'
#' Funktionen ritar inte diagrammet direkt. Den returnerar ett objekt som sparar de
#' argument du angett, och diagrammet beräknas först när objektet skrivs ut eller sparas.
#' Därför går det att ändra inställningar i efterhand med `+`, t.ex.
#' `p + d3_labs(title = "Ny rubrik")` (se [d3_plot()]).
#'
#' @param df Data frame med en rad per tårtbit (eller per tårtbit och
#'   panel/filtervärde). Obligatorisk.
#' @param x_value Namn på kolumnen med tårtbitarnas namn (kategorierna), som text.
#'   Motsvarar `x_value` i [func_d3_linjediagram()]. Obligatorisk.
#' @param y_value Namn på kolumnen med tårtbitarnas storlek, som text. Värdena ska vara
#'   numeriska och minst 0. Obligatorisk.
#' @param facet_var Namn på kolumnen som ger en cirkel per värde (små multiplar), som
#'   text. `NULL` (standard) ger en cirkel.
#' @param facet_ncol Antal kolumner med paneler. `NULL` (standard) räknas ut
#'   automatiskt.
#' @param facet_title_size Panelrubrikernas teckenstorlek i punkter.
#' @param facet_title_wrap Radbryt panelrubrikerna efter så här många tecken; långa ord
#'   avstavas. `NULL` (standard) ger ingen radbrytning.
#' @param title Huvudrubrik. `NULL` (standard) ger ingen rubrik.
#' @param title_width Antal tecken innan rubriken radbryts.
#' @param subtitle Underrubrik. `NULL` (standard) ger ingen underrubrik.
#' @param subtitle_width Antal tecken innan underrubriken radbryts.
#' @param caption Text längst ner, t.ex. källa och noter. `NULL` (standard) ger ingen
#'   text.
#' @param legend_title Förklaringens rubrik. `NULL` (standard) ger namnet på
#'   `x_value`-kolumnen.
#' @param colors Färger för tårtbitarna. En namnlös vektor fördelas i bitarnas ordning
#'   (faktornivåer utan oanvända nivåer, tal sorterade, annars ordningen i data) och
#'   återanvänds om den är för kort. En namngiven vektor matchas mot bitarnas namn,
#'   t.ex. `c("Industri" = "#1a9fb3")`. En bit behåller sin färg när
#'   `pie_other_threshold` slår ihop andra bitar, och biten "Övrigt" får
#'   `pie_other_color`. Rader där namnet saknas (`NA`) tas bort. Standard är
#'   VGR:s färgtema "ru" (samma färger som `func_fargtema("ru")` i funcVGR); `NULL` ger
#'   viridis-färger.
#' @param font Typsnitt, t.ex. `"Verdana"` eller `"Arial"`. Måste vara installerat på
#'   datorn, annars används `"sans"` (med ett meddelande).
#' @param pie_inner_radius Hål i mitten som andel av radien. 0 ger ett cirkeldiagram,
#'   0,5–0,7 ett munkdiagram.
#' @param pie_sort Bitarnas ordning: `"none"` (faktornivåer, tal sorterade, annars
#'   ordningen i data), `"desc"` (störst först), `"asc"` eller `"name"`.
#' @param pie_start_angle Var första biten börjar, i grader medurs från klockan 12.
#' @param pie_pad_angle Mellanrum mellan bitarna i grader, t.ex. 1.
#' @param pie_corner_radius Rundade hörn på bitarna i pixlar (snyggast på munkdiagram),
#'   t.ex. 4.
#' @param pie_border_color Linjefärg mellan bitarna.
#' @param pie_border_size Linjetjocklek mellan bitarna i pixlar.
#' @param pie_center_text Text i munkens hål. `"total"` ger summan av bitarna, annan
#'   text visas som den är. `NULL` (standard) ger ingen text.
#' @param pie_center_label Liten text under texten i mitten, t.ex. `"sysselsatta"`.
#'   `NULL` (standard) ger ingen.
#' @param pie_other_threshold Bitar med mindre andel än så här (t.ex. 0,03 = 3 %) slås
#'   ihop till en, men bara om minst två bitar är så små. `NULL` (standard) behåller
#'   alla.
#' @param pie_other_label Namnet på den sammanslagna biten.
#' @param pie_other_color Färgen på den sammanslagna biten.
#' @param pie_explode Hur långt markerade bitar dras ut, som andel av radien.
#' @param text_scale Faktor som alla teckenstorlekar multipliceras med (rubriker, axlar,
#'   förklaring, panelrubriker och etiketter). 1 = storlekarna som de är angivna, t.ex.
#'   1.3 för stora exporter.
#' @param title_size Rubrikens teckenstorlek i punkter.
#' @param subtitle_size Underrubrikens teckenstorlek i punkter.
#' @param caption_size Teckenstorlek för `caption` i punkter.
#' @param legend_text_size Förklaringens teckenstorlek i punkter.
#' @param legend_position Förklaringens placering: `"right"`, `"left"`, `"top"`,
#'   `"bottom"` eller `"none"`.
#' @param data_labels Visa etiketter på tårtbitarna.
#' @param data_label_text_size Etiketternas teckenstorlek i mm (ggplot2:s textenhet, 3,5
#'   mm ≈ 10 punkter).
#' @param data_label_color Etiketternas färg. `"auto"` (standard) ger vit eller svart
#'   text i biten efter dess ljushet och mörkgrå text utanför.
#' @param data_label_format Etikettformat: `"percent"` (12 %), `"value_only"` (1 234),
#'   `"value_percent"` (1 234 (12 %)), `"x_y"` (Namn: 1 234) eller `"x_percent"` (Namn:
#'   12 %).
#' @param data_label_position Etikettplacering: `"auto"` (i biten om den är stor nog,
#'   annars utanför med en linje), `"inside"` eller `"outside"`.
#' @param data_label_min_share Minsta andel som får en etikett inne i biten när
#'   `data_label_position = "auto"`.
#' @param data_label_filter Alias för `data_label_x_values` (behålls för
#'   kompatibilitet).
#' @param data_label_x_values Visa etiketter bara för de här tårtbitarna. `NULL`
#'   (standard) visar alla.
#' @param data_label_y_values Visa etiketter bara för tårtbitar med de här värdena.
#'   `NULL` (standard) visar alla.
#' @param data_label_min_value Visa etiketter bara för tårtbitar med minst det här
#'   värdet (i `y_value`-enheter). `NULL` (standard) visar alla.
#' @param data_label_repel Sprid ut etiketterna utanför cirkeln i höjdled så att de inte
#'   överlappar.
#' @param data_label_box Visa en ruta bakom etiketterna.
#' @param data_label_box_color Rutans bakgrundsfärg (när `data_label_box = TRUE`).
#' @param data_label_box_alpha Rutans opacitet (0 = genomskinlig, 1 = helt täckande).
#' @param data_label_decimals Antal decimaler i värden och procentandelar.
#' @param data_label_show_group Skriv tårtbitens namn före värdet i etiketten.
#' @param data_label_group_separator Avskiljare mellan gruppnamn och värde när
#'   `data_label_show_group = TRUE`.
#' @param data_label_glow Lägg en glöd runt etiketterna (ritas som en mjuk textkontur).
#' @param data_label_glow_color Glödens färg (när `data_label_glow = TRUE`).
#' @param data_label_glow_sigma Glödens bredd; högre ger mer spridning (typiskt 1–10).
#' @param data_label_glow_expand Extra bredd som läggs till `data_label_glow_sigma`
#'   (typiskt 1–5).
#' @param highlight_groups Tårtbitar som markeras och dras ut från mitten (se
#'   `pie_explode`). `NULL` (standard) markerar inga.
#' @param highlight_color Färg för markerade tårtbitar. `NULL` (standard) behåller deras
#'   färg.
#' @param highlight_bold Visa markerade tårtbitar med fet text i etiketten och
#'   förklaringen.
#' @param group_highlight Tårtbitar som behåller sin färg medan övriga tonas ned i
#'   grått, t.ex. `c("Vård", "Handel")`. `NULL` (standard) tonar inte ned något.
#' @param group_highlight_unhighlighted_color Färg för de nedtonade tårtbitarna.
#' @param group_highlight_unhighlighted_alpha Opacitet för de nedtonade tårtbitarna
#'   (0–1).
#' @param tooltip_show Visa en tooltip med värde, andel och total.
#' @param tooltip_decimals Antal decimaler i tooltipens värden. `NULL` (standard) ger 0
#'   om alla värden är heltal, annars `data_label_decimals`.
#' @param tooltip_unit Enhet som läggs efter tooltipens värden, t.ex. `" %"` eller `"
#'   kr"`.
#' @param interactive_legend Klicka på en tårtbit i förklaringen för att dölja eller
#'   visa den (andelarna räknas om), håll muspekaren över den för att framhäva den.
#' @param interactive_filter_column Kolumn som visas som en rullista med
#'   uppspelningsknapp, t.ex. `"ar"`; diagrammet visar ett värde i taget. `NULL`
#'   (standard) ger ingen rullista.
#' @param interactive_filter_default Värde som visas först i rullistan. `NULL`
#'   (standard) ger det sista värdet.
#' @param interactive_export Visa knappar för att ladda ned diagrammet som PNG eller
#'   SVG.
#' @param animation_draw Låt tårtbitarna svepa in när diagrammet öppnas.
#' @param animation_duration Animationens längd i millisekunder. 0 = ingen animation.
#' @param animation_play_interval Tid i millisekunder per värde i rullistan när
#'   uppspelningsknappen används.
#' @param width Diagrammets bredd i pixlar. Diagrammen (utom kartorna)
#'   krymper för att få plats i fönstret.
#' @param height Diagrammets höjd i pixlar.
#' @param custom_theme Används inte i D3-versionen (ett ggplot2-tema går inte att
#'   använda här, och ett meddelande visas). Finns kvar så att befintliga anrop
#'   fungerar.
#' @param debug `TRUE` returnerar en lista med diagnostik (data, klassindelning och den
#'   beräknade specifikationen) i stället för diagrammet.
#' @param .rendera Intern. `TRUE` bygger diagrammet och används när objektet skrivs ut.
#'   Ändra inte.
#'
#' @return Ett objekt av klassen `vgr_d3`. Det visas i RStudio:s Viewer när det skrivs ut,
#'   i HTML-dokument från R Markdown och Quarto och på sidan från "Run examples" i
#'   hjälpen. Spara det med [func_d3_spara()].
#'   Med `debug = TRUE` returneras i stället en lista med diagnostik.
#' @seealso [func_d3_stapeldiagram()], [d3_plot()] och [func_d3_spara()].
#' @export
#' @example inst/examples/func_d3_cirkeldiagram.R
func_d3_cirkeldiagram <- function(
    # === REQUIRED PARAMETERS ===
  df,
  x_value,
  y_value,

  # === FACETING ===
  facet_var = NULL,
  facet_ncol = NULL,
  facet_title_size = 12,
  facet_title_wrap = NULL,

  # === TITLES AND LABELS ===
  title = NULL,
  title_width = 100,
  subtitle = NULL,
  subtitle_width = 100,
  caption = NULL,
  legend_title = NULL,

  # === STYLING ===
  colors = func_d3_farger_ru(),
  font = "Verdana",

  # === PIE (D3) ===
  pie_inner_radius = 0,
  pie_sort = "none",
  pie_start_angle = 0,
  pie_pad_angle = 0,
  pie_corner_radius = 0,
  pie_border_color = "white",
  pie_border_size = 1,
  pie_center_text = NULL,
  pie_center_label = NULL,
  pie_other_threshold = NULL,
  pie_other_label = "Övrigt",
  pie_other_color = "grey70",
  pie_explode = 0.08,

  # === TEXT SIZING (in points) ===
  text_scale = 1,
  title_size = 16,
  subtitle_size = 13,
  caption_size = 10,
  legend_text_size = 11,

  # === LAYOUT ===
  legend_position = "right",

  # === DATA LABELS ===
  data_labels = TRUE,
  data_label_text_size = 3.5,
  data_label_color = "auto",
  data_label_format = "percent",
  data_label_position = "auto",
  data_label_min_share = 0.04,
  data_label_filter = NULL,
  data_label_x_values = NULL,
  data_label_y_values = NULL,
  data_label_min_value = NULL,
  data_label_repel = TRUE,
  data_label_box = FALSE,
  data_label_box_color = "white",
  data_label_box_alpha = 0.8,
  data_label_decimals = 0,
  data_label_show_group = FALSE,
  data_label_group_separator = ": ",
  data_label_glow = FALSE,
  data_label_glow_color = "white",
  data_label_glow_sigma = 3,
  data_label_glow_expand = 1,

  # === GROUP HIGHLIGHTING (color override method) ===
  highlight_groups = NULL,
  highlight_color = NULL,
  highlight_bold = TRUE,

  # === GGHIGHLIGHT (fade-out method) ===
  group_highlight = NULL,
  group_highlight_unhighlighted_color = "grey80",
  group_highlight_unhighlighted_alpha = 0.6,

  # === TOOLTIP (D3) ===
  tooltip_show = TRUE,
  tooltip_decimals = NULL,
  tooltip_unit = "",

  # === INTERACTIVITY (D3) ===
  interactive_legend = TRUE,
  interactive_filter_column = NULL,
  interactive_filter_default = NULL,
  interactive_export = TRUE,

  # === ANIMATION (D3) ===
  animation_draw = TRUE,
  animation_duration = 900,
  animation_play_interval = 1500,

  # === SIZE (D3) ===
  width = 1000,
  height = 700,

  # === ADVANCED ===
  custom_theme = NULL,
  debug = FALSE,
  .rendera = FALSE
) {

  if (!.rendera) {
    angivna <- setdiff(names(as.list(match.call()))[-1], ".rendera")
    obj <- func_d3_nytt_objekt("cirkeldiagram", mget(angivna, envir = environment()))
    if (debug) return(attr(func_d3_spec(obj), "debug"))
    return(obj)
  }

  # text_scale: en faktor för alla textstorlekar (som func_graf_linjediagram)
  title_size           <- title_size           * text_scale
  subtitle_size        <- subtitle_size        * text_scale
  caption_size         <- caption_size         * text_scale
  legend_text_size     <- legend_text_size     * text_scale
  facet_title_size     <- facet_title_size     * text_scale
  data_label_text_size <- data_label_text_size * text_scale
  p <- mget(setdiff(names(formals()), ".rendera"), envir = environment())
  func_d3_cirkeldiagram_bygg(p)
}

func_d3_cirkeldiagram_bygg <- function(p) {
  df <- if (inherits(p$df, "sf")) sf::st_drop_geometry(p$df) else as.data.frame(p$df)
  x_value <- p$x_value; y_value <- p$y_value; facet_var <- p$facet_var; filt <- p$interactive_filter_column

  # Allow vector inputs by injecting them as temporary columns (as func_graf_linjediagram)
  if (!is.character(x_value) && length(x_value) > 1) { df$.tmp_x <- x_value; x_value <- ".tmp_x" }
  if (!is.character(y_value) && length(y_value) > 1) { df$.tmp_y <- y_value; y_value <- ".tmp_y" }

  missing_cols <- setdiff(c(x_value, y_value, facet_var, filt), names(df))
  if (length(missing_cols) > 0) stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  if (!is.numeric(df[[y_value]])) stop("func_d3_cirkeldiagram: y_value ('", y_value, "') måste vara numerisk.")
  if (!p$legend_position %in% c("none", "top", "bottom", "left", "right"))
    stop("legend_position must be one of: right, left, top, bottom, none")
  if (!p$pie_sort %in% c("none", "desc", "asc", "name")) stop("pie_sort must be one of: none, desc, asc, name")
  if (!p$data_label_position %in% c("auto", "inside", "outside")) stop("data_label_position must be one of: auto, inside, outside")
  if (!is.null(p$custom_theme))
    message("func_d3_cirkeldiagram: custom_theme är ett ggplot2-tema och används inte i D3-versionen.")
  if (nrow(df) == 0) warning("func_d3_cirkeldiagram: data frame has 0 rows.")

  negativa <- sum(df[[y_value]] < 0, na.rm = TRUE)
  if (negativa > 0) {
    warning("func_d3_cirkeldiagram: ", negativa, " negativa värden kan inte visas i ett cirkeldiagram och tas bort.")
    df <- df[is.na(df[[y_value]]) | df[[y_value]] >= 0, , drop = FALSE]
  }

  # Saknad facett blir en egen tårta "NA" sist (som facet_wrap)
  # as.character: anyNA() ser inte en uttrycklig NA-nivå i en faktor (addNA), bara NA-koder
  if (!is.null(facet_var) && anyNA(as.character(df[[facet_var]]))) {
    df[[facet_var]] <- if (is.factor(df[[facet_var]])) func_d3_na_som_niva(df[[facet_var]])
                       else { f <- as.character(df[[facet_var]]); f[is.na(f)] <- "NA"
                              factor(f, levels = c(setdiff(unique(f), "NA"), "NA")) }
  }

  # --- Facet text wrapping (same rules as func_graf_linjediagram) ---
  if (!is.null(facet_var) && !is.null(p$facet_title_wrap) && p$facet_title_wrap > 0) {
    bredd <- p$facet_title_wrap
    df[[facet_var]] <- vapply(as.character(df[[facet_var]]), function(t) {
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
  if (!is.null(facet_var) && !is.factor(df[[facet_var]]))
    df[[facet_var]] <- factor(df[[facet_var]], levels = unique(df[[facet_var]]))

  # --- Tårtbitar (samma ordningsregel som gruppfärgerna i linje/stapel/karta): faktornivåer utan oanvända,
  #     numeriskt sorterat, annars förekomstordning; saknade namn blir ingen bit. Summerade per filter/facett/namn ---
  if (is.numeric(df[[x_value]])) df[[x_value]] <- factor(df[[x_value]])
  kategorier <- if (is.factor(df[[x_value]])) setdiff(levels(droplevels(df[[x_value]])), NA)
                else unique(stats::na.omit(as.character(df[[x_value]])))
  df$.x <- as.character(df[[x_value]])
  # rep(): en skalär kan inte läggas i en ny kolumn i en data frame med 0 rader
  df$.f <- if (!is.null(facet_var)) as.character(df[[facet_var]]) else rep("__alla", nrow(df))
  df$.k <- if (!is.null(filt)) as.character(df[[filt]]) else rep("__alla", nrow(df))
  df <- df[!is.na(df$.x), , drop = FALSE]
  sum_df <- df %>% group_by(.k, .f, .x) %>% summarise(.v = sum(.data[[y_value]], na.rm = TRUE), .groups = "drop")

  # Färgerna delas ut över alla bitar före sammanslagningen, så att en bit behåller sin färg när
  # pie_other_threshold ändras (samma data ger samma färger)
  kategorier_fore <- kategorier

  # --- Små bitar till "Övrigt" ---
  if (!is.null(p$pie_other_threshold) && p$pie_other_threshold > 0) {
    sum_df <- sum_df %>%
      group_by(.k, .f) %>%
      # En tårta med summan 0 (t.ex. ett år utan värden) har inga andelar — där slås inget ihop
      mutate(.andel = if (sum(.v) > 0) .v / sum(.v) else NA_real_,
             .liten = !is.na(.andel) & .andel < p$pie_other_threshold) %>%
      mutate(.x = if (sum(.liten) >= 2) ifelse(.liten, p$pie_other_label, .x) else .x) %>%
      group_by(.k, .f, .x) %>% summarise(.v = sum(.v), .groups = "drop")
    if (p$pie_other_label %in% sum_df$.x) kategorier <- c(setdiff(kategorier, p$pie_other_label), p$pie_other_label)
    kategorier <- kategorier[kategorier %in% sum_df$.x]
  }

  # --- Färger (samma regel som func_graf_linjediagram m.fl.): onamngivna i bitordning (återanvänds om de
  #     är för få), namngivna matchas mot bitens namn. "Övrigt" får pie_other_color
  #     (highlight_groups/group_highlight nedan kan skriva över den) ---
  colors <- p$colors
  vanliga     <- setdiff(kategorier, p$pie_other_label)        # bitar som visas
  farg_ordning <- setdiff(kategorier_fore, p$pie_other_label)  # färgordning: alla bitar före sammanslagning
  if (is.null(colors)) { colors <- viridis::viridis(max(length(farg_ordning), 1), option = "D") }
  if (!is.null(names(colors))) {
    # Bara bitar som faktiskt visas behöver en namngiven färg
    saknas_farg <- setdiff(vanliga, names(colors))
    if (length(saknas_farg) > 0) stop("colors is named but has no color for: ", toString(saknas_farg))
    farg_map <- colors[vanliga]
  } else {
    farg_map <- stats::setNames(rep(colors, length.out = length(farg_ordning)), farg_ordning)[vanliga]
  }
  if (p$pie_other_label %in% kategorier) farg_map[[p$pie_other_label]] <- p$pie_other_color
  for (hg in intersect(p$highlight_groups, names(farg_map))) if (!is.null(p$highlight_color)) farg_map[[hg]] <- p$highlight_color

  gh <- p$group_highlight
  slices <- lapply(kategorier, function(k) {
    faded <- !is.null(gh) && !k %in% gh
    list(key = k,
         color = func_d3_farg(if (faded) p$group_highlight_unhighlighted_color else farg_map[[k]]),
         opacity = if (faded) p$group_highlight_unhighlighted_alpha else 1,
         highlighted = k %in% p$highlight_groups,
         bold = k %in% p$highlight_groups && isTRUE(p$highlight_bold))
  })

  # --- Etikettfilter ---
  x_filter <- p$data_label_filter %||% p$data_label_x_values
  sum_df$.lab <- TRUE
  if (!is.null(x_filter)) sum_df$.lab <- sum_df$.lab & sum_df$.x %in% x_filter
  if (!is.null(p$data_label_y_values)) sum_df$.lab <- sum_df$.lab & sum_df$.v %in% p$data_label_y_values

  rader <- lapply(seq_len(nrow(sum_df)), function(i) {
    list(k = sum_df$.k[i], f = sum_df$.f[i], x = sum_df$.x[i], v = sum_df$.v[i], lab = sum_df$.lab[i])
  })

  filter_values <- if (!is.null(filt)) {
    fv <- df[[filt]]
    as.character(if (is.factor(fv)) levels(droplevels(fv)) else sort(unique(fv)))
  } else "__alla"
  filter_default <- if (!is.null(p$interactive_filter_default) && as.character(p$interactive_filter_default) %in% filter_values)
    as.character(p$interactive_filter_default) else filter_values[length(filter_values)]

  facet_levels <- if (!is.null(facet_var)) levels(df[[facet_var]]) else "__alla"
  facet_levels <- facet_levels[facet_levels %in% df$.f]
  n_f <- length(facet_levels)
  ncol_val <- if (!is.null(facet_var)) (p$facet_ncol %||% max(1L, if (n_f <= 2) n_f else if (n_f <= 4) 2L else 3L)) else 1L

  varden <- sum_df$.v

  spec <- list(
    typ = "cirkeldiagram", js_fil = "cirkeldiagram",
    text_scale = p$text_scale,   # för "Inga värden"-texten och mitttexten i JS
    empty = nrow(sum_df) == 0,
    width = p$width, height = p$height, font = func_d3_typsnitt(p$font),
    text = list(title = func_d3_rader(p$title, p$title_width),
                subtitle = func_d3_rader(p$subtitle, p$subtitle_width),
                caption = func_d3_rader(p$caption),
                title_size = func_d3_pt_till_px(p$title_size),
                subtitle_size = func_d3_pt_till_px(p$subtitle_size),
                caption_size = func_d3_pt_till_px(p$caption_size),
                facet_title_size = func_d3_pt_till_px(p$facet_title_size)),
    slices = slices,
    rows = rader,
    facets = list(levels = as.list(facet_levels), ncol = ncol_val),
    filter = list(enabled = !is.null(filt), label = filt, values = as.list(filter_values), default = filter_default),
    pie = list(inner = p$pie_inner_radius, sort = p$pie_sort, start = p$pie_start_angle, pad = p$pie_pad_angle,
               corner = p$pie_corner_radius, border_color = func_d3_farg(p$pie_border_color), border_size = p$pie_border_size,
               center_text = p$pie_center_text, center_label = p$pie_center_label, explode = p$pie_explode),
    legend = list(position = p$legend_position, title = p$legend_title %||% x_value,
                  text_size = func_d3_pt_till_px(p$legend_text_size), title_size = func_d3_pt_till_px(11 * p$text_scale)),
    labels = list(show = p$data_labels, size = func_d3_mm_till_px(p$data_label_text_size),
                  color = if (p$data_label_color == "auto") "auto" else func_d3_farg(p$data_label_color),
                  format = p$data_label_format, position = p$data_label_position, min_share = p$data_label_min_share,
                  min_value = p$data_label_min_value, repel = p$data_label_repel,
                  box = p$data_label_box, box_color = func_d3_farg(p$data_label_box_color), box_alpha = p$data_label_box_alpha,
                  decimals = p$data_label_decimals, show_group = p$data_label_show_group,
                  separator = p$data_label_group_separator,
                  glow = p$data_label_glow, glow_color = func_d3_farg(p$data_label_glow_color),
                  glow_width = p$data_label_glow_sigma + p$data_label_glow_expand),
    tooltip = list(show = p$tooltip_show, decimals = func_d3_tooltip_decimaler(p$tooltip_decimals, varden, p$data_label_decimals),
                   unit = p$tooltip_unit %||% "", set_decimals = p$tooltip_decimals),
    interactive = list(legend = p$interactive_legend, export = p$interactive_export),
    animation = list(draw = p$animation_draw, duration = p$animation_duration, play_interval = p$animation_play_interval)
  )

  if (isTRUE(p$debug)) {
    attr(spec, "debug") <- list(data = sum_df, categories = kategorier, colors = farg_map, spec = spec)
  }
  spec
}
