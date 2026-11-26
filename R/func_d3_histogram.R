#' Interaktivt histogram
#'
#' D3-version av `func_graf_histogram()` i funcVGR, med samma parametrar,
#' standardvärden och ordning: fördelningen av ett numeriskt värde i lika
#' breda intervall, som antal, andel eller täthet, uppdelat per grupp och panel.
#' D3-tilläggen ligger efter originalets parametrar: tooltip med intervall,
#' antal och andel, knappar som växlar mellan antal och andel och mellan
#' staplade, grupperade och överlagrade grupper, klickbar förklaring, en
#' rullista med uppspelning där staplarna växer till nya höjder, och export
#' till PNG och SVG.
#'
#' Intervallen räknas fram på samma sätt som i `func_graf_histogram()`, över
#' alla värden (även alla filtervärden), så att de ligger still när filtret
#' byts. Varje intervall innehåller sin nedre gräns men inte den övre, utom det
#' sista.
#'
#' @inheritParams func_d3_spridningsdiagram
#' @param df Data frame med ett värde per rad (och per filtervärde). Ett
#'   `sf`-objekt går också bra; geometrin används inte. Obligatorisk.
#' @param x_value Namn på den numeriska kolumn vars fördelning ska visas, som
#'   text. Obligatorisk.
#' @param interactive_legend Klicka på en grupp i förklaringen för att dölja
#'   eller visa dess staplar, dubbelklicka för att bara visa den gruppen.
#' @param group Kolumn som delar upp staplarna i färger, t.ex. kön. `NULL` = en
#'   färg.
#' @param legend_position Förklaringens placering: `"right"`, `"left"`,
#'   `"top"`, `"bottom"` eller `"none"`. Anges den inte blir det `"right"` med
#'   `group`, annars `"none"`.
#' @param bin_width Intervallens bredd, t.ex. `5` för femårsklasser. `NULL` =
#'   bestäms av `bins`, eller automatiskt.
#' @param bins Antal intervall, som ger bredden (värdenas spännvidd / `bins`),
#'   när `bin_width` inte anges. Med `bin_boundary` flyttas gränserna, så det
#'   kan bli ett intervall till. `NULL` = automatiskt: en jämn bredd (1, 2, 2,5
#'   eller 5 gånger en tiopotens) nära Freedman–Diaconis förslag.
#' @param bin_boundary Ett värde som en intervallgräns ska ligga på. `NULL` =
#'   `0`, eller lägsta värdet när `bins` anges.
#' @param histogram_y Staplarnas höjd: `"count"` (antal), `"percent"` (andel av
#'   panelen, eller av gruppen när `bar_type` är `"grouped"` eller `"overlay"`)
#'   eller `"density"` (ytan blir 1).
#' @param bar_type Med `group`: `"stacked"`, `"grouped"` (bredvid varandra) eller
#'   `"overlay"` (över varandra med genomskinlighet).
#' @param bar_alpha Staplarnas opacitet (0–1). `NULL` = 1, eller 0,5 med
#'   `bar_type = "overlay"`.
#' @param bar_line_color Färg på kantlinjen runt staplarna, som i
#'   [func_d3_stapeldiagram()]: bättre kontrast för personer med nedsatt syn, och
#'   intervallen skiljs åt. `NULL` = ingen kantlinje.
#' @param bar_line_width Kantlinjens tjocklek (ggplot2:s `linewidth`).
#' @param x_break_line Lodrät, streckad referenslinje: ett eller flera tal, `"mean"` eller
#'   `"median"` av alla värden (per filtervärde). `NULL` = ingen.
#' @param x_break_line_label Text vid referenslinjen, en per linje (återanvänds om de är färre). `NULL` (standard) ger
#'   ingen text.
#' @param data_labels Visa värdet ovanför staplarna (staplade grupper: summan för
#'   intervallet). Tomma intervall får ingen etikett.
#' @param data_label_color Etiketternas färg.
#' @param data_label_decimals Antal decimaler för andel och täthet (antal visas
#'   utan decimaler).
#' @param x_axis_min,x_axis_max Min- och maxvärde för x-axeln. Värden utanför
#'   räknas ändå med i intervallen. `NULL` = automatiskt.
#' @param y_axis_max Maxvärde för y-axeln. `NULL` = automatiskt. y-axeln börjar
#'   alltid på 0.
#' @param tooltip_decimals Antal decimaler för andelen i tooltipen. `NULL` (standard) =
#'   `data_label_decimals`.
#' @param tooltip_unit Enhet som läggs efter intervallet i tooltipen, t.ex. `" år"`.
#' @param interactive_bar_type_toggle Visa knappar som växlar mellan staplade,
#'   grupperade och överlagrade grupper (bara med `group`).
#' @param interactive_y_toggle Visa knappar som växlar staplarnas höjd mellan
#'   antal och andel (och täthet när `histogram_y = "density"`).
#' @param interactive_filter_column Kolumn (t.ex. år) som visas som en rullista
#'   med uppspelningsknapp. Intervallen är desamma för alla värden. `NULL` =
#'   ingen rullista.
#' @param interactive_filter_same_y `TRUE` ger samma y-axel för alla filtervärden,
#'   så att höjderna går att jämföra. `FALSE` anpassar axeln till varje värde.
#' @param animation_grow Låt staplarna växa fram när diagrammet öppnas.
#'
#' @return Ett `vgr_d3`-objekt som ritas när det skrivs ut och sparas med
#'   [func_d3_spara()]. Med `debug = TRUE` en lista med diagnostik (med
#'   intervallen i `bins`).
#' @seealso [func_d3_stapeldiagram()], [func_d3_spridningsdiagram()]
#' @export
#'
#' @example inst/examples/func_d3_histogram.R
func_d3_histogram <- function(
  # === REQUIRED PARAMETERS ===
  df,
  x_value,

  # === GROUPING AND FACETING ===
  group = NULL,
  facet_var = NULL,
  facet_ncol = NULL,
  facet_scales = "fixed",
  facet_same_y_scale = TRUE,
  facet_title_size = 12,
  facet_title_wrap = NULL,

  # === TITLES AND LABELS ===
  title = NULL,
  title_width = 100,
  subtitle = NULL,
  subtitle_width = 100,
  caption = NULL,
  x_axis_title = NULL,
  y_axis_title = NULL,
  legend_title = NULL,

  # === STYLING ===
  colors = func_d3_farger_ru(),
  font = "Verdana",

  # === BINS ===
  bin_width = NULL,
  bins = NULL,
  bin_boundary = NULL,
  histogram_y = "count",

  # === BARS ===
  bar_type = "stacked",
  bar_alpha = NULL,
  bar_line_color = "black",
  bar_line_width = 0.3,

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

  # === REFERENCE LINE ===
  x_break_line = NULL,

  # === DATA LABELS ===
  data_labels = FALSE,
  data_label_text_size = 3.5,
  data_label_color = "black",
  data_label_decimals = 0,

  # === AXIS LIMITS AND FORMAT ===
  x_axis_min = NULL,
  x_axis_max = NULL,
  y_axis_max = NULL,
  x_number_format = "thousands",

  # === ADVANCED ===
  custom_theme = NULL,
  debug = FALSE,

  # ------------------------------------------------------------------
  # D3-tillägg: nya parametrar ligger efter originalets, så positionella
  # anrop träffar samma parametrar som i func_graf-originalet
  # ------------------------------------------------------------------

  # === D3: REFERENCE LINE ===
  x_break_line_label = NULL,

  # === D3: TOOLTIP ===
  tooltip_show = TRUE,
  tooltip_decimals = NULL,
  tooltip_unit = "",

  # === D3: INTERACTIVITY ===
  interactive_legend = TRUE,
  interactive_bar_type_toggle = TRUE,
  interactive_y_toggle = TRUE,
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
  # === D3: GRID LINES ===
  grid_major_color = "#e5e5e5",
  grid_minor_color = "#f2f2f2",
  grid_major_width = 0.5,
  grid_minor_width = 0.25,
  .rendera = FALSE
) {

  if (!.rendera) {
    angivna <- setdiff(names(as.list(match.call()))[-1], ".rendera")
    obj <- func_d3_nytt_objekt("histogram", mget(angivna, envir = environment()))
    if (debug) return(attr(func_d3_spec(obj), "debug"))
    return(obj)
  }

  if (missing(legend_position) && !is.null(group)) legend_position <- "right"

  # facet_scales vinner när den anges; annars ger facet_same_y_scale = FALSE fri y-axel (som func_d3_stapeldiagram)
  if (missing(facet_scales) && !facet_same_y_scale) facet_scales <- "free_y"

  # text_scale: en faktor för alla textstorlekar (som func_graf_linjediagram)
  title_size           <- title_size           * text_scale
  subtitle_size        <- subtitle_size        * text_scale
  caption_size         <- caption_size         * text_scale
  x_axis_text_size     <- x_axis_text_size     * text_scale
  y_axis_text_size     <- y_axis_text_size     * text_scale
  legend_text_size     <- legend_text_size     * text_scale
  facet_title_size     <- facet_title_size     * text_scale
  data_label_text_size <- data_label_text_size * text_scale
  p <- mget(setdiff(names(formals()), ".rendera"), envir = environment())
  func_d3_histogram_bygg(p)
}

func_d3_histogram_bygg <- function(p) {
  df <- if (inherits(p$df, "sf")) sf::st_drop_geometry(p$df) else as.data.frame(p$df)
  x_value <- p$x_value; group <- p$group; facet_var <- p$facet_var; filt <- p$interactive_filter_column

  # --- Kontroller (samma som func_graf_histogram) ---
  missing_cols <- setdiff(c(x_value, group, facet_var, filt), names(df))
  if (length(missing_cols) > 0) stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  if (!is.numeric(df[[x_value]])) stop("x_value must be a numeric column: ", x_value)
  facet_scales <- match.arg(p$facet_scales, c("fixed", "free", "free_x", "free_y"))
  histogram_y  <- match.arg(p$histogram_y, c("count", "percent", "density"))
  bar_type     <- match.arg(p$bar_type, c("stacked", "grouped", "overlay"))
  if (!p$legend_position %in% c("none", "top", "bottom", "left", "right"))
    stop("legend_position must be one of: right, left, top, bottom, none")
  grid_ok <- function(v) isTRUE(v) || isFALSE(v) || identical(v, "x") || identical(v, "y")
  if (!grid_ok(p$show_major_grid)) stop("show_major_grid must be TRUE, FALSE, \"x\" or \"y\"")
  if (!grid_ok(p$show_minor_grid)) stop("show_minor_grid must be TRUE, FALSE, \"x\" or \"y\"")
  if (!is.null(p$bin_width) && !(is.numeric(p$bin_width) && length(p$bin_width) == 1 && p$bin_width > 0))
    stop("bin_width must be one positive number")
  if (!is.null(p$bins) && !(is.numeric(p$bins) && length(p$bins) == 1 && p$bins >= 1))
    stop("bins must be one number of at least 1")
  if (!is.null(p$custom_theme))
    message("func_d3_histogram: custom_theme är ett ggplot2-tema och används inte i D3-versionen.")

  finite <- is.finite(df[[x_value]])
  if (any(!finite)) {
    message(sprintf("func_d3_histogram: %d rows without a value left out", sum(!finite)))
    df <- df[finite, , drop = FALSE]
  }

  # --- Facetter och grupper (samma regler som func_graf_histogram) ---
  if (!is.null(facet_var)) {
    if (!is.null(p$facet_title_wrap) && p$facet_title_wrap > 0)
      df[[facet_var]] <- str_wrap(as.character(df[[facet_var]]), p$facet_title_wrap)
    # Saknad facett blir en egen panel "NA" sist, som facet_wrap (teckenvariabler behåller förekomstordningen)
    if (anyNA(as.character(df[[facet_var]]))) {
      df[[facet_var]] <- if (is.factor(df[[facet_var]])) func_d3_na_som_niva(df[[facet_var]])
                         else { f <- as.character(df[[facet_var]]); f[is.na(f)] <- "NA"
                                factor(f, levels = c(setdiff(unique(f), "NA"), "NA")) }
    }
    if (!is.factor(df[[facet_var]])) df[[facet_var]] <- factor(df[[facet_var]], levels = unique(df[[facet_var]]))
  }
  # Saknad grupp blir en egen grå grupp "NA" sist; as.character: anyNA() ser inte en uttrycklig NA-nivå (addNA)
  if (!is.null(group) && is.numeric(df[[group]])) df[[group]] <- factor(df[[group]])
  group_levels <- character(0)
  farg_map <- NULL
  if (!is.null(group)) {
    na_grupp <- anyNA(as.character(df[[group]]))
    group_levels <- if (is.factor(df[[group]])) setdiff(levels(droplevels(df[[group]])), NA)
                    else unique(stats::na.omit(as.character(df[[group]])))
    colors <- p$colors
    if (is.null(colors)) colors <- viridisLite::viridis(max(length(group_levels), 1), option = "D")
    if (!is.null(names(colors))) {
      saknas <- setdiff(group_levels, names(colors))
      if (length(saknas) > 0) stop("colors is named but has no color for: ", toString(saknas))
      farg_map <- colors[group_levels]
    } else {
      farg_map <- stats::setNames(rep(colors, length.out = length(group_levels)), group_levels)
    }
    if (na_grupp) { group_levels <- c(group_levels, "NA"); farg_map[["NA"]] <- "grey50" }
  }
  bar_color <- if (!is.null(p$colors)) unname(p$colors[1]) else "#2E86AB"

  df$.k <- if (!is.null(filt)) as.character(df[[filt]]) else rep("__alla", nrow(df))
  df$.f <- if (!is.null(facet_var)) as.character(df[[facet_var]]) else rep("__alla", nrow(df))
  df$.g <- if (!is.null(group)) { g <- as.character(df[[group]]); g[is.na(g)] <- "NA"; g } else rep("__alla", nrow(df))

  # ============================================================
  #  INTERVALL (samma beräkning som func_graf_histogram, över alla värden)
  # ============================================================
  edges <- numeric(0); width <- 1
  if (nrow(df) > 0) {
    x <- df[[x_value]]
    x_range <- range(x)
    span    <- diff(x_range)
    width <- if (!is.null(p$bin_width)) p$bin_width
      else if (!is.null(p$bins)) (if (span > 0) span / p$bins else 1)
      else {
        fd <- 2 * stats::IQR(x) / length(x)^(1/3)
        if (!(fd > 0)) fd <- if (span > 0) span / 10 else 1
        fd <- max(fd, span / 50)
        magnitude <- 10^floor(log10(fd))
        magnitude * c(1, 2, 2.5, 5, 10)[which(c(1, 2, 2.5, 5, 10) >= fd / magnitude - 1e-9)[1]]
      }
    boundary <- p$bin_boundary %||% (if (!is.null(p$bins) && is.null(p$bin_width)) x_range[1] else 0)
    first_edge <- boundary + floor((x_range[1] - boundary) / width + 1e-9) * width
    n_bins     <- max(1, ceiling((x_range[2] - first_edge) / width - 1e-9))
    # Intervallet räknas från avståndet till första gränsen i intervallbredder, med en liten tolerans: med
    # decimalbredder är gränserna inte exakta i flyttal, och cut() mot dem lade ett värde på en gräns fel
    df$.bin <- pmin(floor((x - first_edge) / width + 1e-9) + 1, n_bins)
    edges <- round(first_edge + (0:n_bins) * width, 10)
  }
  # Antal per filtervärde, panel, grupp och intervall (bara de som inte är tomma; JS fyller i nollor)
  counts <- if (nrow(df) > 0) {
    d <- df %>% count(.k, .f, .g, .bin, name = ".n")
    lapply(seq_len(nrow(d)), function(i) list(k = d$.k[i], f = d$.f[i], g = d$.g[i], b = d$.bin[i] - 1, n = d$.n[i]))
  } else list()

  filter_values <- if (!is.null(filt)) {
    fv <- df[[filt]]
    as.character(if (is.factor(fv)) levels(droplevels(fv)) else sort(unique(fv)))
  } else "__alla"
  filter_default <- if (!is.null(p$interactive_filter_default) && as.character(p$interactive_filter_default) %in% filter_values)
    as.character(p$interactive_filter_default) else filter_values[length(filter_values)]

  # Referenslinje: ett tal, eller medelvärde/median av alla värden per filtervärde (samma regel som
  # func_d3_spridningsdiagram; filtervärdet motsvarar ett anrop av func_graf_histogram på den delen av data)
  ref_varde <- function(v, kol) {
    if (is.null(v)) return(NULL)
    if (!(identical(v, "mean") || identical(v, "median") || is.numeric(v)))
      stop("x_break_line must be a number, \"mean\" or \"median\"")
    stats::setNames(lapply(filter_values, function(k) {
      d <- df[[kol]][df$.k == k]
      if (!length(d)) return(NULL)
      if (identical(v, "mean")) mean(d) else if (identical(v, "median")) stats::median(d) else v
    }), filter_values)
  }
  ref_x <- ref_varde(p$x_break_line, x_value)

  facet_levels <- if (!is.null(facet_var)) levels(df[[facet_var]]) else "__alla"
  facet_levels <- facet_levels[facet_levels %in% df$.f]
  n_f <- length(facet_levels)
  ncol_val <- if (!is.null(facet_var)) (p$facet_ncol %||% max(1L, if (n_f <= 2) n_f else if (n_f <= 4) 2L else 3L)) else 1L

  spec <- list(
    typ = "histogram", js_fil = "histogram",
    text_scale = p$text_scale,
    empty = nrow(df) == 0,
    width = p$width, height = p$height, font = func_d3_typsnitt(p$font),
    text = list(title = func_d3_rader(p$title, p$title_width),
                subtitle = func_d3_rader(p$subtitle, p$subtitle_width),
                caption = func_d3_rader(p$caption),
                title_size = func_d3_pt_till_px(p$title_size),
                subtitle_size = func_d3_pt_till_px(p$subtitle_size),
                caption_size = func_d3_pt_till_px(p$caption_size),
                x_axis_title = p$x_axis_title, y_axis_title = p$y_axis_title,
                axis_title_size = func_d3_pt_till_px(11 * p$text_scale),
                x_axis_text_size = func_d3_pt_till_px(p$x_axis_text_size),
                y_axis_text_size = func_d3_pt_till_px(p$y_axis_text_size),
                facet_title_size = func_d3_pt_till_px(p$facet_title_size)),
    bins = list(edges = edges, width = width),
    counts = counts,
    groups = lapply(group_levels, function(g) list(key = g, label = g, color = func_d3_farg(farg_map[[g]]))),
    has_groups = !is.null(group),
    bar = list(color = func_d3_farg(bar_color), type = bar_type, y = histogram_y, alpha = p$bar_alpha,
               line_color = if (is.null(p$bar_line_color)) NULL else func_d3_farg(p$bar_line_color),
               line_width = p$bar_line_width * 72.27 / 25.4),
    x = list(min = p$x_axis_min, max = p$x_axis_max, number_format = p$x_number_format),
    y = list(max = p$y_axis_max),
    ref = list(x = ref_x, x_label = if (!is.null(p$x_break_line_label)) as.character(p$x_break_line_label) else NULL),
    facets = list(levels = as.list(facet_levels), ncol = ncol_val, scales = facet_scales,
                  wrapped = !is.null(facet_var) && any(grepl("\n", facet_levels))),
    filter = list(enabled = !is.null(filt), label = filt, values = as.list(filter_values), default = filter_default,
                  same_y = isTRUE(p$interactive_filter_same_y)),
    legend = list(position = p$legend_position, title = p$legend_title %||% group,
                  text_size = func_d3_pt_till_px(p$legend_text_size), title_size = func_d3_pt_till_px(11 * p$text_scale)),
    labels = list(show = p$data_labels, size = func_d3_mm_till_px(p$data_label_text_size),
                  color = func_d3_farg(p$data_label_color), decimals = p$data_label_decimals),
    grid = func_d3_stodlinjer(p),   # major/minor, färg och tjocklek
    axis_line = list(x = p$x_axis_line, y = p$y_axis_line),
    tooltip = list(show = p$tooltip_show, decimals = p$tooltip_decimals %||% p$data_label_decimals,
                   unit = p$tooltip_unit %||% "", x_title = p$x_axis_title %||% x_value),
    interactive = list(legend = p$interactive_legend, export = p$interactive_export,
                       bar_type_toggle = isTRUE(p$interactive_bar_type_toggle) && !is.null(group),
                       y_toggle = isTRUE(p$interactive_y_toggle)),
    animation = list(grow = p$animation_grow, duration = p$animation_duration, play_interval = p$animation_play_interval)
  )

  if (isTRUE(p$debug)) attr(spec, "debug") <- list(data = df, bins = edges, bin_width = width, spec = spec)
  spec
}
