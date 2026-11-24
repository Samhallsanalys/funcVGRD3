#' Interaktivt lådagram
#'
#' D3-version av `func_graf_ladagram()` i funcVGR, med samma parametrar,
#' standardvärden och ordning: fördelningen av ett numeriskt värde per
#' kategori, med lådor från första till tredje kvartilen, medianstreck,
#' morrhår och avvikande värden. D3-tilläggen ligger efter originalets
#' parametrar: tooltip med antal, kvartiler, morrhår, medelvärde och antal
#' avvikande värden, en sorteringsmeny, klickbar förklaring, en rullista med
#' uppspelning där lådorna glider till sina nya värden, och export till PNG och
#' SVG. `func_d3_boxplot()` är samma funktion under ett engelskt namn.
#'
#' Statistiken räknas fram i R på samma sätt som i `func_graf_ladagram()`
#' (kvartiler med `quantile()`, typ 7), per filtervärde.
#'
#' Parametrarna med `x_` (`x_axis_title`, `x_axis_text_size`, `x_angle`) gäller
#' kategoriaxeln och de med `y_` värdeaxeln, även när lådorna ligger
#' (`horizontal = TRUE`). Undantag: `x_axis_line`, `y_axis_line` och `"x"`/`"y"` i
#' `show_major_grid` och `show_minor_grid` gäller de ritade axlarna (x = den nedre). Med `group` har varje grupp en fast plats i varje
#' kategori, så att samma grupp står på samma ställe även där en annan grupp saknas.
#'
#' @inheritParams func_d3_histogram
#' @param df Data frame med ett värde per rad (och per filtervärde). Ett
#'   `sf`-objekt går också bra; geometrin används inte. Obligatorisk.
#' @param x_value Kolumn med kategorierna, en låda per kategori. `NULL`
#'   (standard) ger en låda för alla värden.
#' @param y_value Namn på den numeriska kolumn vars fördelning ska visas, som
#'   text. Obligatorisk.
#' @param group Kolumn som delar upp lådorna i färger bredvid varandra inom
#'   varje kategori, t.ex. kön. `NULL` = en färg.
#' @param horizontal `TRUE` ger liggande lådor (kategorierna på den lodräta
#'   axeln).
#' @param sort_by Lådornas ordning: `"value_asc"` eller `"value_desc"` (efter
#'   medianen för alla värden), `"name_asc"` eller `"name_desc"`. `NULL` =
#'   kategoriernas ordning.
#' @param box_width Lådornas bredd (0–1 av utrymmet per kategori).
#' @param box_alpha Lådornas opacitet (0–1).
#' @param box_line_color Färg på lådornas kantlinje, medianstreck och morrhår.
#' @param box_line_width Kantlinjens tjocklek (ggplot2:s `linewidth`).
#' @param box_whisker_range Hur långt morrhåren når, i kvartilavstånd från
#'   lådan. 1,5 är Tukeys standard; `Inf` = till minsta och största värdet.
#' @param box_outliers Rita avvikande värden (utanför morrhåren) som punkter.
#' @param box_mean Visa medelvärdet som en romb i lådan.
#' @param point_show Rita alla enskilda värden som punkter ovanpå lådorna,
#'   spridda i sidled.
#' @param point_size Punkternas storlek (ggplot2:s storleksenhet).
#' @param point_alpha Punkternas opacitet (0–1).
#' @param point_jitter Hur mycket punkterna sprids i sidled, som andel av
#'   lådans bredd.
#' @param x_angle Rotation av kategoriaxelns text i grader. Sätts automatiskt
#'   till 45 vid fler än 8 stående kategorier.
#' @param y_break_line Referenslinje på värdeaxeln (lodrät när lådorna ligger): ett eller flera tal, `"mean"` eller
#'   `"median"` av alla värden (per filtervärde). `NULL` = ingen.
#' @param y_break_line_label Text vid referenslinjen, en per linje
#'   (återanvänds om de är färre). `NULL` (standard) ger ingen text.
#' @param data_labels Skriv medianen bredvid varje låda, i höjd med
#'   medianstrecket (ovanför lådan när den ligger ned).
#' @param data_label_decimals Antal decimaler i medianetiketterna.
#' @param y_axis_min,y_axis_max Min- och maxvärde för värdeaxeln. `NULL` =
#'   automatiskt.
#' @param legend_position Förklaringens placering: `"right"`, `"left"`,
#'   `"top"`, `"bottom"` eller `"none"`. Anges den inte blir det `"right"` med
#'   `group`, annars `"none"`.
#' @param tooltip_decimals Antal decimaler för värdena i tooltipen. `NULL`
#'   (standard) ger 0 om alla värden är heltal, annars `data_label_decimals`.
#' @param tooltip_unit Enhet som läggs efter värdena i tooltipen, t.ex.
#'   `" tkr"`.
#' @param interactive_sort Visa en meny ovanför diagrammet som sorterar lådorna
#'   efter medianen eller namnet (för det valda filtervärdet).
#' @param interactive_filter_column Kolumn (t.ex. år) som visas som en rullista
#'   med uppspelningsknapp; lådorna glider till sina nya värden när värdet byts.
#'   `NULL` = ingen rullista.
#' @param interactive_filter_same_y `TRUE` ger samma värdeaxel för alla
#'   filtervärden. `FALSE` anpassar axeln till varje värde.
#' @param interactive_legend Klicka på en grupp i förklaringen för att dölja
#'   eller visa dess lådor, dubbelklicka för att bara visa den gruppen.
#' @param animation_grow Låt lådorna växa fram från medianen när diagrammet
#'   öppnas.
#' @param debug `TRUE` returnerar en lista med data, statistiken per låda
#'   (`boxes`), kategorierna och den beräknade specifikationen i stället för
#'   diagrammet.
#'
#' @return Ett `vgr_d3`-objekt som ritas när det skrivs ut och sparas med
#'   [func_d3_spara()]. Med `debug = TRUE` en lista med diagnostik (med
#'   statistiken per låda i `boxes`). Finns inga värden att rita visar
#'   diagrammet texten "Inga data att visa".
#' @seealso [func_d3_histogram()], [func_d3_stapeldiagram()]
#' @export
#'
#' @example inst/examples/func_d3_ladagram.R
func_d3_ladagram <- function(
  # === REQUIRED PARAMETERS ===
  df,
  x_value = NULL,
  y_value,

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

  # === ARRANGEMENT ===
  horizontal = FALSE,
  sort_by = NULL,

  # === BOXES ===
  box_width = 0.6,
  box_alpha = 0.8,
  box_line_color = "black",
  box_line_width = 0.3,
  box_whisker_range = 1.5,
  box_outliers = TRUE,
  box_mean = FALSE,

  # === POINTS ===
  point_show = FALSE,
  point_size = 1.5,
  point_alpha = 0.4,
  point_jitter = 0.4,

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

  # === REFERENCE LINE ===
  y_break_line = NULL,

  # === DATA LABELS ===
  data_labels = FALSE,
  data_label_text_size = 3.5,
  data_label_color = "black",
  data_label_decimals = 0,

  # === AXIS LIMITS ===
  y_axis_min = NULL,
  y_axis_max = NULL,

  # === ADVANCED ===
  custom_theme = NULL,
  debug = FALSE,

  # ------------------------------------------------------------------
  # D3-tillägg: nya parametrar ligger efter originalets, så positionella
  # anrop träffar samma parametrar som i func_graf-originalet
  # ------------------------------------------------------------------

  # === D3: REFERENCE LINE ===
  y_break_line_label = NULL,

  # === D3: TOOLTIP ===
  tooltip_show = TRUE,
  tooltip_decimals = NULL,
  tooltip_unit = "",

  # === D3: INTERACTIVITY ===
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
  .rendera = FALSE
) {

  if (!.rendera) {
    angivna <- setdiff(names(as.list(match.call()))[-1], ".rendera")
    obj <- func_d3_nytt_objekt("ladagram", mget(angivna, envir = environment()))
    if (debug) return(attr(func_d3_spec(obj), "debug"))
    return(obj)
  }

  if (missing(legend_position) && !is.null(group)) legend_position <- "right"
  # facet_scales vinner när den anges; annars ger facet_same_y_scale = FALSE fri värdeaxel (som func_d3_stapeldiagram)
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
  func_d3_ladagram_bygg(p)
}

#' @rdname func_d3_ladagram
#' @export
func_d3_boxplot <- func_d3_ladagram

func_d3_ladagram_bygg <- function(p) {
  df <- if (inherits(p$df, "sf")) sf::st_drop_geometry(p$df) else as.data.frame(p$df)
  x_value <- p$x_value; y_value <- p$y_value; group <- p$group; facet_var <- p$facet_var
  filt <- p$interactive_filter_column

  # --- Kontroller (samma som func_graf_ladagram) ---
  missing_cols <- setdiff(c(y_value, x_value, group, facet_var, filt), names(df))
  if (length(missing_cols) > 0) stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  if (!is.numeric(df[[y_value]])) stop("y_value must be a numeric column: ", y_value)
  facet_scales <- match.arg(p$facet_scales, c("fixed", "free", "free_x", "free_y"))
  sort_by <- if (!is.null(p$sort_by)) match.arg(p$sort_by, c("value_asc", "value_desc", "name_asc", "name_desc")) else NULL
  if (!(is.numeric(p$box_whisker_range) && length(p$box_whisker_range) == 1 && isTRUE(p$box_whisker_range >= 0)))
    stop("box_whisker_range must be one number of at least 0 (Inf = to min/max)")
  if (!p$legend_position %in% c("none", "top", "bottom", "left", "right"))
    stop("legend_position must be one of: right, left, top, bottom, none")
  grid_ok <- function(v) isTRUE(v) || isFALSE(v) || identical(v, "x") || identical(v, "y")
  if (!grid_ok(p$show_major_grid)) stop("show_major_grid must be TRUE, FALSE, \"x\" or \"y\"")
  if (!grid_ok(p$show_minor_grid)) stop("show_minor_grid must be TRUE, FALSE, \"x\" or \"y\"")
  if (!is.null(p$custom_theme))
    message("func_d3_ladagram: custom_theme är ett ggplot2-tema och används inte i D3-versionen.")

  finite <- is.finite(df[[y_value]])
  if (any(!finite)) {
    message(sprintf("func_d3_ladagram: %d rows without a value left out", sum(!finite)))
    df <- df[finite, , drop = FALSE]
  }

  # --- Kategorier: en låda för alla värden utan x_value; saknad kategori blir en egen låda "NA", sist om inte sort_by sorterar in den bland de andra ---
  x_col <- x_value %||% ".alla"
  if (is.null(x_value)) df$.alla <- rep("", nrow(df))
  cat_levels <- if (is.factor(df[[x_col]])) setdiff(levels(droplevels(df[[x_col]])), NA)
                else unique(stats::na.omit(as.character(df[[x_col]])))
  if (anyNA(as.character(df[[x_col]]))) cat_levels <- c(cat_levels, "NA")
  df$.x <- as.character(df[[x_col]]); df$.x[is.na(df$.x)] <- "NA"

  # --- Facetter och grupper (samma regler som func_graf_ladagram); saknad grupp/facett blir "NA" ---
  if (!is.null(facet_var)) {
    if (!is.null(p$facet_title_wrap) && p$facet_title_wrap > 0)
      df[[facet_var]] <- str_wrap(as.character(df[[facet_var]]), p$facet_title_wrap)
    if (anyNA(as.character(df[[facet_var]]))) {
      df[[facet_var]] <- if (is.factor(df[[facet_var]])) func_d3_na_som_niva(df[[facet_var]])
                         else { f <- as.character(df[[facet_var]]); f[is.na(f)] <- "NA"
                                factor(f, levels = c(setdiff(unique(f), "NA"), "NA")) }
    }
    if (!is.factor(df[[facet_var]])) df[[facet_var]] <- factor(df[[facet_var]], levels = unique(df[[facet_var]]))
  }
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
  box_color <- if (!is.null(p$colors)) unname(p$colors[1]) else "#2E86AB"

  df$.k <- if (!is.null(filt)) as.character(df[[filt]]) else rep("__alla", nrow(df))
  df$.f <- if (!is.null(facet_var)) as.character(df[[facet_var]]) else rep("__alla", nrow(df))
  df$.g <- if (!is.null(group)) { g <- as.character(df[[group]]); g[is.na(g)] <- "NA"; g } else rep("__alla", nrow(df))
  df$.v <- df[[y_value]]

  # Kategoriordning: dataordning, eller efter median/namn med sort_by (medianen för alla värden i kategorin)
  if (!is.null(sort_by) && nrow(df) > 0) {
    cat_med <- tapply(df$.v, df$.x, stats::median)[cat_levels]
    cat_levels <- switch(sort_by,
      value_asc  = cat_levels[order(cat_med)],
      value_desc = cat_levels[order(-cat_med)],
      name_asc   = sort(cat_levels),
      name_desc  = sort(cat_levels, decreasing = TRUE))
  }

  # ============================================================
  #  STATISTIK PER LÅDA (samma som func_graf_ladagram), per filtervärde, panel, kategori och grupp
  # ============================================================
  wr <- p$box_whisker_range
  boxes <- if (nrow(df) > 0) {
    df %>% group_by(.k, .f, .x, .g) %>%
      summarise(n = n(),
                q1 = stats::quantile(.v, 0.25, names = FALSE), median = stats::median(.v),
                q3 = stats::quantile(.v, 0.75, names = FALSE), mean = mean(.v),
                reach = if (is.infinite(wr)) Inf else wr * (q3 - q1),   # Inf * 0 would be NaN
                lower = min(c(q1, .v[.v >= q1 - reach])), upper = max(c(q3, .v[.v <= q3 + reach])),
                vmin_all = min(.v), vmax_all = max(.v),
                outliers = list(.v[.v < lower | .v > upper]),
                values = list(if (isTRUE(p$point_show)) .v else numeric(0)),
                .groups = "drop")
  } else NULL
  # Points sent to the renderer: all values (point_show), otherwise the outliers (box_outliers), otherwise none.
  # vmin/vmax = what the value axis must show for the box (points, whiskers, mean), like the drawn layers in func_graf_ladagram
  visa_punkter <- isTRUE(p$point_show) || isTRUE(p$box_outliers)
  box_list <- if (!is.null(boxes)) Map(function(k, f, x, g, n, q1, md, q3, mn, lo, up, vlo, vhi, out, vals) {
    list(k = k, f = f, x = x, g = g, n = n, q1 = q1, median = md, q3 = q3, mean = mn, lower = lo, upper = up,
         n_out = length(out),
         points = as.list(if (isTRUE(p$point_show)) vals else if (isTRUE(p$box_outliers)) out else numeric(0)),
         vmin = min(if (visa_punkter) vlo else lo, if (isTRUE(p$box_mean)) mn),
         vmax = max(if (visa_punkter) vhi else up, if (isTRUE(p$box_mean)) mn))
  }, boxes$.k, boxes$.f, boxes$.x, boxes$.g, boxes$n, boxes$q1, boxes$median, boxes$q3, boxes$mean,
     boxes$lower, boxes$upper, boxes$vmin_all, boxes$vmax_all, boxes$outliers, boxes$values, USE.NAMES = FALSE) else list()

  # Sort key for the renderer's sort menu: the median of all values per filter value, panel and category
  # (the same rule as sort_by, within what is shown)
  cat_median <- if (nrow(df) > 0) {
    m <- df %>% group_by(.k, .f, .x) %>% summarise(m = stats::median(.v), .groups = "drop")
    lapply(split(m, m$.k), function(d) lapply(split(stats::setNames(d$m, d$.x), d$.f), as.list))
  } else NULL

  filter_values <- if (!is.null(filt)) {
    fv <- df[[filt]]
    as.character(if (is.factor(fv)) levels(droplevels(fv)) else sort(unique(fv)))
  } else "__alla"
  filter_default <- if (!is.null(p$interactive_filter_default) && as.character(p$interactive_filter_default) %in% filter_values)
    as.character(p$interactive_filter_default) else filter_values[length(filter_values)]

  # Referenslinje: ett eller flera tal, eller medelvärde/median av alla värden per filtervärde
  ref_y <- NULL
  if (!is.null(p$y_break_line)) {
    v <- p$y_break_line
    if (!(identical(v, "mean") || identical(v, "median") || is.numeric(v)))
      stop("y_break_line must be one or more numbers, \"mean\" or \"median\"")
    per_k <- split(df$.v, df$.k)
    ref_y <- stats::setNames(lapply(filter_values, function(k) {
      d <- per_k[[k]]
      if (!length(d)) return(NULL)
      if (identical(v, "mean")) mean(d) else if (identical(v, "median")) stats::median(d) else v
    }), filter_values)
  }

  facet_levels <- if (!is.null(facet_var)) levels(df[[facet_var]]) else "__alla"
  facet_levels <- facet_levels[facet_levels %in% df$.f]
  n_f <- length(facet_levels)
  ncol_val <- if (!is.null(facet_var)) (p$facet_ncol %||% max(1L, if (n_f <= 2) n_f else if (n_f <= 4) 2L else 3L)) else 1L

  x_angle <- if (p$x_angle == 0 && length(cat_levels) > 8 && !isTRUE(p$horizontal)) 45 else p$x_angle

  spec <- list(
    typ = "ladagram", js_fil = "ladagram",
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
    categories = as.list(cat_levels), has_categories = !is.null(x_value),
    groups = lapply(group_levels, function(g) list(key = g, label = g, color = func_d3_farg(farg_map[[g]]))),
    has_groups = !is.null(group),
    boxes = box_list, cat_median = cat_median,
    box = list(color = func_d3_farg(box_color), width = p$box_width, alpha = p$box_alpha,
               line_color = func_d3_farg(p$box_line_color), line_width = p$box_line_width * 72.27 / 25.4,
               mean = isTRUE(p$box_mean), outliers = isTRUE(p$box_outliers)),
    points = list(show = isTRUE(p$point_show), r = func_d3_punkt_px(p$point_size), alpha = p$point_alpha, jitter = p$point_jitter),
    horizontal = isTRUE(p$horizontal), x_angle = x_angle,
    y = list(min = p$y_axis_min, max = p$y_axis_max),
    ref = list(y = ref_y, y_label = if (!is.null(p$y_break_line_label)) as.character(p$y_break_line_label) else NULL),
    # Fria axlar per panel som i func_graf_ladagram: y = värdeaxeln och x = kategoriaxeln, även liggande (coord_flip)
    facets = list(levels = as.list(facet_levels), ncol = ncol_val,
                  free_value = facet_scales %in% c("free", "free_y"), free_category = facet_scales %in% c("free", "free_x"),
                  wrapped = !is.null(facet_var) && any(grepl("\n", facet_levels))),
    filter = list(enabled = !is.null(filt), label = filt, values = as.list(filter_values), default = filter_default,
                  same_y = isTRUE(p$interactive_filter_same_y)),
    legend = list(position = p$legend_position, title = p$legend_title %||% group,
                  text_size = func_d3_pt_till_px(p$legend_text_size), title_size = func_d3_pt_till_px(11 * p$text_scale)),
    labels = list(show = p$data_labels, size = func_d3_mm_till_px(p$data_label_text_size),
                  color = func_d3_farg(p$data_label_color), decimals = p$data_label_decimals),
    grid = list(major = p$show_major_grid, minor = p$show_minor_grid),
    axis_line = list(x = p$x_axis_line, y = p$y_axis_line),
    tooltip = list(show = p$tooltip_show, unit = p$tooltip_unit %||% "",
                   decimals = func_d3_tooltip_decimaler(p$tooltip_decimals, df$.v, p$data_label_decimals)),
    # Startordningen är cat_levels (redan sorterad med sort_by, efter medianen för alla värden som i R)
    interactive = list(legend = p$interactive_legend, sort = isTRUE(p$interactive_sort) && !is.null(x_value),
                       export = p$interactive_export),
    animation = list(grow = p$animation_grow, duration = p$animation_duration, play_interval = p$animation_play_interval)
  )

  if (isTRUE(p$debug)) attr(spec, "debug") <- list(data = df, boxes = boxes, categories = cat_levels, spec = spec)
  spec
}
