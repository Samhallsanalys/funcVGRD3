#' Interaktivt spridningsdiagram
#'
#' D3-version av `func_graf_spridningsdiagram()` i funcVGR, med samma
#' parametrar, standardvärden och ordning. Ett numeriskt värde på x-axeln mot
#' ett på y-axeln, en punkt per rad, med färg per grupp, storlek efter en tredje
#' kolumn (bubbeldiagram), etiketter, trendlinje och referenslinjer. D3-tilläggen
#' ligger efter originalets parametrar: tooltip med punktens värden och extra
#' kolumner, klickbar förklaring, zoom genom att dra en ruta, sökruta som hittar
#' en punkt, en rullista med uppspelning där punkterna glider mellan t.ex. år,
#' och export till PNG och SVG. `func_d3_scatterplot()` är samma funktion under
#' ett engelskt namn.
#'
#' Trendlinjen (linjär regression med konfidensband) och referenslinjerna vid
#' medelvärde eller median räknas fram i R, på samma sätt som i funcVGR. Med
#' `interactive_filter_column` räknas de per filtervärde, medan axlarna är
#' desamma för alla filtervärden så att rörelsen syns.
#'
#' @inheritParams func_d3_linjediagram
#' @param df Data frame med en rad per punkt (och per filtervärde). Ett
#'   `sf`-objekt går också bra; geometrin används inte. Obligatorisk.
#' @param x_value Namn på den numeriska kolumnen för x-axeln, som text.
#'   Obligatorisk.
#' @param y_value Namn på den numeriska kolumnen för y-axeln, som text.
#'   Obligatorisk.
#' @param group Kolumn som punkterna färgas efter. `NULL` (standard) ger en
#'   färg för alla.
#' @param facet_same_y_scale Samma y-skala i alla paneler. Används bara när
#'   `facet_scales` inte anges; `FALSE` ger fri y-axel.
#' @param facet_scales Panelernas axelskalor: `"fixed"` (samma skalor),
#'   `"free"`, `"free_x"` eller `"free_y"`.
#' @param colors Färger för grupperna: en namnlös vektor fördelas i
#'   gruppordning, en namngiven matchas mot gruppnamnen. Utan `group` används
#'   den första färgen. `NULL` ger viridis-färger för grupperna (utan `group`:
#'   blått, `"#2E86AB"`).
#' @param legend_position Förklaringens placering: `"right"`, `"left"`,
#'   `"top"`, `"bottom"` eller `"none"`. Anges den inte blir det `"right"` med
#'   `group` eller `point_size_column`, annars `"none"`.
#' @param point_size Punkternas storlek (ggplot2:s storleksenhet). Används när
#'   `point_size_column` inte anges.
#' @param point_alpha Punkternas opacitet (0–1).
#' @param point_size_column Numerisk kolumn som styr punkternas storlek
#'   (bubbeldiagram). Punktens yta växer med värdet. `NULL` = lika stora.
#' @param point_size_range Minsta och största punktstorlek med
#'   `point_size_column`, som en vektor av längd 2.
#' @param point_size_title Rubrik för storleksförklaringen. `NULL` =
#'   kolumnnamnet.
#' @param point_border_color Färg på en kantlinje runt punkterna, t.ex.
#'   `"white"`. `NULL` = ingen kantlinje.
#' @param show_major_grid,show_minor_grid Större respektive mindre stödlinjer:
#'   `TRUE`, `FALSE`, `"x"` (lodräta) eller `"y"` (vågräta).
#' @param trend_line Trendlinje (linjär regression): `FALSE` = ingen, `TRUE` =
#'   en linje för alla punkter (per panel), `"group"` = en per grupp. En linje
#'   ritas bara där det finns minst tre punkter med olika x-värden.
#' @param trend_line_se Visa ett 95-procentigt konfidensband runt
#'   trendlinjen.
#' @param trend_line_color Trendlinjens färg när `trend_line = TRUE`.
#' @param trend_line_size Trendlinjens tjocklek (ggplot2:s `linewidth`).
#' @param x_break_line Lodrät, streckad referenslinje: ett eller flera tal, `"mean"` eller
#'   `"median"` av x för alla punkter (per filtervärde). `NULL` = ingen.
#' @param y_break_line Vågrät referenslinje: ett eller flera tal, `"mean"` eller `"median"`
#'   av y. `NULL` = ingen.
#' @param data_labels Visa etiketter vid punkterna.
#' @param data_label_column Kolumn med etiketternas text, t.ex. kommunnamn. Den
#'   används också som tooltipens rubrik, för sökrutan och för att följa en
#'   punkt mellan filtervärden. `NULL` = y-värdet, se `data_label_format`.
#' @param data_label_filter Visa etiketter bara för punkter vars värde i
#'   `data_label_column` finns i vektorn. `NULL` = alla.
#' @param data_label_format När `data_label_column` saknas: `"value_only"`
#'   (y-värdet) eller `"x_y"` (`"x; y"`).
#' @param data_label_decimals Antal decimaler för tal i etiketterna.
#' @param data_label_color Etiketternas färg, eller `"auto"` för punktens färg.
#' @param data_label_repel Flytta isär etiketter som krockar med varandra eller
#'   med punkterna.
#' @param data_label_max_overlaps Största antal överlapp innan en etikett döljs.
#'   `Inf` visar alla.
#' @param data_label_box Visa en ruta bakom etiketterna.
#' @param x_axis_min,x_axis_max,y_axis_min,y_axis_max Min- och maxvärde för
#'   axlarna. `NULL` = automatiskt.
#' @param x_number_format Format för x-axelns tal: `"thousands"` eller
#'   `"whole"` (utan tusentalsavgränsare, t.ex. årtal).
#' @param x_break_line_label,y_break_line_label Text vid referenslinjerna, en per linje (återanvänds om de är färre).
#'   `NULL` (standard) ger ingen text.
#' @param tooltip_decimals Antal decimaler för x, y och storlek i tooltipen.
#'   `NULL` (standard) ger 0 för heltalskolumner, annars `data_label_decimals`
#'   (minst 1).
#' @param tooltip_unit Enhet som läggs efter y-värdet i tooltipen.
#' @param tooltip_columns Extra kolumner som visas i tooltipen, t.ex.
#'   `c("Invånare" = "invanare")`; utan namn visas kolumnnamnet. Talkolumner får
#'   egna decimaler: heltal utan decimaler, annars `tooltip_decimals` eller
#'   `data_label_decimals`. `NULL` (standard) ger inga.
#' @param interactive_legend Klicka på en grupp i förklaringen för att dölja
#'   eller visa dess punkter, dubbelklicka för att bara visa den gruppen, håll
#'   muspekaren över den för att framhäva den.
#' @param interactive_zoom Dra en ruta i diagrammet för att zooma in,
#'   dubbelklicka eller tryck på Återställ zoom för att gå tillbaka.
#' @param interactive_search Visa en sökruta som hittar och markerar punkter
#'   efter namnet i `data_label_column` (kräver den kolumnen).
#' @param interactive_filter_column Kolumn (t.ex. år) som visas som en rullista
#'   med uppspelningsknapp, med en rad per punkt och filtervärde. Med
#'   `data_label_column` glider varje punkt till sitt nya läge när värdet byts
#'   (punkterna kopplas ihop med namnet); utan den växer punkterna fram på nytt.
#'   `NULL` = ingen rullista.
#' @param interactive_filter_default Filtervärdet som visas först. `NULL` = det
#'   sista.
#' @param interactive_export Visa knappar för att ladda ned diagrammet som PNG
#'   och SVG.
#' @param animation_grow Låt punkterna växa fram när diagrammet öppnas.
#' @param animation_duration Animationens längd i millisekunder. 0 = ingen
#'   animation.
#' @param animation_play_interval Tid per filtervärde i millisekunder när
#'   uppspelningen körs.
#' @param width,height Diagrammets storlek i pixlar. Diagrammet skalas ned när
#'   det inte får plats.
#'
#' @return Ett `vgr_d3`-objekt som ritas när det skrivs ut (i RStudio:s Viewer,
#'   R Markdown eller Quarto) och sparas med [func_d3_spara()]. Med
#'   `debug = TRUE` en lista med diagnostik.
#' @seealso [func_d3_linjediagram()], [func_d3_histogram()]
#' @export
#'
#' @example inst/examples/func_d3_spridningsdiagram.R
func_d3_spridningsdiagram <- function(
  # === REQUIRED PARAMETERS ===
  df,
  x_value,
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

  # === POINTS ===
  point_size = 2.5,
  point_alpha = 0.8,
  point_size_column = NULL,
  point_size_range = c(1.5, 10),
  point_size_title = NULL,
  point_border_color = NULL,

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

  # === TREND AND REFERENCE LINES ===
  trend_line = FALSE,
  trend_line_se = FALSE,
  trend_line_color = "grey30",
  trend_line_size = 0.8,
  x_break_line = NULL,
  y_break_line = NULL,

  # === DATA LABELS ===
  data_labels = FALSE,
  data_label_column = NULL,
  data_label_filter = NULL,
  data_label_format = "value_only",
  data_label_decimals = 0,
  data_label_text_size = 3.5,
  data_label_color = "black",
  data_label_repel = TRUE,
  data_label_max_overlaps = 10,
  data_label_box = FALSE,
  data_label_box_color = "white",
  data_label_box_alpha = 0.8,

  # === AXIS LIMITS AND FORMAT ===
  x_axis_min = NULL,
  x_axis_max = NULL,
  y_axis_min = NULL,
  y_axis_max = NULL,
  x_number_format = "thousands",

  # === ADVANCED ===
  custom_theme = NULL,
  debug = FALSE,

  # ------------------------------------------------------------------
  # D3-tillägg: nya parametrar ligger efter originalets, så positionella
  # anrop träffar samma parametrar som i func_graf-originalet
  # ------------------------------------------------------------------

  # === D3: REFERENCE LINES ===
  x_break_line_label = NULL,
  y_break_line_label = NULL,

  # === D3: TOOLTIP ===
  tooltip_show = TRUE,
  tooltip_decimals = NULL,
  tooltip_unit = "",
  tooltip_columns = NULL,

  # === D3: INTERACTIVITY ===
  interactive_legend = TRUE,
  interactive_zoom = TRUE,
  interactive_search = TRUE,
  interactive_filter_column = NULL,
  interactive_filter_default = NULL,
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
    obj <- func_d3_nytt_objekt("spridningsdiagram", mget(angivna, envir = environment()))
    if (debug) return(attr(func_d3_spec(obj), "debug"))
    return(obj)
  }

  # A legend is only useful when something is mapped to colour or size (as func_graf_spridningsdiagram)
  if (missing(legend_position) && (!is.null(group) || !is.null(point_size_column))) legend_position <- "right"

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
  func_d3_spridningsdiagram_bygg(p)
}

#' @rdname func_d3_spridningsdiagram
#' @export
func_d3_scatterplot <- func_d3_spridningsdiagram

func_d3_spridningsdiagram_bygg <- function(p) {
  df <- if (inherits(p$df, "sf")) sf::st_drop_geometry(p$df) else as.data.frame(p$df)
  x_value <- p$x_value; y_value <- p$y_value; group <- p$group; facet_var <- p$facet_var
  size_col <- p$point_size_column; lab_col <- p$data_label_column; filt <- p$interactive_filter_column

  # --- Kontroller (samma som func_graf_spridningsdiagram) ---
  missing_cols <- setdiff(c(x_value, y_value, group, facet_var, size_col, lab_col, filt, unname(p$tooltip_columns)), names(df))
  if (length(missing_cols) > 0) stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  if (!is.numeric(df[[x_value]])) stop("x_value must be a numeric column: ", x_value)
  if (!is.numeric(df[[y_value]])) stop("y_value must be a numeric column: ", y_value)
  if (!is.null(size_col) && !is.numeric(df[[size_col]])) stop("point_size_column must be a numeric column: ", size_col)
  facet_scales <- match.arg(p$facet_scales, c("fixed", "free", "free_x", "free_y"))
  data_label_format <- match.arg(p$data_label_format, c("value_only", "x_y", "both"))   # "both" = "x_y"
  if (!p$legend_position %in% c("none", "top", "bottom", "left", "right"))
    stop("legend_position must be one of: right, left, top, bottom, none")
  grid_ok <- function(v) isTRUE(v) || isFALSE(v) || identical(v, "x") || identical(v, "y")
  if (!grid_ok(p$show_major_grid)) stop("show_major_grid must be TRUE, FALSE, \"x\" or \"y\"")
  if (!grid_ok(p$show_minor_grid)) stop("show_minor_grid must be TRUE, FALSE, \"x\" or \"y\"")
  trend_line <- p$trend_line
  if (!(isTRUE(trend_line) || isFALSE(trend_line) || identical(trend_line, "group")))
    stop("trend_line must be TRUE, FALSE or \"group\"")
  if (identical(trend_line, "group") && is.null(group)) {
    warning("trend_line = \"group\" needs group - drawing one trend line for all points instead")
    trend_line <- TRUE
  }
  if (!is.null(p$data_label_filter) && is.null(lab_col))
    stop("data_label_filter needs data_label_column (the values are matched against it)")
  if (!is.null(p$custom_theme))
    message("func_d3_spridningsdiagram: custom_theme är ett ggplot2-tema och används inte i D3-versionen.")

  # Punkter utan x- eller y-värde (eller storlek) kan inte ritas
  drawable <- is.finite(df[[x_value]]) & is.finite(df[[y_value]]) &
              (if (!is.null(size_col)) is.finite(df[[size_col]]) else TRUE)
  if (any(!drawable)) {
    message(sprintf("func_d3_spridningsdiagram: %d rows without x, y%s value left out", sum(!drawable),
                    if (!is.null(size_col)) " or size" else ""))
    df <- df[drawable, , drop = FALSE]
  }

  # --- Facetter: radbrytning och ordning som func_graf_spridningsdiagram ---
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

  # --- Grupper och färger (samma regel som func_graf_spridningsdiagram) ---
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
  point_color <- if (!is.null(p$colors)) unname(p$colors[1]) else "#2E86AB"

  # Arbetskolumner: filter (.k), facett (.f), grupp (.g), punktens nyckel (.key) för övergångar mellan filtervärden
  df$.k <- if (!is.null(filt)) as.character(df[[filt]]) else rep("__alla", nrow(df))
  df$.f <- if (!is.null(facet_var)) as.character(df[[facet_var]]) else rep("__alla", nrow(df))
  df$.g <- if (!is.null(group)) { g <- as.character(df[[group]]); g[is.na(g)] <- "NA"; g } else rep("__alla", nrow(df))
  # Utan etikettkolumn finns inget som identifierar en punkt mellan filtervärden: radordningen (punkterna glider då
  # inte, se spridningsdiagram.js)
  df$.key <- if (!is.null(lab_col)) paste(df$.f, as.character(df[[lab_col]]), sep = "\r")
             else paste(df$.f, stats::ave(seq_len(nrow(df)), df$.k, df$.f, FUN = seq_along), sep = "\r")

  # --- Punktstorlek (radie i pixlar med func_d3_punkt_px()) ---
  size_legend <- NULL
  if (!is.null(size_col)) {
    # Samma skala som scale_size(range) i ggplot2, över alla filtervärden: ytan växer med värdet, från minsta
    # till största storleken i point_size_range
    sv <- df[[size_col]]; lim <- range(sv)
    storlek <- function(v) func_d3_punkt_px(scales::area_pal(p$point_size_range)(scales::rescale(v, from = lim)))
    df$.r <- storlek(sv)
    brk <- scales::breaks_extended()(lim)
    brk <- brk[!is.na(brk) & brk >= lim[1] & brk <= lim[2]]
    size_legend <- list(title = p$point_size_title %||% size_col,
                        breaks = lapply(brk, function(b) list(label = func_d3_format_tal(b, 0), r = storlek(b))))
  } else {
    df$.r <- rep(func_d3_punkt_px(p$point_size), nrow(df))
  }

  # --- Etiketter (samma text som func_graf_spridningsdiagram) ---
  fmt <- function(v) func_d3_format_tal(v, p$data_label_decimals)
  df$.lab <- if (!is.null(lab_col)) as.character(df[[lab_col]])
    else if (data_label_format %in% c("x_y", "both")) paste0(fmt(df[[x_value]]), "; ", fmt(df[[y_value]]))
    else fmt(df[[y_value]])
  df$.lab[!is.na(df$.lab) & df$.lab == ""] <- NA
  df$.show_lab <- isTRUE(p$data_labels) & !is.na(df$.lab) &
    (if (!is.null(p$data_label_filter)) as.character(df[[lab_col]]) %in% p$data_label_filter else TRUE)

  # --- Extra tooltipkolumner ---
  tt_kol <- func_d3_tooltip_kolumner(df, p$tooltip_columns, p$tooltip_decimals, p$data_label_decimals)
  df[tt_kol$namn] <- tt_kol$values

  # --- Referenslinjer: ett tal, eller medelvärde/median av punkterna per filtervärde ---
  filter_values <- if (!is.null(filt)) {
    fv <- df[[filt]]
    as.character(if (is.factor(fv)) levels(droplevels(fv)) else sort(unique(fv)))
  } else "__alla"
  ref_varde <- function(v, kol) {
    if (is.null(v)) return(NULL)
    if (!(identical(v, "mean") || identical(v, "median") || is.numeric(v)))
      stop("x_break_line / y_break_line must be a number, \"mean\" or \"median\"")
    stats::setNames(lapply(filter_values, function(k) {
      d <- df[[kol]][df$.k == k]
      if (!length(d)) return(NULL)
      if (identical(v, "mean")) mean(d) else if (identical(v, "median")) stats::median(d) else v
    }), filter_values)
  }
  ref_x <- ref_varde(p$x_break_line, x_value)
  ref_y <- ref_varde(p$y_break_line, y_value)

  # --- Trendlinjer: lm(y ~ x) per filtervärde och panel (och grupp med "group"), samma beräkning som
  #     func_graf_spridningsdiagram. Linjen är rak, så två punkter räcker; konfidensbandet är böjt och får 25 ---
  trends <- list()
  if (!isFALSE(trend_line)) {
    per_grupp <- identical(trend_line, "group")
    delar <- split(df, list(df$.k, df$.f, if (per_grupp) df$.g else rep("", nrow(df))), drop = TRUE)
    n_trend <- if (isTRUE(p$trend_line_se)) 25 else 2
    trends <- Filter(Negate(is.null), lapply(delar, function(d) {
      if (length(unique(d[[x_value]])) < 3) return(NULL)
      fit <- stats::lm(d[[y_value]] ~ d[[x_value]])
      xs  <- seq(min(d[[x_value]]), max(d[[x_value]]), length.out = n_trend)
      X   <- cbind(1, xs)
      pred <- as.vector(X %*% stats::coef(fit))
      se   <- sqrt(rowSums((X %*% stats::vcov(fit)) * X))
      t95  <- stats::qt(0.975, df = fit$df.residual)
      list(k = d$.k[1], f = d$.f[1], g = if (per_grupp) d$.g[1] else NULL,
           x = xs, y = pred, lo = pred - t95 * se, hi = pred + t95 * se)
    }))
    names(trends) <- NULL
  }

  # --- Axeldomäner: alla filtervärden (så att rörelsen syns), referenslinjer och ev. konfidensband; 5 % luft ---
  trend_facett <- vapply(trends, function(t) t$f, character(1))
  domaner <- function(rader) {
    i_trend <- which(trend_facett %in% unique(df$.f[rader]))
    xs <- c(df[[x_value]][rader], unlist(ref_x), unlist(lapply(trends[i_trend], function(t) t$x)))
    ys <- c(df[[y_value]][rader], unlist(ref_y),
            unlist(lapply(trends[i_trend], function(t) c(t$y, if (isTRUE(p$trend_line_se)) c(t$lo, t$hi)))))
    utoka <- function(v, lo, hi) {
      v <- v[is.finite(v)]
      r <- if (length(v)) range(v) else c(0, 1)
      # Bara ett värde: 10 % åt båda håll (1 runt noll), som ggplot2 ger en axel kring en enda punkt
      if (r[1] == r[2]) r <- r + c(-1, 1) * (if (r[1] == 0) 1 else abs(r[1]) * 0.1)
      d <- r + c(-0.05, 0.05) * diff(r)
      c(lo %||% d[1], hi %||% d[2])
    }
    list(x = utoka(xs, p$x_axis_min, p$x_axis_max), y = utoka(ys, p$y_axis_min, p$y_axis_max))
  }
  facet_levels <- if (!is.null(facet_var)) levels(df[[facet_var]]) else "__alla"
  facet_levels <- facet_levels[facet_levels %in% df$.f]
  n_f <- length(facet_levels)
  ncol_val <- if (!is.null(facet_var)) (p$facet_ncol %||% max(1L, if (n_f <= 2) n_f else if (n_f <= 4) 2L else 3L)) else 1L
  global_dom <- domaner(rep(TRUE, nrow(df)))
  facet_dom  <- stats::setNames(lapply(facet_levels, function(f) domaner(df$.f == f)), facet_levels)

  filter_default <- if (!is.null(p$interactive_filter_default) && as.character(p$interactive_filter_default) %in% filter_values)
    as.character(p$interactive_filter_default) else filter_values[length(filter_values)]

  groups <- lapply(group_levels, function(g) list(key = g, label = g, color = func_d3_farg(farg_map[[g]])))
  tt_dec <- max(p$data_label_decimals, 1)   # tooltipens decimaler för x, y och storlek när värdena inte är heltal
  tt_namn <- tt_kol$namn
  points <- lapply(seq_len(nrow(df)), function(i) {
    r <- list(k = df$.k[i], f = df$.f[i], g = df$.g[i], key = df$.key[i],
              x = df[[x_value]][i], y = df[[y_value]][i], r = df$.r[i],
              s = if (!is.null(size_col)) df[[size_col]][i] else NULL,
              lab = if (is.na(df$.lab[i])) NULL else df$.lab[i], show_lab = df$.show_lab[i])
    if (length(tt_namn)) r$tt <- lapply(tt_namn, function(n) df[[n]][i])   # NA -> null (visas inte)
    r
  })

  spec <- list(
    typ = "spridningsdiagram", js_fil = "spridningsdiagram",
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
    x = list(domain = global_dom$x, number_format = p$x_number_format),
    y = list(domain = global_dom$y),
    groups = groups, has_groups = !is.null(group), has_label_col = !is.null(lab_col),
    point = list(color = func_d3_farg(point_color), alpha = p$point_alpha,
                 border = if (is.null(p$point_border_color)) NULL else func_d3_farg(p$point_border_color)),
    points = points,
    size = size_legend,
    trend = list(per_group = identical(trend_line, "group"), se = isTRUE(p$trend_line_se),
                 color = func_d3_farg(p$trend_line_color), width = p$trend_line_size * 72.27 / 25.4),
    trends = trends,
    ref = list(x = ref_x, y = ref_y,
               x_label = if (!is.null(p$x_break_line_label)) as.character(p$x_break_line_label) else NULL,
               y_label = if (!is.null(p$y_break_line_label)) as.character(p$y_break_line_label) else NULL),
    facets = list(levels = as.list(facet_levels), ncol = ncol_val, scales = facet_scales,
                  domains = facet_dom, wrapped = !is.null(facet_var) && any(grepl("\n", facet_levels))),
    filter = list(enabled = !is.null(filt), label = filt, values = as.list(filter_values), default = filter_default),
    legend = list(position = p$legend_position, title = p$legend_title %||% group,
                  text_size = func_d3_pt_till_px(p$legend_text_size), title_size = func_d3_pt_till_px(11 * p$text_scale)),
    labels = list(size = func_d3_mm_till_px(p$data_label_text_size),
                  color = if (p$data_label_color == "auto") "auto" else func_d3_farg(p$data_label_color),
                  repel = p$data_label_repel, max_overlaps = if (is.finite(p$data_label_max_overlaps)) p$data_label_max_overlaps else NULL,
                  box = p$data_label_box, box_color = func_d3_farg(p$data_label_box_color), box_alpha = p$data_label_box_alpha),
    grid = func_d3_stodlinjer(p),   # major/minor, färg och tjocklek
    axis_line = list(x = p$x_axis_line, y = p$y_axis_line),
    tooltip = list(show = p$tooltip_show, unit = p$tooltip_unit %||% "",
                   x_title = p$x_axis_title %||% x_value, y_title = p$y_axis_title %||% y_value,
                   size_title = if (!is.null(size_col)) p$point_size_title %||% size_col else NULL,
                   x_decimals = func_d3_tooltip_decimaler(p$tooltip_decimals, df[[x_value]], tt_dec),
                   y_decimals = func_d3_tooltip_decimaler(p$tooltip_decimals, df[[y_value]], tt_dec),
                   size_decimals = if (!is.null(size_col)) func_d3_tooltip_decimaler(p$tooltip_decimals, df[[size_col]], tt_dec) else 0,
                   columns = tt_kol$labels),
    interactive = list(legend = p$interactive_legend, zoom = p$interactive_zoom,
                       search = isTRUE(p$interactive_search) && !is.null(lab_col), export = p$interactive_export),
    animation = list(grow = p$animation_grow, duration = p$animation_duration, play_interval = p$animation_play_interval)
  )

  if (isTRUE(p$debug)) attr(spec, "debug") <- list(data = df, trends = trends, spec = spec)
  spec
}
