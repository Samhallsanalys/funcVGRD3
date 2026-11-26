#' Rita ett interaktivt ytdiagram
#'
#' Staplat ytdiagram eller 100 % staplat ytdiagram, med samma argument som
#' [func_d3_linjediagram()] (utom dataetiketter, markering, prognos och växling till index).
#' Varje grupp är en yta ovanpå de andra; med `area_type = "filled"` visas gruppernas andel
#' av summan vid varje x-värde, och y-axeln går från 0 till 100 %. Tooltipen visar alla
#' grupper vid x-värdet med värde, andel och summa.
#'
#' @details
#' Den första gruppen (efter faktornivåer, annars i alfabetisk ordning) hamnar överst och
#' den sista närmast x-axeln, som `ggplot2::geom_area()` med `position_stack()`. Förklaringen
#' står i samma ordning, uppifrån och ned; `legend_reverse = TRUE` vänder den.
#'
#' Ett x-värde som saknas i en grupp räknas som 0, så ytorna ska ha samma x-värden. Har
#' flera rader samma panel, grupp och x används den första. Värdena bör vara positiva.
#'
#' x-axeln har ingen luft utanför data, så att ytorna går ut till axlarna. Med
#' `area_type = "filled"` går y-axeln från 0 till 1 (100 %), och `y_break_line` anges då
#' som andel, t.ex. `0.5`.
#'
#' Funktionen ritar inte diagrammet direkt. Den returnerar ett objekt som sparar de
#' argument du angett, och diagrammet beräknas först när objektet skrivs ut eller sparas.
#' Därför går det att ändra inställningar i efterhand med `+`, t.ex.
#' `p + d3_labs(title = "Ny rubrik")` (se [d3_plot()]).
#'
#' @inheritParams func_d3_linjediagram
#' @param group Namn på kolumnen som delar upp data i ytor med egen färg, som text. `NULL`
#'   (standard) ger en yta.
#' @param colors Färger för grupperna. En namnlös vektor fördelas i gruppordning
#'   (faktornivåer utan oanvända nivåer, tal sorterade, annars ordningen i data) och
#'   återanvänds om den är för kort. En namngiven vektor matchas mot gruppnamnen. Standard
#'   är VGR:s färgtema "ru"; `NULL` ger viridis-färger.
#' @param area_type `"stacked"` (staplade ytor) eller `"filled"` (100 %: varje grupps
#'   andel av summan vid x-värdet).
#' @param area_alpha Ytornas opacitet (0–1).
#' @param area_line_color Färg på linjen längs varje ytas överkant, som skiljer ytorna åt.
#'   `NULL` ger ingen linje.
#' @param area_line_width Linjens tjocklek (ggplot2:s `linewidth`).
#' @param legend_position Förklaringens placering: `"right"`, `"left"`, `"top"`,
#'   `"bottom"` eller `"none"`.
#' @param legend_reverse `TRUE` visar förklaringen i omvänd ordning (den nedersta ytan
#'   först).
#' @param x_axis_breaks Värdena för skalstrecken på en tal- eller datumaxel, t.ex.
#'   `seq(1970, 2024, 2)` (årtal vartannat år) eller en vektor med datum. Går före
#'   `x_axis_n_breaks` och `x_date_breaks`. Värden utanför data visas inte (om inte
#'   `x_axis_min`/`x_axis_max` vidgar axeln), och med `x_number_format = "whole"` bara
#'   heltal. Gäller inte kategorier, och inte i inzoomat läge, där skalstrecken väljs om.
#'   `NULL` (standard) väljs automatiskt.
#' @param y_axis_expand Luft på y-axeln utanför data och `y_axis_min`/`y_axis_max`, som
#'   andel av spannet: `c(nedre, övre)`. `NULL` (standard) ger `c(0, 0.05)` för staplade
#'   ytor och `c(0, 0)` för 100 %, så att ytorna börjar vid x-axeln.
#' @param y_axis_format Y-axelns format: `"number"` (standard) eller `"percent"` för
#'   andelar mellan 0 och 1. Med `area_type = "filled"` visas y-axeln alltid i procent;
#'   tooltipen visar då värdena som de är och andelen inom parentes.
#' @param tooltip_show Visa en tooltip med alla grupper vid x-värdet under muspekaren.
#' @param interactive_legend Klicka på en grupp i förklaringen för att dölja eller visa
#'   ytan (de övriga staplas om), dubbelklicka för att bara visa den, håll muspekaren över
#'   den för att framhäva den.
#' @param animation_draw Tona in ytorna när diagrammet öppnas.
#'
#' @return Ett objekt av klassen `vgr_d3`. Det visas i RStudio:s Viewer när det skrivs ut,
#'   i HTML-dokument från R Markdown och Quarto och på sidan från "Run examples" i
#'   hjälpen. Spara det med [func_d3_spara()].
#'   Med `debug = TRUE` returneras i stället en lista med diagnostik.
#' @seealso [func_d3_linjediagram()], [func_d3_stapeldiagram()], [d3_area()] och
#'   [func_d3_spara()].
#' @export
#' @example inst/examples/func_d3_ytdiagram.R
func_d3_ytdiagram <- function(
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

  # === AREAS ===
  area_type = "stacked",
  area_alpha = 1,
  area_line_color = "white",
  area_line_width = 0.3,

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

  # === TEXT SIZING (in points) ===
  text_scale = 1,
  title_size = 16,
  subtitle_size = 13,
  caption_size = 10,
  x_axis_text_size = 11,
  y_axis_text_size = 11,
  legend_text_size = 11,

  # === LAYOUT ===
  legend_position = "right",
  legend_reverse = FALSE,
  show_major_grid = FALSE,
  show_minor_grid = FALSE,
  x_axis_line = TRUE,
  y_axis_line = TRUE,
  x_angle = 0,
  x_angle_auto = FALSE,
  x_axis_breaks = NULL,
  x_axis_n_breaks = NULL,

  # === REFERENCE LINES AND SHADED PERIODS ===
  x_break_line = NULL,
  x_break_line_label = NULL,
  y_break_line = NULL,
  y_break_line_label = NULL,
  x_shade_from = NULL,
  x_shade_to = NULL,
  x_shade_label = NULL,
  x_shade_color = "grey90",

  # === AXES ===
  x_axis_min = NULL,
  x_axis_max = NULL,
  y_axis_min = NULL,
  y_axis_max = NULL,
  y_axis_expand = NULL,
  y_axis_format = "number",
  x_date_labels = "%b %Y",
  x_date_breaks = "auto",
  x_number_format = "whole",

  # === GRID LINES ===
  grid_major_color = "#e5e5e5",
  grid_minor_color = "#f2f2f2",
  grid_major_width = 0.5,
  grid_minor_width = 0.25,

  # === TOOLTIP ===
  tooltip_show = TRUE,
  tooltip_decimals = NULL,
  tooltip_unit = "",
  tooltip_columns = NULL,
  tooltip_x_format = NULL,

  # === INTERACTIVITY AND ANIMATION ===
  interactive_legend = TRUE,
  interactive_zoom = TRUE,
  interactive_export = TRUE,
  animation_draw = TRUE,
  animation_duration = 900,

  # === ADVANCED AND SIZE ===
  debug = FALSE,
  width = 1400,
  height = 800,
  .rendera = FALSE
) {

  if (!.rendera) {
    angivna <- setdiff(names(as.list(match.call()))[-1], ".rendera")
    obj <- func_d3_nytt_objekt("ytdiagram", mget(angivna, envir = environment()))
    if (debug) return(attr(func_d3_spec(obj), "debug"))
    return(obj)
  }

  if (!is.character(area_type) || length(area_type) != 1 || !area_type %in% c("stacked", "filled"))
    stop("area_type must be \"stacked\" or \"filled\"")
  if (!is.numeric(area_alpha) || length(area_alpha) != 1 || is.na(area_alpha) || area_alpha < 0 || area_alpha > 1)
    stop("area_alpha must be a number between 0 and 1")

  # text_scale: en faktor för alla textstorlekar (som func_d3_linjediagram)
  title_size       <- title_size       * text_scale
  subtitle_size    <- subtitle_size    * text_scale
  caption_size     <- caption_size     * text_scale
  x_axis_text_size <- x_axis_text_size * text_scale
  y_axis_text_size <- y_axis_text_size * text_scale
  legend_text_size <- legend_text_size * text_scale
  facet_title_size <- facet_title_size * text_scale

  # Linjediagrammets standardvärden för allt som ytdiagrammet inte har, och sedan de egna argumenten
  std <- formals(func_d3_linjediagram)
  std <- lapply(std[setdiff(names(std), c("df", "x_value", "y_value", ".rendera"))], eval, envir = environment(func_d3_linjediagram))
  egna <- setdiff(names(formals()), c(".rendera", "area_type", "area_alpha", "area_line_color", "area_line_width"))
  p <- std
  for (n in egna) p[n] <- list(get(n, envir = environment()))
  # Inget av linjediagrammets linjetillägg (etiketter, markering, prognos, band, index) gäller ytor
  p$data_labels <- FALSE
  p$legend_direct <- FALSE
  p$interactive_index_toggle <- FALSE
  p$tooltip_mode <- "all"
  p$y_axis_expand <- y_axis_expand %||% (if (area_type == "filled") c(0, 0) else c(0, 0.05))
  p$.area <- list(type = area_type, alpha = area_alpha,
                  line_color = if (is.null(area_line_color)) NULL else func_d3_farg(area_line_color),
                  line_width = area_line_width * 72.27 / 25.4)
  func_d3_linjediagram_bygg(p)
}
