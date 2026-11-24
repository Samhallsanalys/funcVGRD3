#' Rita en interaktiv täthetskarta för punkter
#'
#' Interaktiv D3-version av `funcVGR::func_graf_karta_heatmap()` med samma parametrar och
#' standardvärden. Visar hur tätt punkter (t.ex. arbetsställen) ligger, ovanpå en
#' bakgrundskarta. Utöver originalets parametrar finns tooltip på punkterna, zoom, knappar
#' för att visa eller dölja täthet och punkter, täthet per grupp, reglage för utjämning,
#' tidsreglage och export till PNG och SVG.
#'
#' @details
#' Täthetskonturerna räknas fram av ggplot2 (`stat_density_2d()`), så formerna blir
#' desamma som i ggplot2-versionen; D3 ritar bara. Med flera varianter (grupper, perioder
#' eller utjämningsnivåer) delar alla samma färgskala, så att de går att jämföra.
#'
#' Funktionen ritar inte diagrammet direkt. Den returnerar ett objekt som sparar de
#' argument du angett, och diagrammet beräknas först när objektet skrivs ut eller sparas.
#' Därför går det att ändra inställningar i efterhand med `+`, t.ex.
#' `p + d3_labs(title = "Ny rubrik")` (se [d3_plot()]).
#'
#' @section Skillnader mot func_graf_karta_heatmap():
#' Förklaringen visar punkter per km² när kartan är i meter (`legend_unit = "auto"`).
#' Originalets förklaring visar råa tätheter, som blir "0,00000".
#'
#' @param df `sf`-objekt med punktgeometrier. Obligatorisk.
#' @param df_value Namn på en numerisk kolumn i `df`, som text. Visas i tooltipen.
#'   Obligatorisk.
#' @param basemap `sf`-objekt som bakgrundskarta, t.ex. kommun- eller regiongränser.
#'   `df` transformeras till dess koordinatsystem. Obligatorisk.
#' @param group Namn på kolumnen som punkterna färgas efter, som text, t.ex. `"sektor"`.
#'   Ordningen följer samma regel som `colors`. `NULL` (standard) ger ingen gruppering.
#' @param colors Punkternas färger per grupp, används bara när `group` anges. En namnlös
#'   vektor fördelas i gruppordning (faktornivåer utan oanvända nivåer, tal sorterade,
#'   annars ordningen i data) och återanvänds om den är för kort. En namngiven vektor
#'   matchas mot gruppnamnen, t.ex. `c("Industri" = "#1a9fb3")`. Punkter där gruppen
#'   saknas (`NA`) ritas grå. Standard är VGR:s färgtema "ru" (samma färger som
#'   `func_fargtema("ru")` i funcVGR); `NULL` ger samma färger.
#' @param basemap_color Fyllnadsfärg för bakgrundskartan.
#' @param basemap_color_line Linjefärg för bakgrundskartans gränser.
#' @param adjust_value Justering av bandbredden i täthetsskattningen. Lägre värde ger
#'   tätare kluster, högre en jämnare yta.
#' @param heatmap_alpha Täthetsytornas opacitet (0–1).
#' @param bins Antal konturnivåer (typiskt 5–15).
#' @param low_color Färg för områden med lägst täthet.
#' @param high_color Färg för områden med högst täthet.
#' @param show_points Visa de enskilda punkterna.
#' @param point_size Punkternas storlek (typiskt 1–5).
#' @param point_color Punkternas färg när `group = NULL`.
#' @param point_alpha Punkternas opacitet (0–1).
#' @param title Huvudrubrik. `NULL` (standard) ger ingen rubrik.
#' @param text_scale Faktor som alla teckenstorlekar multipliceras med (rubriker,
#'   förklaring och kommunnamn). 1 = storlekarna som de är angivna, t.ex. 1.3 för stora
#'   exporter.
#' @param title_size Rubrikens teckenstorlek i punkter.
#' @param title_text_size Alias för `title_size` (bakåtkompatibilitet). Vinner om den
#'   anges.
#' @param title_width Antal tecken innan rubriken radbryts.
#' @param title_margin_top Marginal ovanför rubriken i punkter.
#' @param title_position Rubrikens placering: `"plot"` (hela bredden) eller `"panel"`
#'   (över kartan).
#' @param subtitle Underrubrik. `NULL` (standard) ger ingen underrubrik.
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
#' @param legend_title Förklaringens rubrik. `NULL` (standard) ger namnet på `df_value`-
#'   eller `group`-kolumnen.
#' @param legend_name Alias för `legend_title` (bakåtkompatibilitet). Vinner om den
#'   anges.
#' @param legend_position Förklaringens placering: `"right"`, `"left"`, `"top"`,
#'   `"bottom"` eller `"none"`.
#' @param legend_text_size Förklaringens teckenstorlek i punkter.
#' @param legend_key_shape Symbolform i förklaringen: `"square"` eller `"circle"`.
#' @param legend_breaks Önskat antal steg i förklaringen. `NULL` (standard) väljs
#'   automatiskt (oftast 5).
#' @param legend_format Talformat i förklaringen: `"number"` (undviker vetenskaplig
#'   notation) eller `"auto"` (ggplot2:s standard).
#' @param bbox_padding Marginal runt kartutsnittet som andel av utbredningen, så att
#'   konturerna inte klipps (typiskt 0,04–0,06; 0 = ingen).
#' @param element_road `sf`-lager med vägar. `NULL` (standard) ger inget.
#' @param element_ocean `sf`-lager med hav. `NULL` (standard) ger inget.
#' @param element_lake `sf`-lager med sjöar. `NULL` (standard) ger inget.
#' @param element_municipality_names `sf`-lager med kommunnamn i kolumnen `Kommunnamn`.
#'   `NULL` (standard) ger inga namn.
#' @param font Typsnitt, t.ex. `"Verdana"` eller `"Arial"`. Måste vara installerat på
#'   datorn, annars används `"sans"` (med ett meddelande).
#' @param debug `TRUE` returnerar en lista med diagnostik (data, klassindelning och den
#'   beräknade specifikationen) i stället för diagrammet.
#' @param time_column Kolumn med år eller period, som text. Ger en täthet per period och
#'   ett tidsreglage. `NULL` (standard) visar alla punkter tillsammans.
#' @param time_default År eller period som visas när kartan öppnas. `NULL` (standard)
#'   ger det senaste.
#' @param time_label_show Visa vald period med stor text i kartans hörn.
#' @param heatmap_grid_n Rutnätets upplösning i täthetsskattningen (100 = ggplot2:s
#'   standard). Högre ger mjukare kanter men större fil.
#' @param point_colors Alias för `colors` (bakåtkompatibilitet, det äldre D3-namnet).
#'   Vinner om båda anges och den inte är `NULL`.
#' @param legend_unit Täthetens enhet i förklaringen. `"auto"` (standard) ger "punkter
#'   per km²" när kartan är i meter (värdena räknas om), annan text visas som den är med
#'   råa värden, och `NULL` ger rå täthet som i `funcVGR::func_graf_karta_heatmap()`.
#' @param tooltip_show Visa en tooltip när muspekaren hålls över en punkt.
#' @param tooltip_name_column Kolumn som används som rubrik i tooltipen, som text.
#'   `NULL` (standard) letar efter `"namn"`, `"name"`, `"arbetsstalle"` eller
#'   `"kommunnamn"`, annars ingen rubrik.
#' @param tooltip_columns Extra kolumner som visas i tooltipen, gärna namngivna för
#'   visning, t.ex. `c("Befolkning" = "bef")`. `NULL` (standard) ger inga.
#' @param tooltip_decimals Antal decimaler för tal i tooltipen. `NULL` (standard) ger 0
#'   om alla värden är heltal, annars 1. Kolumnerna i `tooltip_columns` avgörs var för
#'   sig, och heltalskolumner visas alltid utan decimaler.
#' @param tooltip_unit Enhet som läggs efter värdet i `df_value`, t.ex. `" anställda"`.
#' @param interactive_zoom Zooma och panorera med mushjulet och genom att dra.
#' @param interactive_layer_toggle Visa knappar för att visa eller dölja tätheten och
#'   punkterna.
#' @param interactive_group_density Med `group`: visa en rullista för att se tätheten
#'   för en grupp i taget (räknas fram i R).
#' @param interactive_adjust_values Numerisk vektor med extra utjämningsnivåer som ger
#'   ett reglage, t.ex. `c(0.25, 0.5, 1)`. `NULL` (standard) ger inget reglage.
#' @param interactive_legend Klicka på en grupp i förklaringen för att dölja eller visa
#'   dess punkter, håll muspekaren över den för att framhäva den.
#' @param interactive_export Visa knappar för att ladda ned diagrammet som PNG eller
#'   SVG.
#' @param animation_duration Övergångstid i millisekunder när tätheten byts. 0 = ingen
#'   animation.
#' @param animation_play_interval Tid i millisekunder per period när uppspelningsknappen
#'   används.
#' @param width Kartans bredd i pixlar.
#' @param height Kartans höjd i pixlar.
#' @param .rendera Intern. `TRUE` bygger diagrammet och används när objektet skrivs ut.
#'   Ändra inte.
#'
#' @return Ett objekt av klassen `vgr_d3`. Det visas i RStudio:s Viewer när det skrivs ut,
#'   i HTML-dokument från R Markdown och Quarto och på sidan från "Run examples" i
#'   hjälpen. Spara det med [func_d3_spara()].
#'   Med `debug = TRUE` returneras i stället en lista med diagnostik.
#' @seealso [func_d3_karta()], [d3_plot()] och [func_d3_spara()].
#' @export
#' @example inst/examples/func_d3_karta_heatmap.R
func_d3_karta_heatmap <- function(
    # === REQUIRED PARAMETERS ===
  df,
  df_value,
  basemap,

  # === GROUPING ===
  group = NULL,
  colors = func_d3_farger_ru(),

  # === MAP STYLING ===
  basemap_color = "white",
  basemap_color_line = "#d3d3d3",

  # === HEATMAP SETTINGS ===
  adjust_value = 0.5,
  heatmap_alpha = 0.5,
  bins = 10,
  low_color = "black",
  high_color = "red",

  # === POINTS DISPLAY ===
  show_points = TRUE,
  point_size = 2,
  point_color = "black",
  point_alpha = 1,

  # === TITLE AND TEXT ===
  title = NULL,
  text_scale = 1,
  title_size = 16,
  title_text_size = 16,
  title_width = 100,
  title_margin_top = 2,
  title_position = "plot",
  subtitle = NULL,
  subtitle_text_size = 13,
  caption = NULL,
  caption_size = 10,
  caption_text_size = 10,

  # === MAP MARGINS ===
  margin_top = 5,
  margin_right = 5,
  margin_bottom = 5,
  margin_left = 5,

  # === LEGEND ===
  legend_title = NULL,
  legend_name = NULL,
  legend_position = "none",
  legend_text_size = 11,
  legend_key_shape = "square",
  legend_breaks = NULL,
  legend_format = "number",

  # === BBOX EXPANSION ===
  bbox_padding = 0.05,

  # === ADDITIONAL LAYERS ===
  element_road = NULL,
  element_ocean = NULL,
  element_lake = NULL,
  element_municipality_names = NULL,

  # === ADVANCED ===
  font = "Verdana",
  debug = FALSE,

  # ------------------------------------------------------------------
  # D3-tillägg: nya parametrar ligger efter originalets, så positionella
  # anrop träffar samma parametrar som i func_graf-originalet
  # ------------------------------------------------------------------

  # === D3: TIME ===
  time_column = NULL,
  time_default = NULL,
  time_label_show = TRUE,

  # === D3: HEATMAP SETTINGS ===
  heatmap_grid_n = 100,

  # === D3: POINTS DISPLAY ===
  point_colors = NULL,

  # === D3: LEGEND ===
  legend_unit = "auto",

  # === D3: TOOLTIP ===
  tooltip_show = TRUE,
  tooltip_name_column = NULL,
  tooltip_columns = NULL,
  tooltip_decimals = NULL,
  tooltip_unit = "",

  # === D3: INTERACTIVITY ===
  interactive_zoom = TRUE,
  interactive_layer_toggle = TRUE,
  interactive_group_density = TRUE,
  interactive_adjust_values = NULL,
  interactive_legend = TRUE,
  interactive_export = TRUE,

  # === D3: ANIMATION ===
  animation_duration = 500,
  animation_play_interval = 1500,

  # === D3: SIZE ===
  width = 800,
  height = 900,
  .rendera = FALSE
) {

  if (!.rendera) {
    angivna <- setdiff(names(as.list(match.call()))[-1], ".rendera")
    obj <- func_d3_nytt_objekt("karta_heatmap", mget(angivna, envir = environment()))
    if (debug) return(attr(func_d3_spec(obj), "debug"))
    return(obj)
  }

  # Resolve alternative parameter names (last-one-wins if both are supplied)
  title_size   <- if (!missing(title_text_size))   title_text_size   else title_size
  caption_size <- if (!missing(caption_text_size)) caption_text_size else caption_size
  legend_title <- if (!missing(legend_name))       legend_name       else legend_title
  # text_scale: en faktor för alla textstorlekar (som func_graf_karta_heatmap)
  title_size         <- title_size         * text_scale
  caption_size       <- caption_size       * text_scale
  subtitle_text_size <- subtitle_text_size * text_scale
  legend_text_size   <- legend_text_size   * text_scale

  p <- mget(setdiff(names(formals()), c(".rendera", "title_text_size", "caption_text_size", "legend_name", "colors", "point_colors")),
            envir = environment())
  # colors räknas bara ut med group (så att en karta utan grupper inte räknar ut gruppfärgerna);
  # point_colors är det gamla D3-namnet och vinner om båda anges
  p$colors <- if (!is.null(group)) (if (!missing(point_colors) && !is.null(point_colors)) point_colors else colors) else NULL
  func_d3_karta_heatmap_bygg(p)
}

func_d3_karta_heatmap_bygg <- function(p) {
  df <- p$df; basemap <- p$basemap; group <- p$group; tid <- p$time_column

  # === VALIDATE INPUTS (same messages as func_graf_karta_heatmap) ===
  if (is.null(df) || nrow(df) == 0) stop("Error: 'df' must be a non-empty data frame")
  if (!p$df_value %in% names(df)) stop(paste("Error: Column", p$df_value, "not found in df"))
  if (!is.null(group) && !group %in% names(df)) stop(paste("Error: Column", group, "not found in df"))
  if (!is.null(tid) && !tid %in% names(df)) stop(paste("Error: Column", tid, "not found in df"))
  if (!inherits(df, "sf")) stop("Error: 'df' must be an sf object with point geometries")
  if (!p$legend_position %in% c("none", "top", "bottom", "left", "right"))
    stop("legend_position must be one of: none, top, bottom, left, right")

  # === PREPARE DATA ===
  # Tätheten räknas i basemaps koordinatsystem, precis som originalet (även om det är lon/lat).
  # Ritningen sker i SWEREF99 TM om basemap är lon/lat (geoIdentity förvränger annars kartan);
  # konturerna räknas först och projiceras sedan om, så formerna blir desamma som i ggplot.
  berakna_crs <- sf::st_crs(basemap)
  df <- sf::st_transform(df, crs = berakna_crs)
  xy <- sf::st_coordinates(df)
  if (any(is.na(xy[, 1])) || any(is.na(xy[, 2]))) stop("Error: Coordinates contain missing values")
  rit_crs <- if (isTRUE(sf::st_is_longlat(basemap))) sf::st_crs(3006) else berakna_crs
  ompro <- !isTRUE(rit_crs == berakna_crs)
  till_rit <- function(m) if (ompro) sf::sf_project(berakna_crs, rit_crs, m) else m
  lager <- lapply(list(road = p$element_road, ocean = p$element_ocean, lake = p$element_lake, names = p$element_municipality_names),
                  function(x) if (is.null(x)) NULL else sf::st_transform(x, rit_crs))

  # === BOUNDING BOX (basemap + padding, as the original) ===
  bbox <- sf::st_bbox(basemap)
  if (p$bbox_padding > 0) {
    xr <- bbox["xmax"] - bbox["xmin"]; yr <- bbox["ymax"] - bbox["ymin"]
    bbox["xmin"] <- bbox["xmin"] - xr * p$bbox_padding; bbox["xmax"] <- bbox["xmax"] + xr * p$bbox_padding
    bbox["ymin"] <- bbox["ymin"] - yr * p$bbox_padding; bbox["ymax"] <- bbox["ymax"] + yr * p$bbox_padding
  }
  if (ompro) bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bbox), rit_crs))
  basemap_rit <- if (ompro) sf::st_transform(basemap, rit_crs) else basemap

  # kde2d:s rutnät: i originalet räknas tätheten innan sf-lagren tränat skalorna, så rutnätet
  # täcker bara punkternas omfång. Varianter (grupp/period) använder samma rutnät som alla punkter.
  lims_x <- range(xy[, 1]); lims_y <- range(xy[, 2])

  # === TÄTHETSKONTURER via ggplot2 (identiskt med stat_density_2d i originalet) ===
  kontur <- function(punkter, adjust) {
    if (nrow(punkter) < 3) return(NULL)
    pl <- ggplot(punkter, aes(x = x, y = y)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", bins = p$bins,
                      contour_var = "count", adjust = adjust, n = p$heatmap_grid_n) +
      expand_limits(x = lims_x, y = lims_y)
    ld <- tryCatch(layer_data(pl, 1), error = function(e) NULL)
    if (is.null(ld) || nrow(ld) == 0) return(NULL)
    ld$group <- as.character(ld$group)
    rit <- till_rit(cbind(ld$x, ld$y))
    ld$x <- rit[, 1]; ld$y <- rit[, 2]
    # unname(): en namnlös lista blir en JSON-array (med gruppnamnen blir det ett objekt)
    unname(lapply(split(ld, factor(ld$group, levels = unique(ld$group))), function(d) {
      list(level = d$level[1], pts = unname(lapply(seq_len(nrow(d)), function(i) round(c(d$x[i], d$y[i]), 1))))
    }))
  }

  punkt_df <- data.frame(x = xy[, 1], y = xy[, 2])
  perioder <- if (!is.null(tid)) as.character(sort(unique(df[[tid]]))) else "__alla"
  # Grupper (samma färgregel som func_graf_karta_heatmap/linje/stapel/karta): färgordning = faktornivåer
  # (oanvända bort), numeriskt sorterat, annars första förekomst; NA får ingen färgplats (grey50).
  # Legendordningen är ggplots: faktornivåer, annars alfabetiskt; saknad grupp blir "NA" sist
  farg_ordning <- NULL; legend_ordning <- character(0); har_na_grupp <- FALSE
  if (!is.null(group)) {
    gv <- df[[group]]
    if (is.numeric(gv)) gv <- factor(gv)
    farg_ordning   <- if (is.factor(gv)) setdiff(levels(droplevels(gv)), NA) else unique(stats::na.omit(as.character(gv)))
    legend_ordning <- if (is.factor(gv)) farg_ordning else sort(farg_ordning)
    har_na_grupp   <- anyNA(as.character(gv))   # as.character: ser även en uttrycklig NA-nivå (addNA)
  }
  # unique(): en grupp som heter "NA" och saknade värden blir samma "NA"-grupp (som i linje/stapel)
  grupp_nivaer <- if (!is.null(group)) unique(c(legend_ordning, if (har_na_grupp) "NA")) else character(0)
  tathet_grupper <- c("__alla", if (!is.null(group) && p$interactive_group_density) grupp_nivaer)
  justeringar <- unique(c(p$adjust_value, p$interactive_adjust_values))

  n_varianter <- length(perioder) * length(tathet_grupper) * length(justeringar)
  if (n_varianter > 80) {
    warning("func_d3_karta_heatmap: ", n_varianter, " täthetsvarianter blir en stor fil — täthet per grupp stängs av.")
    tathet_grupper <- "__alla"
  }
  varianter <- list()
  period_av_rad <- if (!is.null(tid)) as.character(df[[tid]]) else rep("__alla", nrow(df))
  grupp_av_rad  <- if (!is.null(group)) { g_txt <- as.character(df[[group]]); ifelse(is.na(g_txt), "NA", g_txt) } else rep("__alla", nrow(df))
  # Nyckeln för utjämningen skickas färdig till JS (JS räknar inte om den — 1/3 skrivs olika i R och JS)
  justeringar <- sort(justeringar)
  adj_nycklar <- format(justeringar, digits = 15, trim = TRUE, scientific = FALSE)
  for (per in perioder) for (gr in tathet_grupper) for (j in seq_along(justeringar)) {
    urval <- period_av_rad == per & (gr == "__alla" | grupp_av_rad == gr)
    varianter[[paste(per, gr, adj_nycklar[j], sep = "|")]] <- kontur(punkt_df[urval, , drop = FALSE], justeringar[j]) %||% list()
  }
  alla_nivaer <- unlist(lapply(varianter, function(v) vapply(v, function(k) k$level, numeric(1))))
  if (length(alla_nivaer) == 0) {
    warning("func_d3_karta_heatmap: kunde inte beräkna någon täthet (för få punkter?).")
    alla_nivaer <- c(0, 1)
  }
  niva_range <- range(alla_nivaer)

  # === Legend för tätheten ===
  # Nivåerna är punkter per kvadratmeter (contour_var = "count"). Med meter-koordinater blir det
  # mycket små tal ("0,00000" i originalet), så legenden visas i punkter per km² i stället.
  i_meter <- identical(berakna_crs$units_gdal, "metre") || identical(berakna_crs$units, "m")
  auto_enhet <- identical(p$legend_unit, "auto")
  legend_unit <- if (auto_enhet) (if (i_meter) "punkter per km²" else NULL) else p$legend_unit
  # Bara "auto" räknar om till km²; en egen enhetstext visas med de råa värdena
  skala <- if (auto_enhet && i_meter) 1e6 else 1
  skalad <- niva_range * skala
  brk <- if (!is.null(p$legend_breaks)) scales::breaks_pretty(n = p$legend_breaks)(skalad) else scales::extended_breaks()(skalad)
  brk <- brk[brk >= skalad[1] & brk <= skalad[2]]
  brk_etiketter <- if (p$legend_format == "number" && length(brk) > 0) {
    # Minsta antal decimaler som visar varje brytpunkt exakt (2,5 blir inte "2"); en egen
    # legend_unit med råa tätheter kan ge värden kring 1e-7, därför upp till 15 decimaler
    tol <- max(abs(brk)) * 1e-9
    dec <- which(vapply(0:15, function(d) all(abs(round(brk, d) - brk) <= tol), logical(1)))[1] - 1
    if (is.na(dec)) sub(".", ",", format(brk, digits = 3, scientific = TRUE, trim = TRUE), fixed = TRUE)
    else func_d3_format_tal(brk, dec)
  } else format(brk, trim = TRUE)
  brk <- brk / skala
  legend_titel <- if (!is.null(p$legend_title) && !is.null(legend_unit)) paste0(p$legend_title, " (", legend_unit, ")")
                  else p$legend_title %||% legend_unit

  # === Punkter ===
  namn_kol <- p$tooltip_name_column %||% intersect(c("namn", "name", "arbetsstalle", "kommunnamn", "Kommunnamn"), names(df))[1]
  data_df <- sf::st_drop_geometry(df)
  tt_kol <- p$tooltip_columns
  tt_etiketter <- names(tt_kol) %||% rep("", length(tt_kol))
  tt_etiketter[tt_etiketter == ""] <- tt_kol[tt_etiketter == ""]
  saknas <- setdiff(tt_kol, names(data_df))
  if (length(saknas)) stop("Missing columns in data: ", paste(saknas, collapse = ", "))
  # Decimaler (gemensam regel): huvudvärdet följer ett angivet tooltip_decimals som det är; extra
  # kolumner avgörs per kolumn och heltalskolumner visas alltid utan decimaler (som i func_d3_karta)
  tt_dec <- function(x) func_d3_tooltip_decimaler(if (is.numeric(x) && all(is.na(x) | x == round(x))) 0 else p$tooltip_decimals, x, 1)
  tt_dec_kol <- lapply(stats::setNames(nm = unique(tt_kol)), function(k) tt_dec(data_df[[k]]))
  tt_dec_huvud <- func_d3_tooltip_decimaler(p$tooltip_decimals, data_df[[p$df_value]], 1)
  fmt_cell <- function(v, dec) if (is.numeric(v)) func_d3_format_tal(v, dec) else as.character(v)
  xy_rit <- till_rit(xy)
  punkter <- lapply(seq_len(nrow(df)), function(i) {
    list(x = round(xy_rit[i, 1], 1), y = round(xy_rit[i, 2], 1),
         v = if (is.na(data_df[[p$df_value]][i])) NULL else paste0(fmt_cell(data_df[[p$df_value]][i], tt_dec_huvud), p$tooltip_unit %||% ""),
         g = if (!is.null(group)) grupp_av_rad[i] else NULL,
         t = period_av_rad[i],
         n = if (!is.na(namn_kol)) as.character(data_df[[namn_kol]][i]) else NULL,
         c = if (length(tt_kol)) unname(lapply(tt_kol, function(k) {
           cell <- data_df[[k]][i]
           if (is.na(cell)) NULL else fmt_cell(cell, tt_dec_kol[[k]])
         })) else NULL)
  })
  punkt_farger <- NULL
  if (!is.null(group)) {
    farger <- p$colors %||% func_d3_farger_ru()
    if (!is.null(names(farger))) {
      saknas_farg <- setdiff(farg_ordning, names(farger))
      if (length(saknas_farg) > 0) stop("colors is named but has no color for: ", toString(saknas_farg))
      farg_map <- farger[farg_ordning]
    } else {
      farg_map <- stats::setNames(rep(farger, length.out = length(farg_ordning)), farg_ordning)
    }
    punkt_farger <- as.list(func_d3_farg(unname(farg_map[legend_ordning])))
    names(punkt_farger) <- legend_ordning
    if (har_na_grupp) punkt_farger[["NA"]] <- func_d3_farg("grey50")
    punkt_farger <- punkt_farger[grupp_nivaer]
  }

  # === Extra lager ===
  geo <- function(x) if (is.null(x)) NULL else func_d3_geojson(x)
  kommunnamn <- list()
  if (!is.null(lager$names)) {
    if (!"Kommunnamn" %in% names(lager$names)) stop("Error: element_municipality_names must have a 'Kommunnamn' column")
    pos <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(lager$names)))
    kommunnamn <- lapply(seq_len(nrow(pos)), function(i) list(x = round(pos[i, 1], 1), y = round(pos[i, 2], 1),
                                                              text = as.character(lager$names$Kommunnamn[i])))
  }

  standard_period <- if (!is.null(p$time_default) && as.character(p$time_default) %in% perioder) as.character(p$time_default) else perioder[length(perioder)]

  spec <- list(
    typ = "karta_heatmap", js_fil = "karta_heatmap",
    width = p$width, height = p$height, font = func_d3_typsnitt(p$font),
    margin = list(t = func_d3_pt_till_px(p$margin_top), r = func_d3_pt_till_px(p$margin_right),
                  b = func_d3_pt_till_px(p$margin_bottom), l = func_d3_pt_till_px(p$margin_left)),
    text = list(title = func_d3_rader(p$title, p$title_width), subtitle = func_d3_rader(p$subtitle),
                caption = func_d3_rader(p$caption),
                title_size = func_d3_pt_till_px(p$title_size), title_margin_top = func_d3_pt_till_px(p$title_margin_top),
                title_position = p$title_position,
                subtitle_size = func_d3_pt_till_px(p$subtitle_text_size),
                caption_size = func_d3_pt_till_px(p$caption_size)),
    bbox = unname(as.numeric(bbox[c("xmin", "ymin", "xmax", "ymax")])),
    basemap = func_d3_geojson(basemap_rit),
    basemap_color = func_d3_farg(p$basemap_color), basemap_color_line = func_d3_farg(p$basemap_color_line),
    layers = list(road = geo(lager$road), ocean = geo(lager$ocean), lake = geo(lager$lake), names = kommunnamn,
                  names_size = func_d3_mm_till_px(2 * p$text_scale)),
    heat = list(variants = varianter, domain = niva_range,
                low = func_d3_farg(p$low_color), high = func_d3_farg(p$high_color), alpha = p$heatmap_alpha,
                ticks = as.list(brk), tick_labels = as.list(brk_etiketter),
                adjust_values = as.list(justeringar), adjust_keys = as.list(adj_nycklar),
                adjust_index = match(p$adjust_value, justeringar) - 1L),
    text_scale = p$text_scale,   # för periodstämpeln
    periods = as.list(perioder), default_period = standard_period,
    density_groups = as.list(tathet_grupper),
    points = list(show = p$show_points, data = punkter, r = func_d3_punkt_px(p$point_size),
                  color = func_d3_farg(p$point_color), alpha = p$point_alpha,
                  group_colors = punkt_farger, groups = as.list(grupp_nivaer)),
    legend = list(position = p$legend_position, title = legend_titel,
                  group_title = p$legend_title %||% group,
                  text_size = func_d3_pt_till_px(p$legend_text_size), key_shape = p$legend_key_shape),
    tooltip = list(show = p$tooltip_show, value_label = p$df_value, group_label = group, name = !is.na(namn_kol),
                   column_labels = as.list(tt_etiketter)),
    time = list(enabled = !is.null(tid) && length(perioder) > 1, label_show = p$time_label_show),
    interactive = list(zoom = p$interactive_zoom, layer_toggle = p$interactive_layer_toggle,
                       group_density = length(tathet_grupper) > 1,
                       adjust_slider = length(justeringar) > 1,
                       legend = p$interactive_legend, export = p$interactive_export),
    animation = list(duration = p$animation_duration, play_interval = p$animation_play_interval)
  )

  if (isTRUE(p$debug)) {
    attr(spec, "debug") <- list(data = df, bbox = bbox, bbox_padding_applied = p$bbox_padding,
                                group_levels = farg_ordning,
                                n_points = nrow(df), n_variants = length(varianter),
                                level_range = niva_range, spec = spec)
  }
  spec
}
