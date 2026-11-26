# "+"-grammatiken: d3_plot(df) + d3_line(...) + d3_labs(...). Ett lager är en namngiven parameterlista
# (och ev. en diagramtyp) som "+" lägger på ett vgr_d3-objekt.

# Startpunkt: data + valfria parametrar. Diagramtypen sätts av d3_map()/d3_map_change()/d3_heatmap(),
# d3_line()/d3_bar()/d3_dumbbell() eller d3_pie()/d3_donut().
#' Starta ett diagram för `+`-syntaxen
#'
#' Ett alternativ till att anropa `func_d3_*()` med alla argument på en gång, i samma anda
#' som ggplot2. `d3_plot()` tar emot data, ett diagramtypslager väljer diagramtyp och
#' övriga lager lägger till inställningar: `d3_plot(df) + d3_line(x_value = "ar", y_value
#' = "antal") + d3_labs(title = "...")`.
#'
#' @details
#' Diagramtypen väljs med [d3_map()], [d3_map_change()], [d3_heatmap()], [d3_line()],
#' [d3_area()], [d3_bar()], [d3_dumbbell()], [d3_pie()], [d3_scatter()],
#' [d3_histogram()] eller [d3_boxplot()].
#' Inställningar läggs till med lager som
#' tar fullständiga parameternamn ([d3_labs()], [d3_theme()], [d3_colors()],
#' [d3_params()], [d3_size()]), lager för en parameterfamilj ([d3_labels()],
#' [d3_legend()], [d3_breaks()], [d3_tooltip()], [d3_interactive()], [d3_animation()],
#' [d3_margin()], [d3_time()], [d3_bubbles()], [d3_facet()], [d3_highlight()],
#' [d3_donut()]) och lager för kartlager och prognos ([d3_basemap()], [d3_overlay()],
#' [d3_forecast()]). Varje lager har en egen hjälpsida.
#'
#' Lagren kan också läggas på ett objekt från en vanlig `func_d3_*()`-funktion, t.ex.
#' `karta + d3_legend(position = "right")`. Ett senare lager vinner över ett tidigare, och
#' ett lager som sätter ett huvudnamn (t.ex. `legend_title`) ersätter ett tidigare angivet
#' äldre alias (`legend_name`). Ett parameternamn som diagramtypen inte har ger ett fel,
#' men först när diagramtypen är vald.
#'
#' @param df Data frame eller `sf`-objekt med data. Obligatorisk.
#' @param ... Valfria parametrar till diagramfunktionen med fullständiga namn, t.ex.
#'   `title` eller `x_value`. Alla måste vara namngivna.
#'
#' @return Ett objekt av klassen `vgr_d3` (se t.ex. [func_d3_linjediagram()]). Det visas
#'   när det skrivs ut, men först när en diagramtyp har lagts till.
#' @seealso [func_d3_hjalp()] listar diagramfunktionerna och lagren.
#' @export
#' @example inst/examples/d3_plot.R
d3_plot <- function(df, ...) func_d3_nytt_objekt(NULL, c(list(df = df), list(...)))

#' Välj koropletkarta i `+`-syntaxen
#'
#' Gör diagrammet till en koropletkarta i `+`-syntaxen. Lagret tar samma parametrar som
#' [func_d3_karta()], med fullständiga namn, t.ex. `df_value`, `geom_col` och `group`.
#'
#' @details
#' De obligatoriska parametrarna i [func_d3_karta()] måste finnas någonstans i diagrammet:
#' `df` anges i [d3_plot()], de övriga i det här lagret eller i ett annat lager. Lägg till
#' lagret på ett objekt som redan har en annan diagramtyp så byts typen; parametrar som
#' betyder samma sak i båda typerna (t.ex. `data_labels` och `data_label_show`) får rätt
#' namn automatiskt.
#'
#' @param ... Parametrar till [func_d3_karta()] med fullständiga namn. Alla måste vara
#'   namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_map.R
d3_map        <- function(...) func_d3_lager(list(...), typ = "karta")          # Koropletkarta (func_d3_karta): df_value, group, colors, geom_col, basemap ...
#' Välj förändringskarta i `+`-syntaxen
#'
#' Gör diagrammet till en förändringskarta i `+`-syntaxen. Lagret tar samma parametrar som
#' [func_d3_karta_change()], med fullständiga namn, t.ex. `df_value`, `geom_col` och
#' `year_column`.
#'
#' @details
#' De obligatoriska parametrarna i [func_d3_karta_change()] måste finnas någonstans i
#' diagrammet: `df` anges i [d3_plot()], de övriga i det här lagret eller i ett annat
#' lager. Lägg till lagret på ett objekt som redan har en annan diagramtyp så byts typen;
#' parametrar som betyder samma sak i båda typerna (t.ex. `data_labels` och
#' `data_label_show`) får rätt namn automatiskt.
#'
#' @param ... Parametrar till [func_d3_karta_change()] med fullständiga namn. Alla måste
#'   vara namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_map_change.R
d3_map_change <- function(...) func_d3_lager(list(...), typ = "karta_change")   # Förändringskarta (func_d3_karta_change): df_value, year_column, geom_col ...
#' Välj täthetskarta i `+`-syntaxen
#'
#' Gör diagrammet till en täthetskarta i `+`-syntaxen. Lagret tar samma parametrar som
#' [func_d3_karta_heatmap()], med fullständiga namn, t.ex. `df_value`, `basemap` och
#' `bins`.
#'
#' @details
#' De obligatoriska parametrarna i [func_d3_karta_heatmap()] måste finnas någonstans i
#' diagrammet: `df` anges i [d3_plot()], de övriga i det här lagret eller i ett annat
#' lager. Lägg till lagret på ett objekt som redan har en annan diagramtyp så byts typen;
#' parametrar som betyder samma sak i båda typerna (t.ex. `data_labels` och
#' `data_label_show`) får rätt namn automatiskt.
#'
#' @param ... Parametrar till [func_d3_karta_heatmap()] med fullständiga namn. Alla måste
#'   vara namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_heatmap.R
d3_heatmap    <- function(...) func_d3_lager(list(...), typ = "karta_heatmap")  # Täthetskarta (func_d3_karta_heatmap): df_value, basemap, group ...
#' Välj linjediagram i `+`-syntaxen
#'
#' Gör diagrammet till ett linjediagram i `+`-syntaxen. Lagret tar samma parametrar som
#' [func_d3_linjediagram()], med fullständiga namn, t.ex. `x_value`, `y_value` och
#' `group`.
#'
#' @details
#' De obligatoriska parametrarna i [func_d3_linjediagram()] måste finnas någonstans i
#' diagrammet: `df` anges i [d3_plot()], de övriga i det här lagret eller i ett annat
#' lager. Lägg till lagret på ett objekt som redan har en annan diagramtyp så byts typen;
#' parametrar som betyder samma sak i båda typerna (t.ex. `data_labels` och
#' `data_label_show`) får rätt namn automatiskt.
#'
#' @param ... Parametrar till [func_d3_linjediagram()] med fullständiga namn. Alla måste
#'   vara namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_line.R
d3_line       <- function(...) func_d3_lager(list(...), typ = "linjediagram")   # Linjediagram (func_d3_linjediagram): x_value, y_value, group ...
#' Välj stapeldiagram i `+`-syntaxen
#'
#' Gör diagrammet till ett stapeldiagram i `+`-syntaxen. Lagret tar samma parametrar som
#' [func_d3_stapeldiagram()], med fullständiga namn, t.ex. `x_value`, `y_value` och
#' `bar_type`.
#'
#' @details
#' De obligatoriska parametrarna i [func_d3_stapeldiagram()] måste finnas någonstans i
#' diagrammet: `df` anges i [d3_plot()], de övriga i det här lagret eller i ett annat
#' lager. Lägg till lagret på ett objekt som redan har en annan diagramtyp så byts typen;
#' parametrar som betyder samma sak i båda typerna (t.ex. `data_labels` och
#' `data_label_show`) får rätt namn automatiskt.
#'
#' @param ... Parametrar till [func_d3_stapeldiagram()] med fullständiga namn. Alla måste
#'   vara namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_bar.R
d3_bar        <- function(...) func_d3_lager(list(...), typ = "stapeldiagram")  # Stapeldiagram (func_d3_stapeldiagram): x_value, y_value, group, bar_type ...
#' Välj hanteldiagram i `+`-syntaxen
#'
#' Gör diagrammet till ett hanteldiagram i `+`-syntaxen. Lagret tar samma parametrar som
#' [func_d3_dumbbell()], med fullständiga namn, t.ex. `x_value`, `y_value` och `group`.
#'
#' @details
#' De obligatoriska parametrarna i [func_d3_dumbbell()] måste finnas någonstans i
#' diagrammet: `df` anges i [d3_plot()], de övriga i det här lagret eller i ett annat
#' lager. Lägg till lagret på ett objekt som redan har en annan diagramtyp så byts typen;
#' parametrar som betyder samma sak i båda typerna (t.ex. `data_labels` och
#' `data_label_show`) får rätt namn automatiskt.
#'
#' @param ... Parametrar till [func_d3_dumbbell()] med fullständiga namn. Alla måste vara
#'   namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_dumbbell.R
d3_dumbbell   <- function(...) func_d3_lager(list(...), typ = "dumbbell")       # Hantel-diagram (func_d3_dumbbell): x_value, y_value, group ...
#' Välj cirkel- eller munkdiagram i `+`-syntaxen
#'
#' Gör diagrammet till ett cirkeldiagram i `+`-syntaxen. Lagret tar samma parametrar som
#' [func_d3_cirkeldiagram()], med fullständiga namn, t.ex. `x_value`, `y_value` och
#' `pie_inner_radius`.
#'
#' @details
#' De obligatoriska parametrarna i [func_d3_cirkeldiagram()] måste finnas någonstans i
#' diagrammet: `df` anges i [d3_plot()], de övriga i det här lagret eller i ett annat
#' lager. Lägg till lagret på ett objekt som redan har en annan diagramtyp så byts typen;
#' parametrar som betyder samma sak i båda typerna (t.ex. `data_labels` och
#' `data_label_show`) får rätt namn automatiskt.
#'
#' @param ... Parametrar till [func_d3_cirkeldiagram()] med fullständiga namn. Alla måste
#'   vara namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_pie.R
d3_pie        <- function(...) func_d3_lager(list(...), typ = "cirkeldiagram")  # Cirkel-/munkdiagram (func_d3_cirkeldiagram): x_value, y_value ...
#' Välj spridningsdiagram i `+`-syntaxen
#'
#' Gör diagrammet till ett spridningsdiagram i `+`-syntaxen. Lagret tar samma parametrar
#' som [func_d3_spridningsdiagram()], med fullständiga namn, t.ex. `x_value`, `y_value`,
#' `group` och `trend_line`.
#'
#' @details
#' De obligatoriska parametrarna i [func_d3_spridningsdiagram()] måste finnas någonstans i
#' diagrammet: `df` anges i [d3_plot()], de övriga i det här lagret eller i ett annat
#' lager. Lägg till lagret på ett objekt som redan har en annan diagramtyp så byts typen;
#' parametrar som betyder samma sak i båda typerna (t.ex. `data_labels` och
#' `data_label_show`) får rätt namn automatiskt.
#'
#' @param ... Parametrar till [func_d3_spridningsdiagram()] med fullständiga namn. Alla
#'   måste vara namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_scatter.R
d3_scatter    <- function(...) func_d3_lager(list(...), typ = "spridningsdiagram")  # Spridningsdiagram (func_d3_spridningsdiagram): x_value, y_value, group ...
#' Välj histogram i `+`-syntaxen
#'
#' Gör diagrammet till ett histogram i `+`-syntaxen. Lagret tar samma parametrar som
#' [func_d3_histogram()], med fullständiga namn, t.ex. `x_value`, `bin_width` och
#' `group`.
#'
#' @details
#' De obligatoriska parametrarna i [func_d3_histogram()] måste finnas någonstans i
#' diagrammet: `df` anges i [d3_plot()], de övriga i det här lagret eller i ett annat
#' lager. Lägg till lagret på ett objekt som redan har en annan diagramtyp så byts typen;
#' parametrar som betyder samma sak i båda typerna (t.ex. `data_labels` och
#' `data_label_show`) får rätt namn automatiskt.
#'
#' @param ... Parametrar till [func_d3_histogram()] med fullständiga namn. Alla måste
#'   vara namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_histogram.R
d3_histogram  <- function(...) func_d3_lager(list(...), typ = "histogram")          # Histogram (func_d3_histogram): x_value, bin_width, group ...
#' Välj lådagram i `+`-syntaxen
#'
#' Gör diagrammet till ett lådagram (boxplot) i `+`-syntaxen. Lagret tar samma parametrar
#' som [func_d3_ladagram()], med fullständiga namn, t.ex. `x_value`, `y_value` och
#' `group`.
#'
#' @details
#' De obligatoriska parametrarna i [func_d3_ladagram()] måste finnas någonstans i
#' diagrammet: `df` anges i [d3_plot()], de övriga i det här lagret eller i ett annat
#' lager. Lägg till lagret på ett objekt som redan har en annan diagramtyp så byts typen;
#' parametrar som betyder samma sak i båda typerna (t.ex. `data_labels` och
#' `data_label_show`) får rätt namn automatiskt.
#'
#' @param ... Parametrar till [func_d3_ladagram()] med fullständiga namn. Alla måste
#'   vara namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_boxplot.R
d3_boxplot    <- function(...) func_d3_lager(list(...), typ = "ladagram")           # Lådagram (func_d3_ladagram): x_value, y_value, group ...
#' Välj ytdiagram i `+`-syntaxen
#'
#' Gör diagrammet till ett staplat ytdiagram i `+`-syntaxen. Lagret tar samma parametrar
#' som [func_d3_ytdiagram()], med fullständiga namn, t.ex. `x_value`, `y_value`, `group`
#' och `area_type`.
#'
#' @details
#' De obligatoriska parametrarna i [func_d3_ytdiagram()] måste finnas någonstans i
#' diagrammet: `df` anges i [d3_plot()], de övriga i det här lagret eller i ett annat
#' lager. Lägg till lagret på ett objekt som redan har en annan diagramtyp så byts typen;
#' parametrar som ytdiagrammet inte har (t.ex. `data_labels`) ger då ett fel.
#'
#' @param ... Parametrar till [func_d3_ytdiagram()] med fullständiga namn. Alla måste vara
#'   namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_area.R
d3_area       <- function(...) func_d3_lager(list(...), typ = "ytdiagram")          # Ytdiagram (func_d3_ytdiagram): x_value, y_value, group, area_type ...
#' Ställ in cirkel- och munkdiagram
#'
#' Sätter det som bara finns i cirkeldiagram (`pie_*`), t.ex. hålet som gör ett
#' munkdiagram, texten i mitten, sortering och sammanslagning av små bitar till "Övrigt".
#'
#' @details
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `inner_radius` | `pie_inner_radius` | cirkeldiagram |
#' | `sort` | `pie_sort` | cirkeldiagram |
#' | `start_angle` | `pie_start_angle` | cirkeldiagram |
#' | `pad_angle` | `pie_pad_angle` | cirkeldiagram |
#' | `corner_radius` | `pie_corner_radius` | cirkeldiagram |
#' | `border_color` | `pie_border_color` | cirkeldiagram |
#' | `border_size` | `pie_border_size` | cirkeldiagram |
#' | `center_text` | `pie_center_text` | cirkeldiagram |
#' | `center_label` | `pie_center_label` | cirkeldiagram |
#' | `other_threshold` | `pie_other_threshold` | cirkeldiagram |
#' | `other_label` | `pie_other_label` | cirkeldiagram |
#' | `other_color` | `pie_other_color` | cirkeldiagram |
#' | `explode` | `pie_explode` | cirkeldiagram |
#'
#' @param ... Inställningar i familjen `pie_*`, namngivna utan prefixet, t.ex. `position =
#'   "right"` för `pie_position`. Tabellen nedan visar vilka argument som finns. Ett
#'   argument som diagramtypen inte har ger ett fel.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_donut.R
d3_donut      <- function(...) func_d3_prefixa(list(...), "pie_")               # inner_radius, center_text, sort, other_threshold ... -> pie_*
#' Markera grupper eller kategorier
#'
#' Sätter parametrarna för markering (`highlight_*`): grupper eller kategorier som ska
#' framhävas med egen färg, tjockare linje eller färgad bakgrund.
#'
#' @details
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `groups` | `highlight_groups` | linjediagram, cirkeldiagram |
#' | `color` | `highlight_color` | linjediagram, stapeldiagram, cirkeldiagram |
#' | `line_size` | `highlight_line_size` | linjediagram |
#' | `bold` | `highlight_bold` | linjediagram, cirkeldiagram |
#' | `x_values` | `highlight_x_values` | stapeldiagram, hanteldiagram |
#' | `other_alpha` | `highlight_other_alpha` | stapeldiagram |
#' | `background` | `highlight_background` | hanteldiagram |
#' | `background_color` | `highlight_background_color` | hanteldiagram |
#' | `background_alpha` | `highlight_background_alpha` | hanteldiagram |
#' | `background_full_width` | `highlight_background_full_width` | hanteldiagram |
#' | `bar` | `highlight_bar` | hanteldiagram |
#' | `bar_color` | `highlight_bar_color` | hanteldiagram |
#' | `bar_width` | `highlight_bar_width` | hanteldiagram |
#' | `bar_alpha` | `highlight_bar_alpha` | hanteldiagram |
#'
#' @param ... Inställningar i familjen `highlight_*`, namngivna utan prefixet, t.ex.
#'   `position = "right"` för `highlight_position`. Tabellen nedan visar vilka argument
#'   som finns. Ett argument som diagramtypen inte har ger ett fel.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_highlight.R
d3_highlight  <- function(...) func_d3_prefixa(list(...), "highlight_")         # x_values, color, other_alpha (stapel) / groups, color (linje) -> highlight_*
#' Lägg till en prognos i ett linjediagram
#'
#' Ritar linjerna streckade från ett x-värde och lägger till ett skuggat osäkerhetsband
#' (parametrarna `line_forecast_*` och `line_band_*` i [func_d3_linjediagram()]). Bara de
#' argument som anges ändrar diagrammet.
#'
#' @param from x-värde där prognosen börjar (`line_forecast_from`). `NULL` (standard)
#'   ändrar inte inställningen.
#' @param label Text vid prognosens början (`line_forecast_label`). Anges `label = NULL`
#'   tas texten bort.
#' @param band_lower Kolumn med osäkerhetsbandets nedre gräns, som text
#'   (`line_band_lower`).
#' @param band_upper Kolumn med osäkerhetsbandets övre gräns, som text
#'   (`line_band_upper`).
#' @param band_alpha Bandets opacitet, 0–1 (`line_band_alpha`).
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_forecast.R
d3_forecast   <- function(from = NULL,
                          label = NULL,
                          band_lower = NULL,
                          band_upper = NULL,
                          band_alpha = NULL) {
  # Bara angivna argument skickas vidare — så att t.ex. label = NULL faktiskt tar bort texten
  namn <- c(from = "line_forecast_from", label = "line_forecast_label", band_lower = "line_band_lower",
            band_upper = "line_band_upper", band_alpha = "line_band_alpha")
  angivna <- names(as.list(match.call()))[-1]
  params <- stats::setNames(lapply(angivna, function(a) get(a)), namn[angivna])
  func_d3_lager(params)
}
#' Dela upp diagrammet i paneler
#'
#' Ritar ett diagram per värde i en kolumn (`facet_*`), t.ex. ett per kön.
#'
#' @details
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `var` | `facet_var` | linjediagram, ytdiagram, stapeldiagram, hanteldiagram, cirkeldiagram, spridningsdiagram, histogram, lådagram |
#' | `ncol` | `facet_ncol` | linjediagram, ytdiagram, stapeldiagram, hanteldiagram, cirkeldiagram, spridningsdiagram, histogram, lådagram |
#' | `same_y_scale` | `facet_same_y_scale` | linjediagram, ytdiagram, stapeldiagram, hanteldiagram, spridningsdiagram, histogram, lådagram |
#' | `title_size` | `facet_title_size` | linjediagram, ytdiagram, stapeldiagram, hanteldiagram, cirkeldiagram, spridningsdiagram, histogram, lådagram |
#' | `title_wrap` | `facet_title_wrap` | linjediagram, ytdiagram, stapeldiagram, cirkeldiagram, spridningsdiagram, histogram, lådagram |
#' | `scales` | `facet_scales` | stapeldiagram, spridningsdiagram, histogram, lådagram |
#'
#' @param ... Inställningar i familjen `facet_*`, namngivna utan prefixet, t.ex. `position
#'   = "right"` för `facet_position`. Tabellen nedan visar vilka argument som finns. Ett
#'   argument som diagramtypen inte har ger ett fel.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_facet.R
d3_facet      <- function(...) func_d3_prefixa(list(...), "facet_")             # var, ncol, same_y_scale, title_size -> facet_*

#' Lägg till rubriker och axeltitlar
#'
#' Sätter diagrammets texter: `title`, `subtitle`, `caption`, `legend_title`,
#' `x_axis_title` och `y_axis_title`. `NULL` tar bort en text som angetts tidigare.
#'
#' @details
#' Lagret tar egentligen vilka parametrar som helst med fullständiga namn, precis som
#' [d3_params()]; namnet visar vad det är tänkt för. Alla diagramtyper har `title`,
#' `subtitle`, `caption` och `legend_title`; axeltitlarna finns i linje-, yt-, stapel-,
#' hantel-, spridnings- och lådagram samt histogram.
#'
#' @param ... Texter med diagramfunktionens fullständiga parameternamn, t.ex. `title =
#'   "Arbetslöshet"`. Alla måste vara namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_labs.R
d3_labs        <- function(...) func_d3_lager(list(...))                          # title, subtitle, caption, legend_title, x_axis_title, y_axis_title
#' Ändra typsnitt och textstorlekar
#'
#' Sätter typsnittet (`font`) och textstorlekarna, t.ex. `text_scale`, `title_size` och
#' `legend_text_size`. `text_scale` skalar alla texter på en gång.
#'
#' @details
#' Lagret tar egentligen vilka parametrar som helst med fullständiga namn, precis som
#' [d3_params()]; namnet visar vad det är tänkt för. Vilka storleksparametrar som finns
#' skiljer sig mellan diagramtyperna; se diagramfunktionens hjälpsida. Stödlinjernas färg och
#' tjocklek (`grid_major_color`, `grid_minor_color`, `grid_major_width`,
#' `grid_minor_width`) sätts också med fördel här, i alla diagram med stödlinjer.
#'
#' @param ... Parametrar med diagramfunktionens fullständiga namn, t.ex. `font = "Arial"`
#'   eller `text_scale = 1.3`. Alla måste vara namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_theme.R
d3_theme       <- function(...) func_d3_lager(list(...))                          # font, title_size, subtitle_text_size, caption_size ...
#' Lägg till valfria parametrar med fullständiga namn
#'
#' Lägger till vilka parametrar som helst till diagramfunktionen, med deras fullständiga
#' namn. Använd det för parametrar som inte hör till något annat lager, t.ex.
#' `legend_direct` i linjediagram eller `show_points` i täthetskartor.
#'
#' @param ... Parametrar med diagramfunktionens fullständiga namn. Alla måste vara
#'   namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_params.R
d3_params      <- function(...) func_d3_lager(list(...))                          # Valfria parametrar med sina fullständiga namn
#' Ändra färger
#'
#' Sätter diagrammets färger med diagramfunktionens fullständiga parameternamn, t.ex.
#' `colors` (en färg per grupp), `color` och `color_negative` (kartornas färgskala) eller
#' `color_negative` i stapeldiagram.
#'
#' @details
#' Lagret tar egentligen vilka parametrar som helst med fullständiga namn, precis som
#' [d3_params()]; namnet visar vad det är tänkt för. Färgparametrar per diagramtyp:
#'
#' * koropletkarta: `colors`, `basemap_color`, `color`, `color_position`,
#'   `data_label_text_color`, `data_label_box_color`, `data_label_glow_color`,
#'   `data_label_line_color`, `overlay_color`, `bubble_color`, `bubble_line_color`,
#'   `color_low`, `color_negative`, `color_na`
#' * förändringskarta: `basemap_color`, `color`, `color_low`, `color_position`,
#'   `data_label_text_color`, `data_label_box_color`, `data_label_glow_color`,
#'   `data_label_line_color`, `overlay_color`, `bubble_color`, `bubble_line_color`,
#'   `color_negative`, `color_na`
#' * täthetskarta: `colors`, `basemap_color`, `low_color`, `high_color`, `point_color`,
#'   `point_colors`
#' * linjediagram: `colors`, `data_label_color`, `data_label_box_color`,
#'   `data_label_glow_color`, `highlight_color`, `group_highlight_label_color`,
#'   `group_highlight_unhighlighted_color`, `x_shade_color`
#' * ytdiagram: `colors`, `area_line_color`, `x_shade_color`
#' * stapeldiagram: `colors`, `color_palette`, `data_label_color`, `data_label_box_color`,
#'   `bar_line_color`, `color_negative`, `reference_color`, `highlight_color`,
#'   `data_label_inside_color`
#' * hanteldiagram: `line_color`, `baseline_color`, `colors`, `color_darkest`,
#'   `color_lightest`, `before_color`, `after_color`, `highlight_background_color`,
#'   `highlight_bar_color`, `data_label_color`, `data_label_box_color`,
#'   `data_label_glow_color`, `y_break_line_color`
#' * cirkeldiagram: `colors`, `pie_border_color`, `pie_other_color`, `data_label_color`,
#'   `data_label_box_color`, `data_label_glow_color`, `highlight_color`,
#'   `group_highlight_unhighlighted_color`
#' * spridningsdiagram: `colors`, `point_border_color`, `trend_line_color`,
#'   `data_label_color`, `data_label_box_color`
#' * histogram: `colors`, `bar_line_color`, `data_label_color`
#' * lådagram: `colors`, `box_line_color`, `data_label_color`
#' * alla diagram med stödlinjer (linje-, yt-, stapel-, hantel-, spridnings- och lådagram samt
#'   histogram): `grid_major_color`, `grid_minor_color`
#'
#' @param ... Färgparametrar med fullständiga namn, t.ex. `colors = c("Kvinnor" =
#'   "#1a9fb3", "Män" = "#ffc107")`. Alla måste vara namngivna.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_colors.R
d3_colors      <- function(...) func_d3_lager(list(...))                          # color, color_low, color_negative, color_position, colors ...
#' Ändra diagrammets storlek
#'
#' Sätter diagrammets bredd och höjd i pixlar (parametrarna `width` och `height`). Alla diagram
#' utom kartorna krymper för att få plats i fönstret.
#' Koropletkartan och förändringskartan ritas om i den bredd som finns (`responsive`),
#' så att texten behåller sin storlek; täthetskartan har den storlek som anges.
#'
#' @param width Bredd i pixlar. `NULL` (standard) ändrar inte bredden.
#' @param height Höjd i pixlar. `NULL` (standard) ändrar inte höjden.
#' @param responsive Bara koropletkartan och förändringskartan: `TRUE` ritar om kartan i
#'   den bredd som finns, `FALSE` ger fast storlek (se [func_d3_karta()]). `NULL` (standard)
#'   ändrar inte inställningen.
#' @param responsive_breakpoint Bara koropletkartan och förändringskartan: bredd i pixlar
#'   under vilken förklaringen läggs under kartan (se [func_d3_karta()]). `NULL` (standard)
#'   ändrar inte inställningen.
#' @param resize_min Bara koropletkartan och förändringskartan: minsta bredd och höjd i
#'   pixlar när kartan dras till ny storlek med `interactive_resize = TRUE` (se
#'   [func_d3_karta()]). `NULL` (standard) ändrar inte inställningen.
#' @param resize_max Bara koropletkartan och förändringskartan: största bredd och höjd i
#'   pixlar när kartan dras till ny storlek (se [func_d3_karta()]). `NULL` (standard) ändrar
#'   inte inställningen.
#' @param resize_keep_scale Bara koropletkartan och förändringskartan: `TRUE` behåller
#'   kartans skala när rutan dras till ny storlek (ett annat utsnitt visas), `FALSE` ritar om
#'   kartan i den nya storleken (se [func_d3_karta()]). `NULL` (standard) ändrar inte
#'   inställningen.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_size.R
d3_size        <- function(width = NULL, height = NULL, responsive = NULL,
                           responsive_breakpoint = NULL, resize_min = NULL,
                           resize_max = NULL, resize_keep_scale = NULL) {         # Diagrammets storlek i pixlar
  func_d3_lager(Filter(Negate(is.null), list(width = width, height = height, responsive = responsive,
                                             responsive_breakpoint = responsive_breakpoint,
                                             resize_min = resize_min, resize_max = resize_max,
                                             resize_keep_scale = resize_keep_scale)))
}
#' Ställ in dataetiketterna
#'
#' Sätter parametrarna för dataetiketter (`data_label_*`), t.ex. om de ska visas, deras
#' storlek, färg, antal decimaler och vilka värden som får etikett.
#'
#' @details
#' `show = TRUE` slår på etiketterna i alla diagramtyper: i kartorna heter parametern
#' `data_label_show` och i övriga `data_labels`, och lagret väljer rätt namn.
#'
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `show` | `data_label_show` | koropletkarta, förändringskarta |
#' | `text_size` | `data_label_text_size` | koropletkarta, förändringskarta, linjediagram, stapeldiagram, hanteldiagram, cirkeldiagram, histogram, spridningsdiagram, lådagram |
#' | `text_color` | `data_label_text_color` | koropletkarta, förändringskarta |
#' | `box` | `data_label_box` | koropletkarta, förändringskarta, linjediagram, stapeldiagram, hanteldiagram, cirkeldiagram, spridningsdiagram |
#' | `box_color` | `data_label_box_color` | koropletkarta, förändringskarta, linjediagram, stapeldiagram, hanteldiagram, cirkeldiagram, spridningsdiagram |
#' | `box_alpha` | `data_label_box_alpha` | koropletkarta, förändringskarta, linjediagram, hanteldiagram, cirkeldiagram, spridningsdiagram |
#' | `decimals` | `data_label_decimals` | koropletkarta, förändringskarta, linjediagram, stapeldiagram, cirkeldiagram, histogram, spridningsdiagram, lådagram |
#' | `decimal` | `data_label_decimal` | koropletkarta |
#' | `glow` | `data_label_glow` | koropletkarta, förändringskarta, linjediagram, hanteldiagram, cirkeldiagram |
#' | `glow_color` | `data_label_glow_color` | koropletkarta, förändringskarta, linjediagram, hanteldiagram, cirkeldiagram |
#' | `glow_sigma` | `data_label_glow_sigma` | koropletkarta, förändringskarta, linjediagram, hanteldiagram, cirkeldiagram |
#' | `glow_expand` | `data_label_glow_expand` | koropletkarta, förändringskarta, linjediagram, hanteldiagram, cirkeldiagram |
#' | `column` | `data_label_column` | koropletkarta, förändringskarta, spridningsdiagram, stapeldiagram |
#' | `show_value` | `data_label_show_value` | koropletkarta, förändringskarta |
#' | `separator` | `data_label_separator` | koropletkarta, förändringskarta |
#' | `repel` | `data_label_repel` | koropletkarta, förändringskarta, linjediagram, stapeldiagram, hanteldiagram, cirkeldiagram, spridningsdiagram |
#' | `force` | `data_label_force` | koropletkarta, förändringskarta, stapeldiagram |
#' | `max_overlaps` | `data_label_max_overlaps` | koropletkarta, förändringskarta, stapeldiagram, spridningsdiagram |
#' | `nudge_x` | `data_label_nudge_x` | koropletkarta, förändringskarta, linjediagram, stapeldiagram, hanteldiagram |
#' | `nudge_y` | `data_label_nudge_y` | koropletkarta, förändringskarta, linjediagram, stapeldiagram, hanteldiagram |
#' | `line` | `data_label_line` | koropletkarta, förändringskarta |
#' | `line_color` | `data_label_line_color` | koropletkarta, förändringskarta |
#' | `line_size` | `data_label_line_size` | koropletkarta, förändringskarta |
#' | `outside_map` | `data_label_outside_map` | koropletkarta, förändringskarta |
#' | `expand_factor` | `data_label_expand_factor` | koropletkarta, förändringskarta |
#' | `filter` | `data_label_filter` | koropletkarta, förändringskarta, linjediagram, stapeldiagram, cirkeldiagram, spridningsdiagram |
#' | `filter_column` | `data_label_filter_column` | koropletkarta, förändringskarta |
#' | `thousands_mark` | `data_label_thousands_mark` | koropletkarta, förändringskarta |
#' | `thousands_sep` | `data_label_thousands_sep` | förändringskarta |
#' | `zoom` | `data_label_zoom` | koropletkarta, förändringskarta |
#' | `zoom_overlap` | `data_label_zoom_overlap` | koropletkarta, förändringskarta |
#' | `color` | `data_label_color` | linjediagram, stapeldiagram, hanteldiagram, cirkeldiagram, histogram, spridningsdiagram, lådagram |
#' | `format` | `data_label_format` | linjediagram, stapeldiagram, cirkeldiagram, spridningsdiagram |
#' | `x_values` | `data_label_x_values` | linjediagram, stapeldiagram, hanteldiagram, cirkeldiagram |
#' | `y_values` | `data_label_y_values` | linjediagram, stapeldiagram, hanteldiagram, cirkeldiagram |
#' | `first_last` | `data_label_first_last` | linjediagram, stapeldiagram |
#' | `skip` | `data_label_skip` | linjediagram, stapeldiagram, hanteldiagram |
#' | `direction` | `data_label_direction` | linjediagram |
#' | `show_group` | `data_label_show_group` | linjediagram, stapeldiagram, cirkeldiagram |
#' | `group_separator` | `data_label_group_separator` | linjediagram, stapeldiagram, cirkeldiagram |
#' | `position` | `data_label_position` | stapeldiagram, hanteldiagram, cirkeldiagram |
#' | `size` | `data_label_size` | stapeldiagram |
#' | `accuracy` | `data_label_accuracy` | stapeldiagram |
#' | `width` | `data_label_width` | stapeldiagram, hanteldiagram |
#' | `group_values` | `data_label_group_values` | stapeldiagram |
#' | `side` | `data_label_side` | hanteldiagram |
#' | `axis_padding` | `data_label_axis_padding` | hanteldiagram |
#' | `angle` | `data_label_angle` | hanteldiagram |
#' | `only_first_last` | `data_label_only_first_last` | hanteldiagram |
#' | `min_share` | `data_label_min_share` | cirkeldiagram |
#' | `min_value` | `data_label_min_value` | cirkeldiagram |
#' | `overflow` | `data_label_overflow` | stapeldiagram |
#' | `inside_color` | `data_label_inside_color` | stapeldiagram |
#'
#' @param ... Inställningar i familjen `data_label_*`, namngivna utan prefixet, t.ex.
#'   `position = "right"` för `data_label_position`. Tabellen nedan visar vilka argument
#'   som finns. Ett argument som diagramtypen inte har ger ett fel.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_labels.R
d3_labels      <- function(...) func_d3_prefixa(list(...), "data_label_")         # show, text_size, box, repel, filter ... -> data_label_*
#' Ställ in förklaringen
#'
#' Sätter parametrarna för förklaringen (`legend_*`), t.ex. placering, rubrik och
#' textstorlek.
#'
#' @details
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `title` | `legend_title` | alla |
#' | `name` | `legend_name` | koropletkarta, förändringskarta, täthetskarta |
#' | `position` | `legend_position` | alla |
#' | `key_width` | `legend_key_width` | koropletkarta, förändringskarta |
#' | `key_height` | `legend_key_height` | koropletkarta, förändringskarta |
#' | `text_size` | `legend_text_size` | alla |
#' | `key_shape` | `legend_key_shape` | koropletkarta, förändringskarta, täthetskarta |
#' | `title_change` | `legend_title_change` | koropletkarta |
#' | `title_level` | `legend_title_level` | förändringskarta |
#' | `breaks` | `legend_breaks` | täthetskarta |
#' | `format` | `legend_format` | täthetskarta |
#' | `unit` | `legend_unit` | täthetskarta |
#' | `direct` | `legend_direct` | linjediagram |
#' | `title_size` | `legend_title_size` | stapeldiagram, hanteldiagram |
#' | `reverse` | `legend_reverse` | linjediagram, ytdiagram, stapeldiagram |
#'
#' @param ... Inställningar i familjen `legend_*`, namngivna utan prefixet, t.ex.
#'   `position = "right"` för `legend_position`. Tabellen nedan visar vilka argument som
#'   finns. Ett argument som diagramtypen inte har ger ett fel.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_legend.R
d3_legend      <- function(...) func_d3_prefixa(list(...), "legend_")             # position, title, key_shape ... -> legend_*
#' Ställ in klassindelningen i en karta
#'
#' Sätter klassindelningen av färgskalan i koropletkartor och förändringskartor
#' (`breaks_*`): metod, antal klasser, egna gränser och decimaler i klassgränserna.
#'
#' @details
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `method` | `breaks_method` | koropletkarta, förändringskarta |
#' | `custom` | `breaks_custom` | koropletkarta, förändringskarta |
#' | `num_group` | `breaks_num_group` | koropletkarta, förändringskarta |
#' | `decimals` | `breaks_decimals` | koropletkarta, förändringskarta |
#' | `round_labels` | `breaks_round_labels` | koropletkarta, förändringskarta |
#' | `custom_change` | `breaks_custom_change` | koropletkarta |
#'
#' @param ... Inställningar i familjen `breaks_*`, namngivna utan prefixet, t.ex.
#'   `position = "right"` för `breaks_position`. Tabellen nedan visar vilka argument som
#'   finns. Ett argument som diagramtypen inte har ger ett fel.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_breaks.R
d3_breaks      <- function(...) func_d3_prefixa(list(...), "breaks_")             # method, custom, num_group, decimals -> breaks_*
#' Ställ in tooltipen
#'
#' Sätter parametrarna för tooltipen (`tooltip_*`), rutan som visas när muspekaren hålls
#' över diagrammet: t.ex. enhet, decimaler och extra kolumner.
#'
#' @details
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `show` | `tooltip_show` | alla |
#' | `name_column` | `tooltip_name_column` | koropletkarta, förändringskarta, täthetskarta |
#' | `columns` | `tooltip_columns` | koropletkarta, förändringskarta, täthetskarta, linjediagram, ytdiagram, stapeldiagram, hanteldiagram, spridningsdiagram |
#' | `rank` | `tooltip_rank` | koropletkarta, förändringskarta |
#' | `rank_order` | `tooltip_rank_order` | koropletkarta, förändringskarta |
#' | `class` | `tooltip_class` | koropletkarta, förändringskarta |
#' | `value_label` | `tooltip_value_label` | koropletkarta, förändringskarta |
#' | `unit` | `tooltip_unit` | alla |
#' | `unit_change` | `tooltip_unit_change` | koropletkarta, förändringskarta, hanteldiagram |
#' | `decimals` | `tooltip_decimals` | alla |
#' | `mode` | `tooltip_mode` | linjediagram |
#' | `change_percent` | `tooltip_change_percent` | hanteldiagram |
#' | `x_format` | `tooltip_x_format` | linjediagram, ytdiagram, stapeldiagram |
#'
#' @param ... Inställningar i familjen `tooltip_*`, namngivna utan prefixet, t.ex.
#'   `position = "right"` för `tooltip_position`. Tabellen nedan visar vilka argument som
#'   finns. Ett argument som diagramtypen inte har ger ett fel.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_tooltip.R
d3_tooltip     <- function(...) func_d3_prefixa(list(...), "tooltip_")            # show, columns, rank ... -> tooltip_*
#' Slå på och av interaktiva funktioner
#'
#' Sätter parametrarna för diagrammets interaktiva funktioner (`interactive_*`), t.ex.
#' zoom, sökruta, klickbar förklaring, knappar och rullistor med uppspelning.
#'
#' @details
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `zoom` | `interactive_zoom` | koropletkarta, förändringskarta, täthetskarta, linjediagram, ytdiagram, spridningsdiagram |
#' | `zoom_group_column` | `interactive_zoom_group_column` | koropletkarta, förändringskarta |
#' | `search` | `interactive_search` | koropletkarta, förändringskarta, spridningsdiagram |
#' | `legend` | `interactive_legend` | alla |
#' | `export` | `interactive_export` | alla |
#' | `mode_toggle` | `interactive_mode_toggle` | koropletkarta, förändringskarta |
#' | `mode_labels` | `interactive_mode_labels` | koropletkarta, förändringskarta |
#' | `time_slider` | `interactive_time_slider` | koropletkarta, förändringskarta |
#' | `time_range` | `interactive_time_range` | koropletkarta, förändringskarta |
#' | `resize` | `interactive_resize` | koropletkarta, förändringskarta |
#' | `layer_toggle` | `interactive_layer_toggle` | täthetskarta |
#' | `group_density` | `interactive_group_density` | täthetskarta |
#' | `adjust_values` | `interactive_adjust_values` | täthetskarta |
#' | `index_toggle` | `interactive_index_toggle` | linjediagram |
#' | `index_base` | `interactive_index_base` | linjediagram |
#' | `index_labels` | `interactive_index_labels` | linjediagram |
#' | `bar_type_toggle` | `interactive_bar_type_toggle` | stapeldiagram, histogram |
#' | `y_toggle` | `interactive_y_toggle` | histogram |
#' | `sort` | `interactive_sort` | stapeldiagram, hanteldiagram, lådagram |
#' | `filter_column` | `interactive_filter_column` | stapeldiagram, cirkeldiagram, spridningsdiagram, histogram, lådagram |
#' | `filter_default` | `interactive_filter_default` | stapeldiagram, cirkeldiagram, spridningsdiagram, histogram, lådagram |
#' | `filter_same_y` | `interactive_filter_same_y` | stapeldiagram, histogram, lådagram |
#'
#' @param ... Inställningar i familjen `interactive_*`, namngivna utan prefixet, t.ex.
#'   `position = "right"` för `interactive_position`. Tabellen nedan visar vilka argument
#'   som finns. Ett argument som diagramtypen inte har ger ett fel.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_interactive.R
d3_interactive <- function(...) func_d3_prefixa(list(...), "interactive_")        # zoom, search, export ... -> interactive_*
#' Ställ in animationerna
#'
#' Sätter animationernas längd och uppspelningshastighet (`animation_*`). `duration = 0`
#' stänger av animationerna.
#'
#' @details
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `duration` | `animation_duration` | alla |
#' | `play_interval` | `animation_play_interval` | koropletkarta, förändringskarta, täthetskarta, stapeldiagram, cirkeldiagram, spridningsdiagram, histogram, lådagram |
#' | `draw` | `animation_draw` | linjediagram, ytdiagram, hanteldiagram, cirkeldiagram |
#' | `grow` | `animation_grow` | stapeldiagram, spridningsdiagram, histogram, lådagram |
#'
#' @param ... Inställningar i familjen `animation_*`, namngivna utan prefixet, t.ex.
#'   `position = "right"` för `animation_position`. Tabellen nedan visar vilka argument
#'   som finns. Ett argument som diagramtypen inte har ger ett fel.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_animation.R
d3_animation   <- function(...) func_d3_prefixa(list(...), "animation_")          # duration, play_interval -> animation_*
#' Ändra kartans marginaler
#'
#' Sätter marginalerna runt kartan i punkter (`margin_*`). Bara kartorna har marginaler.
#'
#' @details
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `top` | `margin_top` | koropletkarta, förändringskarta, täthetskarta |
#' | `right` | `margin_right` | koropletkarta, förändringskarta, täthetskarta |
#' | `bottom` | `margin_bottom` | koropletkarta, förändringskarta, täthetskarta |
#' | `left` | `margin_left` | koropletkarta, förändringskarta, täthetskarta |
#'
#' @param ... Inställningar i familjen `margin_*`, namngivna utan prefixet, t.ex.
#'   `position = "right"` för `margin_position`. Tabellen nedan visar vilka argument som
#'   finns. Ett argument som diagramtypen inte har ger ett fel.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_margin.R
d3_margin      <- function(...) func_d3_prefixa(list(...), "margin_")             # top, right, bottom, left -> margin_* (bara kartorna har marginaler)
#' Lägg till ett tidsreglage i en karta
#'
#' Sätter parametrarna för tidsreglaget i kartorna (`time_*`): kolumnen med år, vilket år
#' som visas först och om året ska stå i kartans hörn.
#'
#' @details
#' I förändringskartan heter kolumnen `year_column`; `column` byts automatiskt till det
#' namnet. I stapel- och cirkeldiagram byts den till `interactive_filter_column`, som ger
#' en rullista.
#'
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `column` | `time_column` | koropletkarta, täthetskarta |
#' | `default` | `time_default` | koropletkarta, förändringskarta, täthetskarta |
#' | `label_show` | `time_label_show` | koropletkarta, förändringskarta, täthetskarta |
#'
#' @param ... Inställningar i familjen `time_*`, namngivna utan prefixet, t.ex. `position
#'   = "right"` för `time_position`. Tabellen nedan visar vilka argument som finns. Ett
#'   argument som diagramtypen inte har ger ett fel.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_time.R
d3_time        <- function(...) func_d3_prefixa(list(...), "time_")               # column, label_show ... -> time_*
#' Lägg till bubblor i en karta
#'
#' Ritar en cirkel i varje område med en storlek som motsvarar ett värde, t.ex. befolkning
#' (`bubble_*`).
#'
#' @details
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `column` | `bubble_column` | koropletkarta, förändringskarta |
#' | `color` | `bubble_color` | koropletkarta, förändringskarta |
#' | `alpha` | `bubble_alpha` | koropletkarta, förändringskarta |
#' | `max_size` | `bubble_max_size` | koropletkarta, förändringskarta |
#' | `line_color` | `bubble_line_color` | koropletkarta, förändringskarta |
#'
#' @param ... Inställningar i familjen `bubble_*`, namngivna utan prefixet, t.ex.
#'   `position = "right"` för `bubble_position`. Tabellen nedan visar vilka argument som
#'   finns. Ett argument som diagramtypen inte har ger ett fel.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_bubbles.R
d3_bubbles     <- function(...) func_d3_prefixa(list(...), "bubble_")             # column, color, max_size ... -> bubble_*
#' Lägg till en bakgrundskarta
#'
#' Lägger en bakgrundskarta, t.ex. länsgränsen, under data och sätter dess färger och
#' utsnitt (`basemap` och `basemap_*`).
#'
#' @details
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `color` | `basemap_color` | koropletkarta, förändringskarta, täthetskarta |
#' | `color_line` | `basemap_color_line` | koropletkarta, förändringskarta, täthetskarta |
#' | `fit` | `basemap_fit` | koropletkarta, förändringskarta |
#'
#' @param basemap `sf`-objekt som bakgrundskarta (parametern `basemap`). Obligatorisk.
#' @param ... Inställningar i familjen `basemap_*`, namngivna utan prefixet, t.ex. `color
#'   = "#eeeeee"` för `basemap_color`.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_basemap.R
d3_basemap     <- function(basemap, ...) {                                        # basemap-objekt + color, color_line, fit -> basemap_*
  lager <- func_d3_prefixa(list(...), "basemap_")
  lager$params <- c(list(basemap = basemap), lager$params)
  lager
}
#' Lägg till konturer ovanpå en karta
#'
#' Ritar gränserna för andra områden ovanpå data, t.ex. kommunalförbund, med namn om så
#' önskas (`overlay` och `overlay_*`).
#'
#' @details
#' Argument och var de finns (vad parametrarna gör står på diagramfunktionernas
#' hjälpsidor, t.ex. [func_d3_karta()]):
#'
#' | Argument | Parameter | Finns i |
#' |---|---|---|
#' | `color` | `overlay_color` | koropletkarta, förändringskarta |
#' | `line_size` | `overlay_line_size` | koropletkarta, förändringskarta |
#' | `label_column` | `overlay_label_column` | koropletkarta, förändringskarta |
#' | `label_size` | `overlay_label_size` | koropletkarta, förändringskarta |
#'
#' @param overlay `sf`-objekt med områdena (parametern `overlay`). Obligatorisk.
#' @param ... Inställningar i familjen `overlay_*`, namngivna utan prefixet, t.ex.
#'   `label_column = "namn"` för `overlay_label_column`.
#'
#' @return Ett lager av klassen `vgr_d3_lager`, som läggs till ett diagram med `+`.
#' @seealso [d3_plot()] beskriver `+`-syntaxen.
#' @export
#' @example inst/examples/d3_overlay.R
d3_overlay     <- function(overlay, ...) {                                        # t.ex. kommunalförbund + color, line_size, label_column -> overlay_*
  lager <- func_d3_prefixa(list(...), "overlay_")
  lager$params <- c(list(overlay = overlay), lager$params)
  lager
}

#' @export
#' @noRd
`+.vgr_d3` <- function(e1, e2) {
  if (!inherits(e2, "vgr_d3_lager"))
    stop("vgr_D3: bara d3_*-lager kan läggas till med +, t.ex. + d3_labs(title = \"...\").")
  if (!is.null(e2$typ)) {
    e1$typ     <- e2$typ
    e1$giltiga <- func_d3_giltiga(e2$typ)
    e1$params  <- func_d3_anpassa_namn(e1$params, e1$giltiga)
  }
  nya <- func_d3_anpassa_namn(e2$params, e1$giltiga)
  if (!is.null(e1$typ)) func_d3_kontrollera_namn(nya, e1$giltiga, e1$typ)
  # Ett huvudnamn i lagret ersätter ett tidigare alias (legend_name, title_text_size ...)
  for (huvud in intersect(names(nya), names(func_d3_parameteralias))) {
    alias <- setdiff(func_d3_parameteralias[[huvud]], names(nya))
    e1$params <- e1$params[setdiff(names(e1$params), alias)]
  }
  # [<- med en lista behåller NULL-värden (t.ex. title = NULL tar bort en titel)
  if (length(nya) > 0) e1$params[names(nya)] <- nya
  e1
}
