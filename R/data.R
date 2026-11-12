#' Kommuner i Västra Götaland
#'
#' Gränser för Västra Götalands 49 kommuner som `sf`-objekt i SWEREF 99 TM
#' (EPSG:3006). Gränserna är generaliserade och passar för översiktskartor och
#' exempel, t.ex. med [func_d3_karta()], [func_d3_karta_change()] och
#' [func_d3_karta_heatmap()]. Samma dataset finns i funcVGR.
#'
#' @format Ett `sf`-objekt med 49 rader och 3 kolumner:
#' \describe{
#'   \item{kommunkod}{Kommunkod, fyra tecken, t.ex. `"1480"`.}
#'   \item{kommun}{Kommunens namn, t.ex. `"Göteborg"`.}
#'   \item{geometry}{Kommunens yta (`MULTIPOLYGON`/`POLYGON`).}
#' }
#' @source SCB:s generaliserade kommungränser (öppna data, CC0). Se
#'   `data-raw/vg_kommuner.R` i paketets källkod.
#' @examples
#' # Kommunernas koder och namn
#' head(sf::st_drop_geometry(vg_kommuner))
#'
#' # Gränserna som en enkel karta
#' plot(sf::st_geometry(vg_kommuner))
"vg_kommuner"
