#' funcVGRD3: Interaktiva D3-diagram och kartor i VGR:s grafiska profil
#'
#' Paketet samlar funktionerna som tidigare låg som fristående R-skript i
#' katalogen `vgr_D3`. Diagrammen ritas med D3.js och har samma parametrar som
#' `func_graf_*`-funktionerna i funcVGR, plus interaktiva tillägg. Funktionerna
#' är grupperade enligt nedan.
#'
#' @section Kartor:
#' * [func_d3_karta()] – koropletkarta, med tidsreglage och växling nivå/förändring
#' * [func_d3_karta_change()] – karta över förändring mellan två år
#' * [func_d3_karta_heatmap()] – täthetskarta för punkter
#'
#' @section Diagram:
#' * [func_d3_linjediagram()] – linjediagram
#' * [func_d3_stapeldiagram()] (även `func_d3_barchart()`) – stapeldiagram
#' * [func_d3_dumbbell()] – hanteldiagram (två perioder per kategori)
#' * [func_d3_cirkeldiagram()] – cirkel- och munkdiagram
#'
#' @section Bygga, visa och spara:
#' * [d3_plot()] och lagren `d3_*()` – bygg ett diagram lager för lager med `+`
#' * [func_d3_spara()] – spara som HTML, PNG eller SVG, och [func_d3_oppna()] – öppna
#'   den sparade filen
#' * [func_d3_hjalp()] – lista funktionerna
#'
#' @section Data:
#' * [vg_kommuner] – Västra Götalands 49 kommuner som `sf`-objekt, för exempel och
#'   översiktskartor
#'
#' @keywords internal
#' @import dplyr
#' @importFrom ggplot2 aes after_stat expand_limits ggplot layer_data stat_density_2d
#' @importFrom rlang %||%
#' @importFrom stringr str_wrap
#' @importFrom tidyr pivot_wider
#' @importFrom utils head
"_PACKAGE"
