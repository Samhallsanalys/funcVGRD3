# Skapar data/vg_kommuner.rda: Västra Götalands 49 kommuner som sf-objekt.
#
# Samma dataset som vg_kommuner i funcVGR (data/vg_kommuner.rda kopierades därifrån i
# version 0.3.0), så att funcVGRD3 inte behöver funcVGR. Skriptet nedan är detsamma som
# funcVGR:s och används om gränserna ska uppdateras.
#
# Källa: SCB:s generaliserade kommungränser (Kommun_Sweref99TM_region.shp), som
# redan används lokalt i flera projekt, t.ex. H:/R/industri/karta/Shape-svenska/.
# SCB publicerar kommungränserna som öppna data (CC0).
#
# Körs från paketets rot: source("data-raw/vg_kommuner.R")

library(sf)
library(dplyr)

fil <- "H:/R/industri/karta/Shape-svenska/KommunSweref99TM/Kommun_Sweref99TM_region.shp"

vg_kommuner <- st_read(fil, quiet = TRUE) |>
  filter(substr(KnKod, 1, 2) == "14") |>
  transmute(kommunkod = KnKod, kommun = KnNamn) |>
  arrange(kommunkod) |>
  st_make_valid()

# Plain data frame columns and a geometry column named "geometry", in SWEREF 99 TM
st_geometry(vg_kommuner) <- "geometry"
vg_kommuner <- st_transform(vg_kommuner, 3006)

stopifnot(nrow(vg_kommuner) == 49, all(st_is_valid(vg_kommuner)))

usethis::use_data(vg_kommuner, overwrite = TRUE, compress = "xz")
