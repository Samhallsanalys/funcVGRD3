# Gemensam kärna för alla func_d3_*-diagram: plot-objektet (klass "vgr_d3"), rendering till HTML
# och små hjälpfunktioner som delas av diagramtyperna (enheter, färger, typsnitt, GeoJSON,
# kartornas kompakta kodning).
#
# Arbetsflöde: alla func_d3_<typ>()-funktioner returnerar ett vgr_d3-objekt som bara sparar de
# argument du angav. Först när objektet skrivs ut (eller sparas med func_d3_spara()) anropas
# funktionen igen med .rendera = TRUE och alla beräkningar görs. Därför fungerar "+"-lager även
# på objekt som skapats med de vanliga funktionsanropen.

# ============================================================
#  SÖKVÄG OCH BEROENDEN
# ============================================================

# Returnerar paketets katalog. JavaScript-filerna ligger i inst/js och installeras i js/.
func_d3_sokvag <- function() {
  system.file(package = "funcVGRD3", mustWork = TRUE)
}

# JavaScript-beroenden: D3, den delade kärnan och diagramtypens egen fil.
# all_files = FALSE gör att bara de listade filerna kopieras när sidan byggs.
func_d3_beroenden <- function(js_fil) {
  sokvag <- func_d3_sokvag()
  list(
    htmltools::htmlDependency("d3", "7.9.0", src = c(file = file.path(sokvag, "js", "lib")),
                              script = "d3.v7.min.js", all_files = FALSE),
    htmltools::htmlDependency("vgr-d3-core", "0.1.0", src = c(file = file.path(sokvag, "js")),
                              script = "vgr_core.js", all_files = FALSE),
    htmltools::htmlDependency(paste0("vgr-d3-", js_fil), "0.1.0", src = c(file = file.path(sokvag, "js")),
                              script = paste0(js_fil, ".js"), all_files = FALSE)
  )
}

# ============================================================
#  PLOT-OBJEKT OCH "+"-GRAMMATIK
# ============================================================

# Vilken R-funktion som bygger respektive diagramtyp
func_d3_typ_funktion <- function(typ) {
  fn_namn <- paste0("func_d3_", typ)
  if (!exists(fn_namn, mode = "function")) stop("vgr_D3: okänd diagramtyp '", typ, "' (funktionen ", fn_namn, " saknas).")
  get(fn_namn, mode = "function")
}

# Giltiga parameternamn för en diagramtyp (funktionens argument utom det interna .rendera)
func_d3_giltiga <- function(typ) setdiff(names(formals(func_d3_typ_funktion(typ))), ".rendera")

# Räknare för unika element-id per R-session
.vgr_d3_tillstand <- new.env()
.vgr_d3_tillstand$n <- 0L

# NA i en grupp-/facettkolumn blir kategorin "NA" sist (som ggplot2 visar NA i legend och facetter).
# Returnerar en faktor; teckenvariabler sorteras som ggplot2 gör (alfabetiskt).
func_d3_na_som_niva <- function(x) {
  # setdiff(NA): en uttrycklig NA-nivå (addNA(), fct_na_value_to_level()) blir samma "NA"-nivå som vanliga NA
  niva <- if (is.factor(x)) setdiff(levels(droplevels(x)), NA) else sort(unique(as.character(x[!is.na(x)])))
  x <- as.character(x)
  har_na <- anyNA(x)
  x[is.na(x)] <- "NA"
  factor(x, levels = unique(c(setdiff(niva, "NA"), if (har_na || "NA" %in% niva) "NA")))
}

func_d3_nytt_objekt <- function(typ, params) {
  structure(list(typ = typ, params = params, giltiga = if (!is.null(typ)) func_d3_giltiga(typ) else NULL),
            class = "vgr_d3")
}

# Ett lager är bara en namngiven parameterlista (och ev. en diagramtyp) som "+" lägger på objektet
func_d3_lager <- function(params, typ = NULL) {
  if (length(params) > 0 && (is.null(names(params)) || any(names(params) == "")))
    stop("vgr_D3: alla argument till d3_*-lager måste vara namngivna, t.ex. d3_labs(title = \"...\").")
  structure(list(params = params, typ = typ), class = "vgr_d3_lager")
}

# Lägger till ett prefix på alla argumentnamn, t.ex. d3_legend(position = "right") -> legend_position
func_d3_prefixa <- function(params, prefix, undantag = character(0)) {
  if (length(params) == 0) return(func_d3_lager(params))
  if (is.null(names(params)) || any(names(params) == ""))
    stop("vgr_D3: alla argument till d3_*-lager måste vara namngivna.")
  byt <- !names(params) %in% undantag
  names(params)[byt] <- paste0(prefix, names(params)[byt])
  func_d3_lager(params)
}

# Namn som skiljer sig mellan diagramtyper men betyder samma sak i grammatiken.
# Varje namn får en eller flera kandidater; den första som diagramtypen har används.
func_d3_namnalias <- list(
  data_label_show            = "data_labels",                       # karta: data_label_show | linje/stapel/dumbbell/cirkel: data_labels
  data_labels                = "data_label_show",
  time_column                = c("year_column", "interactive_filter_column"),  # karta/heatmap | karta_change | stapel/cirkel
  year_column                = c("time_column", "interactive_filter_column"),
  interactive_filter_column  = c("time_column", "year_column"),
  time_default               = "interactive_filter_default",
  interactive_filter_default = "time_default",
  subtitle_size              = "subtitle_text_size",                # linje/stapel/cirkel | kartor/dumbbell
  subtitle_text_size         = "subtitle_size",
  data_label_color           = "data_label_text_color",             # linje/stapel/dumbbell/cirkel | kartor
  data_label_text_color      = "data_label_color",
  data_label_first_last      = "data_label_only_first_last",        # linje/stapel | dumbbell
  data_label_only_first_last = "data_label_first_last",
  animation_draw             = "animation_grow",                    # linje/yt/dumbbell/cirkel | stapel
  animation_grow             = "animation_draw",
  bar_type                   = "area_type",                         # stapel | yt ("stacked"/"filled")
  area_type                  = "bar_type",
  bar_line_color             = "area_line_color",                   # stapel | yt (kantlinjen)
  area_line_color            = "bar_line_color",
  bar_line_width             = "area_line_width",
  area_line_width            = "bar_line_width"
)

# Byter ut alias mot det namn diagramtypen faktiskt använder
func_d3_anpassa_namn <- function(params, giltiga) {
  if (is.null(giltiga) || length(params) == 0) return(params)
  n <- names(params)
  for (i in seq_along(n)) {
    if (!n[i] %in% giltiga && n[i] %in% names(func_d3_namnalias)) {
      kandidat <- intersect(func_d3_namnalias[[n[i]]], giltiga)
      if (length(kandidat) > 0) n[i] <- kandidat[1]
    }
  }
  names(params) <- n
  params
}

# Äldre alias (för bakåtkompatibilitet) vinner i funktionerna via missing(). När ett "+"-lager sätter
# huvudnamnet ska det gälla, så aliasen tas bort ur objektet (annars vinner ett tidigare alias).
func_d3_parameteralias <- list(
  title_size          = "title_text_size",
  subtitle_size       = "subtitle_text_size",
  caption_size        = "caption_text_size",
  legend_title        = "legend_name",
  data_label_decimals = c("data_label_decimal", "data_label_accuracy"),
  data_label_size     = "data_label_text_size",
  y_axis_min          = "y_min",
  y_axis_max          = "y_max",
  y_axis_breaks       = "y_breaks",
  y_axis_n_breaks     = "y_n_breaks",
  y_axis_auto_breaks  = "y_auto_breaks",
  colors              = "point_colors"   # täthetskartan: point_colors är det gamla D3-namnet
)

func_d3_kontrollera_namn <- function(params, giltiga, typ) {
  okanda <- setdiff(names(params), giltiga)
  if (length(okanda) > 0)
    stop("vgr_D3: ", paste(okanda, collapse = ", "), " är inte en giltig parameter för '", typ,
         "'. Se argumentlistan i func_d3_", typ, "().")
}

# ============================================================
#  RENDERING
# ============================================================

# Kör diagramfunktionen med .rendera = TRUE och returnerar spec-listan som skickas till JavaScript
func_d3_spec <- function(x) {
  if (is.null(x$typ)) stop("vgr_D3: diagramtyp saknas — lägg till t.ex. + d3_map(...).")
  giltiga <- func_d3_giltiga(x$typ)
  params  <- func_d3_anpassa_namn(x$params, giltiga)
  func_d3_kontrollera_namn(params, giltiga, x$typ)
  do.call(func_d3_typ_funktion(x$typ), c(params, list(.rendera = TRUE)))
}

# geo_utskrivna: miljö med kontrollsummorna för de geometriblock som redan finns på sidan (se
# func_d3_geo_omfang()). NULL = sidans egen: den pågående knitr-körningens, annars en ny.
func_d3_html <- function(x, geo_utskrivna = NULL) {
  spec <- func_d3_spec(x)
  # Unikt id utan sample(): påverkar inte användarens slumptal och krockar inte efter set.seed()
  .vgr_d3_tillstand$n <- .vgr_d3_tillstand$n + 1L
  id   <- sprintf("vgrd3-%s-%d", format(as.numeric(Sys.time()) * 1000, scientific = FALSE, nsmall = 0), .vgr_d3_tillstand$n)
  id   <- gsub("[^A-Za-z0-9-]", "", id)
  # Kartgeometrin i egna block, en gång per sida även när flera kartor har samma underlag
  delad <- func_d3_dela_geometri(spec, geo_utskrivna %||% func_d3_geo_omfang())
  spec  <- delad$spec
  json <- func_d3_json(spec)
  htmltools::tagList(
    func_d3_beroenden(spec$js_fil),
    htmltools::tags$div(id = id, class = "vgr-d3-container"),
    lapply(names(delad$block), function(h)
      htmltools::tags$script(type = "application/json", `data-vgr-geo` = h, htmltools::HTML(delad$block[[h]]))),
    htmltools::tags$script(type = "application/json", id = paste0(id, "-spec"), htmltools::HTML(json)),
    htmltools::tags$script(htmltools::HTML(sprintf("vgrD3.whenReady(function(){vgrD3.render('%s');});", id)))
  )
}

# Spec (eller en del av den) som JSON-text för en script-tagg
func_d3_json <- function(x) {
  json <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA,
                           json_verbatim = TRUE)   # GeoJSON-text (klass "json") bäddas in som den är
  # Alla "<" skrivs som < (giltig JSON): varken "</script>" eller "<!--" i data kan bryta script-taggen
  gsub("<", "\\u003c", json, fixed = TRUE)
}

# --- Delad kartgeometri ---
# Kartornas geometri (områdena, bakgrundskartan och överlagret) skrivs i egna script-block,
# märkta med en kontrollsumma av innehållet, och spec:en får en referens till blocket
# (list(type = "vgrRef", ref = kontrollsumma)) som karta.js slår upp. Har flera kartor på samma
# sida samma geometri (samma områden i samma ordning och på samma rutnät) skrivs blocket bara ut
# vid den första, och de andra pekar på det.
#
# Vilka block som redan finns på sidan sparas i en miljö per sida: i den pågående
# knitr::knit()-körningens ram (R Markdown, Quarto och "Run examples"), i func_d3_spara() när
# flera diagram sparas i samma fil, och annars en ny miljö per diagram (RStudio:s Viewer, där
# varje diagram är en egen sida).
func_d3_geo_omfang <- function() {
  ram <- func_d3_knit_ram()
  if (is.null(ram)) return(new.env(parent = emptyenv()))
  omfang <- get0(".vgr_d3_geo", envir = ram, inherits = FALSE)
  if (is.null(omfang)) {
    omfang <- new.env(parent = emptyenv())
    assign(".vgr_d3_geo", omfang, envir = ram)
  }
  omfang
}

# Returnerar list(spec, block): spec med referenser i stället för geometri, och de block (namngiven
# lista kontrollsumma = JSON-text) som inte redan finns på sidan. Bara kartorna (karta.js) har geometri.
func_d3_dela_geometri <- function(spec, utskrivna) {
  block <- list()
  if (!identical(spec$js_fil, "karta")) return(list(spec = spec, block = block))
  dela <- function(g) {
    if (is.null(g)) return(g)
    json <- if (inherits(g, "json")) gsub("<", "\\u003c", as.character(g), fixed = TRUE) else func_d3_json(g)
    h <- rlang::hash(json)
    if (!exists(h, envir = utskrivna, inherits = FALSE)) {
      assign(h, TRUE, envir = utskrivna)
      block[[h]] <<- json
    }
    list(type = "vgrRef", ref = h)
  }
  spec$geo     <- dela(spec$geo)
  spec$basemap <- dela(spec$basemap)
  if (!is.null(spec$overlay)) spec$overlay$geo <- dela(spec$overlay$geo)
  list(spec = spec, block = block)
}

#' @export
#' @noRd
print.vgr_d3 <- function(x, ...) {
  print(htmltools::browsable(func_d3_html(x)))
  invisible(x)
}

# Används av knitr/Quarto när objektet skrivs ut i ett dokument. R Markdown och Quarto tar
# hand om JavaScript-beroendena. "Run examples" i hjälpen kör däremot knitr så att HTML skrivs
# direkt (out.format "html"), och då kommer beroendena aldrig med på sidan; där bäddas de in.
#' @exportS3Method knitr::knit_print
#' @noRd
knit_print.vgr_d3 <- function(x, ...) {
  if (identical(knitr::opts_knit$get("out.format"), "html")) return(func_d3_knit_inbaddat(x))
  knitr::knit_print(htmltools::browsable(func_d3_html(x)), ...)
}

# Diagrammet med skripten inbäddade. Varje skript bäddas bara in en gång per sida: vilka som
# redan finns sparas i den pågående knit()-anropets ram, som försvinner när sidan är klar.
# Hittas ingen sådan ram bäddas alla skript in (de tål att köras flera gånger).
func_d3_knit_inbaddat <- function(x) {
  renderad  <- htmltools::renderTags(func_d3_html(x))
  beroenden <- htmltools::resolveDependencies(renderad$dependencies)
  namn <- vapply(beroenden, function(d) paste(d$name, d$version), character(1))
  ram  <- func_d3_knit_ram()
  redan <- if (is.null(ram)) character(0) else get0(".vgr_d3_inbaddade", envir = ram, inherits = FALSE,
                                                     ifnotfound = character(0))
  nya <- !namn %in% redan
  if (!is.null(ram)) assign(".vgr_d3_inbaddade", c(redan, namn[nya]), envir = ram)
  knitr::asis_output(paste(c(func_d3_skript_inbaddade(beroenden[nya]), renderad$html), collapse = "\n"))
}

# Ramen för det knitr::knit()-anrop som pågår, eller NULL
func_d3_knit_ram <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    if (identical(sys.function(i), knitr::knit)) return(sys.frame(i))
  }
  NULL
}

# <script>-taggar med innehållet i JavaScript-beroendena (används även av func_d3_spara())
func_d3_skript_inbaddade <- function(beroenden) {
  vapply(beroenden, function(d) {
    innehall <- paste(readLines(file.path(d$src$file, d$script), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    paste0("<script>", gsub("</script", "<\\/script", innehall, fixed = TRUE), "</script>")
  }, character(1))
}

# ============================================================
#  HJÄLPFUNKTIONER
# ============================================================

# Storlekar: ggplot2-texter anges i punkter, geom_sf_text/linjebredder i mm
func_d3_pt_till_px <- function(pt) pt * 96 / 72
func_d3_mm_till_px <- function(mm) mm * 96 / 25.4
# Punktradie i pixlar för ggplot2:s punktstorlek (size), som geom_point ritar den
func_d3_punkt_px <- function(storlek) 0.375 * (storlek * 72.27 / 25.4 + 0.5 * 96 / 25.4 / 2) * 96 / 72

# R-färgnamn ("grey80") finns inte i CSS — gör om allt till hex
func_d3_farg <- function(x) {
  if (is.null(x)) return(NULL)
  ut <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  if (any(ok)) {
    rgba   <- grDevices::col2rgb(x[ok], alpha = TRUE)
    ut[ok] <- ifelse(rgba[4, ] == 255,
                     grDevices::rgb(rgba[1, ], rgba[2, ], rgba[3, ], maxColorValue = 255),
                     grDevices::rgb(rgba[1, ], rgba[2, ], rgba[3, ], rgba[4, ], maxColorValue = 255))
  }
  ut
}

# Typsnitt: samma kontroll som func_graf_* (systemfonts), returnerar en CSS font-family-sträng
func_d3_typsnitt <- function(font) {
  if (font %in% c("sans", "serif", "mono")) return(switch(font, sans = "sans-serif", serif = "serif", mono = "monospace"))
  if (requireNamespace("systemfonts", quietly = TRUE) && !font %in% systemfonts::system_fonts()$family) {
    message(sprintf("Font '%s' not found. Using 'sans'. Run View(systemfonts::system_fonts()) to see available fonts.", font))
    return("sans-serif")
  }
  paste0("'", font, "', sans-serif")
}

# Radbryter text som func_graf_* gör (str_wrap) och delar upp i rader för SVG
func_d3_rader <- function(text, bredd = NULL) {
  if (is.null(text) || length(text) == 0 || all(is.na(text)) || identical(text, "")) return(list())
  text <- paste(text, collapse = "\n")
  rader <- if (!is.null(bredd)) stringr::str_wrap(text, width = bredd) else text
  as.list(strsplit(rader, "\n", fixed = TRUE)[[1]])
}

# Radbryter varje rad i en text för sig (str_wrap slår annars ihop raderna): "\n" ger alltid en ny rad,
# och bredd = NULL ger bara de raderna. Används för kategoriaxelns text (x_axis_text_wrap). Returnerar
# text med "\n", som JavaScript delar upp i rader.
func_d3_radbryt <- function(text, bredd = NULL) {
  vapply(as.character(text), function(t) {
    if (is.na(t) || !nzchar(t)) return(t)
    rader <- strsplit(t, "\n", fixed = TRUE)[[1]]
    if (!is.null(bredd) && bredd > 0) rader <- vapply(rader, stringr::str_wrap, character(1), width = bredd, USE.NAMES = FALSE)
    paste(rader, collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

# Stödlinjerna (show_*_grid och grid_*) i alla diagram som har dem. Tjockleken anges som ggplot2:s
# linewidth och blir pixlar avrundade till hundradelar (0,5 -> 1,42 px och 0,25 -> 0,71 px, som förut)
func_d3_stodlinjer <- function(p) {
  for (n in c("grid_major_width", "grid_minor_width"))
    if (!is.numeric(p[[n]]) || length(p[[n]]) != 1 || is.na(p[[n]]) || p[[n]] < 0) stop("vgr_D3: ", n, " ska vara ett tal >= 0.")
  for (n in c("grid_major_color", "grid_minor_color"))
    if (!is.character(p[[n]]) || length(p[[n]]) != 1 || is.na(p[[n]]) || inherits(try(grDevices::col2rgb(p[[n]]), silent = TRUE), "try-error"))
      stop("vgr_D3: ", n, " ska vara en färg, t.ex. \"#e5e5e5\" eller \"grey80\".")
  list(major = p$show_major_grid, minor = p$show_minor_grid,
       # tolower: samma text i SVG:en som de tidigare fasta färgerna ("#e5e5e5")
       major_color = tolower(func_d3_farg(p$grid_major_color)), minor_color = tolower(func_d3_farg(p$grid_minor_color)),
       major_width = round(p$grid_major_width * 72.27 / 25.4, 2), minor_width = round(p$grid_minor_width * 72.27 / 25.4, 2))
}

# Tooltipens rubrik för x-värdena (tooltip_x_format): NULL = as.character(x), en text = datumformat
# (strftime) för en datumaxel, en funktion = det den returnerar för hela x-vektorn
func_d3_x_tooltip <- function(x, x_format = NULL) {
  if (is.null(x_format)) return(as.character(x))
  if (is.function(x_format)) {
    ut <- as.character(x_format(x))
    if (length(ut) != length(x)) stop("vgr_D3: tooltip_x_format ska returnera en text per x-värde.")
    return(ut)
  }
  if (!is.character(x_format) || length(x_format) != 1 || is.na(x_format))
    stop("vgr_D3: tooltip_x_format ska vara ett datumformat, t.ex. \"%Y\", eller en funktion.")
  if (!inherits(x, c("Date", "POSIXt")))
    stop("vgr_D3: tooltip_x_format som text är ett datumformat och kräver en datumaxel. ",
         "Ange en funktion för andra x-värden, t.ex. function(x) paste(x, \"år\").")
  format(x, x_format)
}

# Kontroll av y_axis_format ("number"/"percent") och y_axis_expand (NULL eller c(nedre, övre) >= 0)
func_d3_kontrollera_yaxel <- function(p) {
  if (!is.character(p$y_axis_format) || length(p$y_axis_format) != 1 || !p$y_axis_format %in% c("number", "percent"))
    stop("vgr_D3: y_axis_format ska vara \"number\" eller \"percent\".")
  e <- p$y_axis_expand
  if (!is.null(e) && (!is.numeric(e) || length(e) != 2 || anyNA(e) || any(e < 0)))
    stop("vgr_D3: y_axis_expand ska vara två tal >= 0, c(nedre, övre), t.ex. c(0, 0.05).")
  invisible(TRUE)
}

# Gemensam regel för tooltip_decimals i alla func_d3_*:ett angivet värde används som det är;
# NULL = 0 om alla värden är heltal, annars samma antal som dataetiketterna. min_decimaler används
# av kartorna så att tooltipen aldrig visar färre decimaler än klassgränserna (breaks_decimals).
func_d3_tooltip_decimaler <- function(tooltip_decimals, varden, etikett_decimaler = 1, min_decimaler = 0) {
  if (!is.null(tooltip_decimals)) return(tooltip_decimals)
  v <- if (is.numeric(varden)) varden[is.finite(varden)] else numeric(0)
  heltal <- all(v == round(v))
  max(if (heltal) 0 else etikett_decimaler, min_decimaler)
}

# tooltip_columns i linje-, stapel- och hanteldiagram (samma regel som kartans extra kolumner):
# namngiven vektor c("Visat namn" = "kolumn"), onamngivna visas med kolumnnamnet. Talkolumner formateras
# här med decimaler per kolumn — heltalskolumner alltid utan decimaler, annars tooltip_decimals eller
# etikett_decimaler; övriga kolumner visas som text. Returnerar list(labels, values, namn) där values är en
# textvektor per kolumn i dataramens radordning (NA = visas inte) och namn interna kolumnnamn som inte
# krockar med df. Utan kolumner: tomma listor (labels blir [] i spec:en).
func_d3_tooltip_kolumner <- function(df, kolumner, tooltip_decimals, etikett_decimaler = 1) {
  if (is.null(kolumner) || length(kolumner) == 0) return(list(labels = list(), values = list(), namn = character(0)))
  if (!is.character(kolumner)) stop("vgr_D3: tooltip_columns ska vara kolumnnamn, t.ex. c(\"Antal\" = \"antal\").")
  saknas <- setdiff(unname(kolumner), names(df))
  if (length(saknas) > 0) stop("Missing columns in data (tooltip_columns): ", paste(saknas, collapse = ", "))   # som kartorna
  etiketter <- names(kolumner) %||% rep("", length(kolumner))
  etiketter[is.na(etiketter) | etiketter == ""] <- kolumner[is.na(etiketter) | etiketter == ""]
  varden <- lapply(unname(kolumner), function(kol) {
    x <- df[[kol]]
    if (is.numeric(x)) {
      x[!is.finite(x)] <- NA   # Inf/NaN (t.ex. division med noll) visas inte, som NA
      heltal <- all(is.na(x) | x == round(x))
      func_d3_format_tal(x, func_d3_tooltip_decimaler(if (heltal) 0 else tooltip_decimals, x, etikett_decimaler))
    } else {
      x <- as.character(x)
      x[!is.na(x) & x == ""] <- NA_character_
      x
    }
  })
  # Interna kolumnnamn för diagrammens arbetsdata, som inte krockar med någon kolumn i df
  namn <- paste0(".vgr_tt", seq_along(varden))
  while (any(namn %in% names(df))) namn <- paste0(".", namn)
  list(labels = as.list(unname(etiketter)), values = varden, namn = namn)
}

# Första värdet som inte saknas (dubblettrader i tooltip_columns), annars NA
func_d3_forsta_varde <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else NA_character_ }

# Svensk talformatering (mellanslag som tusentalsavgränsare, decimalkomma)
func_d3_format_tal <- function(x, decimaler = 0, tusental = " ") {
  ifelse(is.na(x), NA_character_,
         format(round(as.numeric(x), decimaler), nsmall = decimaler, big.mark = tusental,
                decimal.mark = ",", scientific = FALSE, trim = TRUE))
}

# sf-objekt -> GeoJSON-text i objektets eget koordinatsystem (t.ex. SWEREF99 TM).
# Klassen "json" gör att jsonlite bäddar in texten som den är.
func_d3_geojson <- function(x, id = NULL, toleransen = NULL) {
  geom <- sf::st_geometry(x)
  if (!is.null(toleransen) && toleransen > 0) geom <- sf::st_simplify(geom, preserveTopology = TRUE, dTolerance = toleransen)
  g <- sf::st_sf(id = if (is.null(id)) as.character(seq_along(geom)) else as.character(id), geometry = geom)
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  sf::st_write(g, tmp, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE,
               layer_options = c("COORDINATE_PRECISION=1", "RFC7946=NO", "WRITE_BBOX=NO"))
  structure(paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = ""), class = "json")
}

# Gemensamt rutnät för kompakt geometri: steg q (en tiopotens, ca 1/200 000 av kartans
# största utsträckning, t.ex. 1 m för hela Sverige i SWEREF99 TM) och origo i nedre vänstra
# hörnet. Alla lager i samma karta ska använda samma rutnät så att de ligger kant i kant.
func_d3_geo_rutnat <- function(...) {
  lager <- Filter(Negate(is.null), list(...))
  bb <- do.call(rbind, lapply(lager, function(x) as.numeric(sf::st_bbox(x))))
  bb <- bb[stats::complete.cases(bb), , drop = FALSE]
  if (nrow(bb) == 0) return(list(q = 1, origin = c(0, 0)))
  utstr <- max(max(bb[, 3]) - min(bb[, 1]), max(bb[, 4]) - min(bb[, 2]))
  q <- if (utstr > 0) 10^floor(log10(utstr / 2e5)) else 1
  list(q = q, origin = c(floor(min(bb[, 1]) / q) * q, floor(min(bb[, 2]) / q) * q))
}

# sf-objekt med ytor -> kompakt geometri: koordinaterna avrundas till rutnätet och varje ring
# blir en platt heltalsvektor (x0, y0, dx1, dy1, ...) där första punkten är absolut och resten
# är steg från föregående punkt. Steg utan förflyttning tas bort. karta.js packar upp det
# till GeoJSON. Linjer och punkter ger NULL; anroparen använder då func_d3_geojson().
func_d3_geo_kompakt <- function(x, rutnat, toleransen = NULL) {
  geom <- sf::st_geometry(x)
  if (!is.null(toleransen) && toleransen > 0) geom <- sf::st_simplify(geom, preserveTopology = TRUE, dTolerance = toleransen)
  n <- length(geom)
  if (!all(as.character(sf::st_geometry_type(geom)) %in% c("POLYGON", "MULTIPOLYGON"))) return(NULL)
  tomma <- sf::st_is_empty(geom)
  ytor  <- vector("list", n)
  ytor[] <- list(list())
  if (all(tomma)) return(list(type = "vgrKompakt", q = rutnat$q, origin = rutnat$origin, features = ytor))

  k  <- sf::st_coordinates(sf::st_cast(geom[!tomma], "MULTIPOLYGON"))   # X, Y, L1 (ring), L2 (yta), L3 (objekt)
  xi <- round((k[, "X"] - rutnat$origin[1]) / rutnat$q)
  yi <- round((k[, "Y"] - rutnat$origin[2]) / rutnat$q)
  ny_ring <- c(TRUE, diff(k[, "L1"]) != 0 | diff(k[, "L2"]) != 0 | diff(k[, "L3"]) != 0)
  dx <- c(xi[1], diff(xi))
  dy <- c(yi[1], diff(yi))
  dx[ny_ring] <- xi[ny_ring]
  dy[ny_ring] <- yi[ny_ring]
  behall <- ny_ring | dx != 0 | dy != 0
  ring <- cumsum(ny_ring)[behall]
  ringar <- unname(split(as.integer(rbind(dx[behall], dy[behall])), rep(ring, each = 2)))

  # Ringar -> ytor -> objekt (split behåller ordningen eftersom id:na är stigande heltal)
  forsta <- which(ny_ring)
  ring_yta  <- cumsum(c(TRUE, diff(k[forsta, "L2"]) != 0 | diff(k[forsta, "L3"]) != 0))
  ytor_alla <- unname(split(ringar, ring_yta))
  yta_objekt <- k[forsta, "L3"][!duplicated(ring_yta)]
  per_objekt <- split(ytor_alla, factor(yta_objekt, levels = seq_len(sum(!tomma))))
  ytor[!tomma] <- lapply(per_objekt, unname)
  list(type = "vgrKompakt", q = rutnat$q, origin = rutnat$origin, features = ytor)
}

# Värden per år som vektorer i samma ordning som områdena (id): list(år = c(...)). Område
# som saknar rad ett år får NA (null i JSON). Kortare än en lista med id per värde.
func_d3_per_ar_vektor <- function(varden, rad_id, rad_ar, id, ar_lista) {
  stats::setNames(lapply(ar_lista, function(a) {
    rader <- rad_ar == a
    ut <- varden[rader][match(id, rad_id[rader])]
    if (length(ut) == 1) I(ut) else ut   # I(): en vektor av längd 1 ska bli en JSON-lista
  }), ar_lista)
}

# Index (från 0) för områden som saknar rad, per år: list(år = c(...)). Bara år där något
# område saknas tas med; NULL om alla områden har rad alla år.
func_d3_saknade_rader <- function(rad_id, rad_ar, id, ar_lista) {
  ut <- lapply(ar_lista, function(a) {
    i <- which(!id %in% rad_id[rad_ar == a]) - 1L
    if (length(i) == 1) I(i) else i
  })
  names(ut) <- ar_lista
  ut <- ut[lengths(ut) > 0]
  if (length(ut) == 0) NULL else ut
}

# Kolumn till tooltip eller etiketter i kompakt form. Är kolumnen densamma alla år för varje
# område skickas den en gång (const), annars per år (years). Tal skickas som tal med antal
# decimaler och formateras i webbläsaren (vgrD3.fmt ger samma resultat som func_d3_format_tal).
func_d3_kompakt_kolumn <- function(x, rad_id, rad_ar, id, ar_lista, decimaler = 0, tusental = " ") {
  tal <- is.numeric(x)
  if (!tal) x <- as.character(x)
  per_id <- split(x, factor(rad_id, levels = id))
  konstant <- all(vapply(per_id, function(v) length(unique(v)) <= 1, logical(1)))
  ut <- list(num = tal, decimals = decimaler, thousands = tusental)
  if (konstant) {
    v <- x[match(id, rad_id)]
    ut$const <- if (length(v) == 1) I(v) else v
  } else {
    ut$years <- func_d3_per_ar_vektor(x, rad_id, rad_ar, id, ar_lista)
  }
  ut
}
