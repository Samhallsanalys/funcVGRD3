#' Spara ett vgr_D3-diagram som HTML, PNG eller SVG
#'
#' Sparar ett diagram från `func_d3_*()` eller `d3_plot()` till fil. Formatet avgörs av
#' filändelsen. En lista med diagram sparas i samma HTML-fil.
#'
#' @details
#' * `.html` ger en fristående, interaktiv sida där D3 och all JavaScript är inbäddade, så
#'   att den fungerar utan nätverk.
#' * `.png` ger en bild via \pkg{webshot2}, t.ex. till PowerPoint.
#' * `.svg` ger en vektorbild via \pkg{chromote}, med samma innehåll som SVG-knappen i
#'   diagrammet.
#'
#' PNG och SVG kräver paketen webshot2 respektive chromote och en Chromium-baserad
#' webbläsare. Saknas Chrome används Microsoft Edge; sökvägen till Edge sätts i
#' miljövariabeln `CHROMOTE_CHROME` bara medan funktionen körs. Hittas ingen av dem, ange
#' sökvägen själv med `Sys.setenv(CHROMOTE_CHROME = "<sökväg>")`.
#'
#' Flera diagram kan sparas i samma HTML-fil genom att `plot` är en lista med diagram, t.ex.
#' `list("Linjediagram" = p1, "Stapeldiagram" = p2)`. Namnen i listan blir rubriker i
#' filen. PNG och SVG sparas ett diagram i taget.
#'
#' När filen sparats skrivs sökvägen ut. I RStudio:s konsol är sökvägen en länk: klicka på
#' den för att öppna filen med [func_d3_oppna()], t.ex. en HTML-fil i webbläsaren. RStudio
#' kör inte länken om sökvägen innehåller parenteser eller semikolon; då visas koden i
#' stället, och den går att kopiera till konsolen. När exemplen körs med "Run examples" på
#' en hjälpsida visas resultatet som en HTML-sida i stället för i konsolen; där blir
#' sökvägen en vanlig länk till filen.
#'
#' @param plot Ett `vgr_d3`-objekt från en `func_d3_*()`-funktion eller från
#'   `d3_plot()` och lager. För HTML går det också att ange en lista med sådana objekt,
#'   som sparas i samma fil med namnen i listan som rubriker. Obligatorisk.
#' @param fil Sökväg till filen som ska skapas. Ändelsen avgör formatet: `.html`, `.png`
#'   eller `.svg`. Saknade kataloger skapas. Obligatorisk.
#' @param zoom PNG: upplösningsfaktor (2 = dubbel upplösning, 3 = tryckkvalitet).
#' @param delay PNG och SVG: antal sekunder att vänta på att diagrammet ritats färdigt.
#' @param static PNG och SVG: `TRUE` stänger av animationerna så att bilden visar
#'   slutläget.
#'
#' @return Sökvägen `fil`, osynligt. Funktionen skriver dessutom ut var filen sparades.
#'   På sidan från "Run examples" returneras sökvägen synligt och visas som en länk.
#' @seealso [func_d3_oppna()] öppnar den sparade filen. [func_d3_karta()],
#'   [func_d3_linjediagram()] och övriga diagramfunktioner skapar diagrammen.
#' @export
#' @example inst/examples/func_d3_spara.R
func_d3_spara <- function(
    plot,
    fil,
    zoom = 2,
    delay = 1,
    static = TRUE
) {
  # En lista med diagram sparas i samma HTML-fil; namnen i listan blir rubriker
  flera <- is.list(plot) && !inherits(plot, "vgr_d3")
  diagram <- if (flera) plot else list(plot)
  if (length(diagram) == 0 || !all(vapply(diagram, inherits, logical(1), what = "vgr_d3")))
    stop("func_d3_spara: plot måste vara ett vgr_d3-objekt (från func_d3_*()) eller en lista med sådana.")
  format <- tolower(tools::file_ext(fil))
  if (!format %in% c("html", "png", "svg")) stop("func_d3_spara: filändelsen måste vara .html, .png eller .svg.")
  if (flera && format != "html")
    stop("func_d3_spara: flera diagram kan bara sparas i en .html-fil. Spara PNG och SVG ett diagram i taget.")
  plot <- diagram[[1]]
  if (!dir.exists(dirname(fil))) dir.create(dirname(fil), recursive = TRUE)

  if (format != "html" && static && !is.null(plot$typ) && "animation_duration" %in% func_d3_giltiga(plot$typ))
    plot$params$animation_duration <- 0

  # --- Fristående HTML: alla skript läses in och bäddas in i sidan ---
  skriv_html <- function(sokvag) {
    # Filen är en sida: kartor med samma geometri delar på ett geometriblock
    geo_utskrivna <- new.env(parent = emptyenv())
    innehall <- if (!flera) func_d3_html(plot, geo_utskrivna) else {
      rubriker <- names(diagram) %||% rep("", length(diagram))
      htmltools::tagList(lapply(seq_along(diagram), function(i) {
        htmltools::div(
          style = "margin-bottom:32px",
          if (!is.na(rubriker[i]) && nzchar(rubriker[i]))
            htmltools::tags$h2(rubriker[i], style = "font-family:Verdana,sans-serif;font-size:16px;margin:16px 0 8px"),
          func_d3_html(diagram[[i]], geo_utskrivna))
      }))
    }
    renderad <- htmltools::renderTags(innehall)
    beroenden <- htmltools::resolveDependencies(renderad$dependencies)
    skript <- func_d3_skript_inbaddade(beroenden)
    titel <- plot$params[["title", exact = TRUE]] %||% "vgr_D3"   # [[ ]]: $title skulle matcha title_size
    sida <- paste0(
      # viewport: på en mobil visas sidan i skärmens bredd (utan den som en ~980 px bred
      # skrivbordssida, förminskad), så att kartornas responsiva layout får verka
      "<!DOCTYPE html>\n<html lang=\"sv\">\n<head>\n<meta charset=\"utf-8\">\n",
      "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
      "<title>", htmltools::htmlEscape(paste(titel, collapse = " ")), "</title>\n",
      paste(skript, collapse = "\n"),
      "\n</head>\n<body style=\"margin:0;padding:8px;background:#fff\">\n",
      renderad$html,
      "\n</body>\n</html>\n")
    con <- file(sokvag, open = "w", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(sida, con, useBytes = FALSE)
    invisible(sokvag)
  }

  if (format == "html") {
    skriv_html(fil)
    return(func_d3_sparad(fil))
  }

  # webshot2/chromote behöver en Chromium-webbläsare — saknas Chrome används Edge (finns på VGR-datorer)
  if (Sys.getenv("CHROMOTE_CHROME") == "") {
    if (!requireNamespace("chromote", quietly = TRUE)) stop("func_d3_spara: installera paketet chromote för att spara som PNG eller SVG.")
    chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
    if (is.null(chrome) || !nzchar(chrome)) {
      edge <- c("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
                "C:/Program Files/Microsoft/Edge/Application/msedge.exe")
      edge <- edge[file.exists(edge)]
      if (length(edge) == 0) stop("func_d3_spara: hittar varken Chrome eller Edge. Sätt Sys.setenv(CHROMOTE_CHROME = \"<sökväg>\").")
      # Sätts bara medan funktionen körs, så att sessionen inte ändras
      on.exit(Sys.unsetenv("CHROMOTE_CHROME"), add = TRUE)
      Sys.setenv(CHROMOTE_CHROME = edge[1])
    }
  }

  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  skriv_html(tmp)
  url <- paste0("file:///", normalizePath(tmp, winslash = "/"))
  bredd <- plot$params$width %||% formals(func_d3_typ_funktion(plot$typ))$width %||% 1000
  hojd  <- plot$params$height %||% formals(func_d3_typ_funktion(plot$typ))$height %||% 800

  if (format == "png") {
    if (!requireNamespace("webshot2", quietly = TRUE)) stop("func_d3_spara: installera paketet webshot2 för att spara som PNG.")
    webshot2::webshot(url, file = fil, selector = "svg.vgr-d3-svg", zoom = zoom, delay = delay,
                      vwidth = bredd + 100, vheight = hojd + 200, quiet = TRUE)
  } else {
    session <- chromote::ChromoteSession$new(width = bredd + 100, height = hojd + 200)
    on.exit(session$close(), add = TRUE)
    laddad <- session$Page$loadEventFired(wait_ = FALSE)
    session$Page$navigate(url, wait_ = FALSE)
    session$wait_for(laddad)
    Sys.sleep(delay)
    fel <- session$Runtime$evaluate("(document.querySelector('.vgr-d3-container') || {}).getAttribute ? document.querySelector('.vgr-d3-container').getAttribute('data-error') : null")
    if (!is.null(fel$result$value)) stop("func_d3_spara: diagrammet kunde inte ritas: ", fel$result$value)
    svar <- session$Runtime$evaluate("vgrD3.serializeSvg(document.querySelector('svg.vgr-d3-svg'))")
    if (is.null(svar$result$value)) stop("func_d3_spara: kunde inte läsa SVG från sidan.")
    con <- file(fil, open = "w", encoding = "UTF-8")
    writeLines(svar$result$value, con)
    close(con)
  }
  func_d3_sparad(fil)
}

#' Öppna en sparad fil
#'
#' Öppnar en fil i det program datorn använder för filtypen, t.ex. en HTML-fil från
#' [func_d3_spara()] i webbläsaren eller en PNG-fil i bildvisaren. Det är den här
#' funktionen som körs när man klickar på sökvägen som `func_d3_spara()` skriver ut i
#' RStudio.
#'
#' @param fil Sökväg till filen som ska öppnas. Obligatorisk.
#'
#' @return Sökvägen `fil`, osynligt. Funktionen anropas för att öppna filen. Finns filen
#'   inte blir det ett fel.
#' @seealso [func_d3_spara()]
#' @export
#' @example inst/examples/func_d3_oppna.R
func_d3_oppna <- function(fil) {
  if (!file.exists(fil)) stop("func_d3_oppna: filen finns inte: ", fil)
  utils::browseURL(normalizePath(fil))
  invisible(fil)
}

# Returvärdet från func_d3_spara(). Normalt sökvägen, osynligt, och ett meddelande om var
# filen sparades. "Run examples" i hjälpen kör exemplen med knitr och visar resultatet som en
# HTML-sida, där meddelanden blir vanlig text. Där returneras sökvägen i stället synligt med
# klassen vgr_d3_fil, och knit_print skriver en länk till filen via hjälpserverns adress
# /session/, som pekar på sessionens temporära katalog (där exemplens filer sparas).
func_d3_sparad <- function(fil) {
  if (func_d3_pa_hjalpsida(fil)) return(structure(fil, class = "vgr_d3_fil"))
  func_d3_meddela_sparad(fil)
  invisible(fil)
}

# TRUE när koden körs av "Run examples" (knitr skriver HTML direkt, inte via R Markdown)
# och filen ligger i sessionens temporära katalog
func_d3_pa_hjalpsida <- function(fil) {
  isTRUE(getOption("knitr.in.progress")) &&
    identical(knitr::opts_knit$get("out.format"), "html") &&
    nzchar(func_d3_session_url(fil))
}

# Adressen till filen under /session/, eller "" om filen inte ligger i tempdir()
func_d3_session_url <- function(fil) {
  sokvag <- normalizePath(fil, winslash = "/", mustWork = FALSE)
  tmp <- paste0(normalizePath(tempdir(), winslash = "/"), "/")
  if (!startsWith(tolower(sokvag), tolower(tmp))) return("")
  delar <- strsplit(substring(sokvag, nchar(tmp) + 1), "/", fixed = TRUE)[[1]]
  paste0("/session/", paste(vapply(delar, utils::URLencode, character(1), reserved = TRUE), collapse = "/"))
}

#' @exportS3Method knitr::knit_print
#' @noRd
knit_print.vgr_d3_fil <- function(x, ...) {
  knitr::asis_output(sprintf(
    "<p>Sparade <a href=\"%s\" target=\"_blank\">%s</a> (klicka för att öppna)</p>",
    func_d3_session_url(x), htmltools::htmlEscape(normalizePath(x))))
}

#' @export
#' @noRd
print.vgr_d3_fil <- function(x, ...) {
  print(unclass(x))
  invisible(x)
}

# Skriver ut var filen sparades. Där det går (RStudio) blir sökvägen en länk som kör
# funcVGRD3::func_d3_oppna(); RStudio kör bara länkar av formen paket::funktion(argument).
# Annars skrivs bara sökvägen ut.
func_d3_meddela_sparad <- function(fil) {
  sokvag <- normalizePath(fil)
  if (isTRUE(cli::ansi_hyperlink_types()$run)) {
    kod <- sprintf("funcVGRD3::func_d3_oppna(\"%s\")", normalizePath(fil, winslash = "/"))
    message("Sparade ", cli::format_inline("{.run [{sokvag}]({kod})}"), " (klicka för att öppna)")
  } else {
    message("Sparade ", sokvag)
  }
}
