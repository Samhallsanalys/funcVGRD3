#' Rita en interaktiv koropletkarta
#'
#' Interaktiv D3-version av `funcVGR::func_graf_karta()` med samma parametrar och
#' standardvärden, så att ett befintligt anrop fungerar om man bara byter funktionsnamn.
#' Utöver originalets parametrar finns tooltip med rangordning, zoom och sökruta, filter i
#' förklaringen, tidsreglage (`time_column`), växling mellan nivå och förändring,
#' bubblor (`bubble_*`), överlagrade gränser (`overlay_*`) och export till PNG och SVG.
#'
#' @details
#' Områdena färgas antingen efter en klassindelad färgskala för `df_value` (`breaks_*`)
#' eller efter kategorierna i `group`. Med `time_column` innehåller `df` en rad per område
#' och år, och kartan får ett tidsreglage; klassgränserna är då gemensamma för alla år.
#' Klassgränser, etiketter och talformat beräknas i R, så kartan ser ut som
#' ggplot2-versionen. Geometrin skickas i kartans eget koordinatsystem (t.ex. SWEREF99 TM);
#' data i lon/lat transformeras till SWEREF99 TM (EPSG:3006).
#'
#' Datan skickas i kompakt form så att HTML-filen blir liten även med många områden och
#' år: ytornas koordinater avrundas till ett rutnät (ca 1/200 000 av kartans största
#' utsträckning, t.ex. 1 m för hela Sverige) och sparas som heltalssteg, värdena per år
#' sparas som listor i områdenas ordning, tooltipkolumner som inte ändras mellan åren sparas
#' en gång per område, och klasser och förändringar räknas fram i webbläsaren.
#'
#' Funktionen ritar inte diagrammet direkt. Den returnerar ett objekt som sparar de
#' argument du angett, och diagrammet beräknas först när objektet skrivs ut eller sparas.
#' Därför går det att ändra inställningar i efterhand med `+`, t.ex.
#' `p + d3_labs(title = "Ny rubrik")` (se [d3_plot()]).
#'
#' @section Skillnader mot func_graf_karta():
#' Etiketter som flyttas isär (`data_label_repel`) använder en egen algoritm i stället för
#' ggrepel, och glöd (`data_label_glow`) ritas som en textkontur.
#'
#' @param df Data frame eller `sf`-objekt med data och geometri. En rad per område,
#'   eller en rad per område och år när `time_column` anges. Obligatorisk.
#' @param df_value Namn på kolumnen med värden som ska visas, som text, t.ex.
#'   `"arbetsloshet_andel"`. Kan utelämnas när `group` anges; då används `df_value` bara
#'   till etiketterna och tooltipen. Standard är `NULL`.
#' @param basemap `sf`-objekt som bakgrundskarta, t.ex. läns- eller regiongräns. `NULL`
#'   (standard) ger ingen bakgrundskarta.
#' @param geom_col Namn på geometrikolumnen i `df`, som text, t.ex. `"geom"` eller
#'   `"geometry"`. Obligatorisk.
#' @param group Namn på kolumnen med kategorier som områdena färgas efter, som text,
#'   t.ex. `"kategori"`. Då får varje grupp en egen färg och `breaks_*`, `color` och
#'   `color_position` används inte; `df_value` används fortfarande till etiketter och
#'   tooltip. Grupperna ordnas efter faktornivåerna (oanvända nivåer tas bort), tal
#'   sorteras och annan text får ordningen den först förekommer i `df` (samma regel som
#'   i linje- och stapeldiagrammen, så samma data får samma färger). Områden där gruppen
#'   saknas (`NA`) ritas i `color_na`. Förändringsläget finns inte med grupper. `NULL`
#'   (standard) ger en färgskala efter `df_value`.
#' @param colors Färger för grupperna, används bara när `group` anges. En namnlös vektor
#'   fördelas i gruppordning och återanvänds om den är för kort. En namngiven vektor
#'   matchas mot gruppnamnen, t.ex. `c("Fler födda, fler inflyttare" = "#1a9fb3")`.
#'   Standard är VGR:s färgtema "ru" (samma färger som `func_fargtema("ru")` i funcVGR);
#'   `NULL` ger samma färger.
#' @param basemap_color Fyllnadsfärg för bakgrundskartan.
#' @param basemap_color_line Linjefärg för bakgrundskartans gränser.
#' @param data_color_line Linjefärg mellan dataområdena.
#' @param data_line_size Linjetjocklek för dataområdenas gränser (typiskt 0,1–1).
#' @param title Huvudrubrik. `NULL` (standard) ger ingen rubrik.
#' @param text_scale Faktor som alla teckenstorlekar multipliceras med (rubriker, axlar,
#'   förklaring, panelrubriker och etiketter). 1 = storlekarna som de är angivna, t.ex.
#'   1.3 för stora exporter.
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
#' @param legend_title Förklaringens rubrik. `NULL` (standard) ger ingen rubrik, eller
#'   namnet på `group`-kolumnen när `group` anges.
#' @param legend_name Alias för `legend_title` (bakåtkompatibilitet). Vinner om den
#'   anges.
#' @param legend_position Förklaringens placering: `"none"`, `"top"`, `"bottom"`,
#'   `"left"` eller `"right"`. Anges den inte och `group` är satt blir den `"right"`.
#' @param legend_key_width Bredd på förklaringens symboler i mm (typiskt 5–30).
#' @param legend_key_height Höjd på förklaringens symboler i mm (typiskt 5–30).
#' @param legend_text_size Förklaringens teckenstorlek i punkter.
#' @param legend_key_shape Symbolform i förklaringen: `"square"` eller `"circle"`.
#' @param color Basfärg för färgskalan (den mörkaste färgen). Används inte när `group`
#'   anges (använd `colors`).
#' @param color_position Vilka värden som får mörkast färg: `"highest"` (höga värden
#'   mörka) eller `"lowest"`. Används inte när `group` anges.
#' @param breaks_method Klassindelning: `"equal"` (lika breda intervall), `"quantile"`
#'   (lika många i varje klass) eller `"custom"` (egna gränser i `breaks_custom`).
#'   Används inte när `group` anges. Med `time_column` gäller samma gränser alla år, så
#'   att färgerna går att jämföra.
#' @param breaks_custom Numerisk vektor med egna klassgränser. Används bara när
#'   `breaks_method = "custom"`.
#' @param breaks_num_group Antal klasser (1–20). Används inte när `group` anges.
#' @param breaks_decimals Antal decimaler i klassgränsernas etiketter. Klassen omfattar sin
#'   nedre gräns men inte sin övre. Med heltalsgränser och heltalsvärden skrivs klassen
#'   [40, 45) som "40-44"; har värdena decimaler skrivs den med minst en decimal, "40,0-44,9",
#'   så att etiketten visar vilka värden som ingår.
#' @param data_label_show Visa etiketter på kartan.
#' @param data_label_text_size Etiketternas teckenstorlek i mm (ggplot2:s textenhet, 3
#'   mm ≈ 8,5 punkter).
#' @param data_label_text_color Etiketternas textfärg. `"auto"` ger mörk text på ljusa
#'   områden och vit text på mörka, efter områdets färg; passar med
#'   `data_label_box = FALSE`.
#' @param data_label_box Visa en ruta bakom etiketterna.
#' @param data_label_box_color Rutans bakgrundsfärg (när `data_label_box = TRUE`).
#' @param data_label_box_alpha Rutans opacitet (0 = genomskinlig, 1 = helt täckande).
#' @param data_label_decimals Antal decimaler i etiketterna.
#' @param data_label_decimal Alias för `data_label_decimals` (bakåtkompatibilitet).
#'   Vinner om den anges.
#' @param data_label_glow Lägg en glöd runt etiketterna (ritas som en mjuk textkontur).
#' @param data_label_glow_color Glödens färg (när `data_label_glow = TRUE`).
#' @param data_label_glow_sigma Glödens bredd; högre ger mer spridning (typiskt 1–10).
#' @param data_label_glow_expand Extra bredd som läggs till `data_label_glow_sigma`
#'   (typiskt 1–5).
#' @param data_label_column Kolumn med etikettext, som text. `NULL` (standard) ger
#'   värdet i `df_value`, eller gruppen om `df_value` inte anges.
#' @param data_label_show_value Om `TRUE` och `data_label_column` anges visas både text
#'   och värde, t.ex. "Borås: 123".
#' @param data_label_separator Avskiljare mellan text och värde när
#'   `data_label_show_value = TRUE`.
#' @param data_label_repel Flytta isär etiketterna så att de inte överlappar.
#'   D3-versionen har en egen algoritm (inte ggrepel).
#' @param data_label_force Extra avstånd mellan etiketter som flyttats isär; högre ger
#'   mer avstånd (typiskt 0,5–5).
#' @param data_label_max_overlaps Etiketter som fortfarande överlappar fler än så här
#'   många andra döljs.
#' @param data_label_nudge_x Vågrät förskjutning av etiketterna i kartans enheter
#'   (positiv = åt höger).
#' @param data_label_nudge_y Lodrät förskjutning av etiketterna i kartans enheter
#'   (positiv = uppåt).
#' @param data_label_line Visa linjer från flyttade etiketter till deras område.
#' @param data_label_line_color Färg på linjerna från etiketterna.
#' @param data_label_line_size Tjocklek på linjerna från etiketterna (typiskt 0,1–1).
#' @param data_label_outside_map `TRUE` placerar etiketterna i kolumner till vänster och
#'   höger om kartan, med linjer till områdena (kräver `data_label_repel = TRUE`).
#' @param data_label_expand_factor Utrymme för etiketter utanför kartan, som andel av
#'   kartans bredd (0,5 = 50 %, fördelat på båda sidor).
#' @param data_label_filter Visa etiketter bara för de här områdena (teckenvektor med
#'   namn eller koder). `NULL` (standard) visar alla.
#' @param data_label_filter_column Kolumn som `data_label_filter` matchas mot, som text.
#'   `NULL` (standard) väljer `"region_kod"` eller `"kommun"` om de finns.
#' @param debug `TRUE` returnerar en lista med diagnostik (data, klassindelning och den
#'   beräknade specifikationen) i stället för diagrammet.
#' @param font Typsnitt, t.ex. `"Verdana"` eller `"Arial"`. Måste vara installerat på
#'   datorn, annars används `"sans"` (med ett meddelande).
#' @param time_column Kolumn med år eller period, som text. Ger ett tidsreglage med
#'   uppspelningsknapp; klassgränserna är gemensamma för alla år så att färgerna går att
#'   jämföra. `NULL` (standard) ger en karta.
#' @param time_default År eller period som visas när kartan öppnas. `NULL` (standard)
#'   ger det senaste.
#' @param time_label_show Visa vald period med stor text i kartans hörn.
#' @param percentage_growth Gäller förändringsläget (se `interactive_mode_toggle`):
#'   `TRUE` ger procentuell förändring, `FALSE` absolut förändring. Används inte när
#'   `group` anges.
#' @param basemap_fit `TRUE` gör kartutsnittet så stort att hela bakgrundskartan syns
#'   (som i ggplot2-versionen), `FALSE` zoomar till dataområdena.
#' @param geom_id_column Kolumn som identifierar varje område över åren, som text.
#'   `NULL` (standard) letar efter t.ex. `"region_kod"`, `"kommunkod"` eller `"kommun"`,
#'   annars används geometrins position.
#' @param geom_simplify_tolerance Förenkla geometrierna innan de skickas till
#'   webbläsaren, i kartans enheter (t.ex. 50 = 50 m i SWEREF99 TM). Ger en mindre
#'   HTML-fil. `NULL` (standard) förenklar inte.
#' @param overlay `sf`-objekt som ritas som konturer ovanpå data, t.ex. kommunalförbund.
#'   `NULL` (standard) ger inget.
#' @param overlay_color Linjefärg för `overlay`.
#' @param overlay_line_size Linjetjocklek för `overlay`, i samma enhet som
#'   `data_line_size`.
#' @param overlay_label_column Kolumn i `overlay` med namn som skrivs ut i varje område,
#'   som text. `NULL` (standard) ger inga namn.
#' @param overlay_label_size Teckenstorlek för namnen i `overlay`, i mm (som ggplot2:s
#'   `geom_sf_text`).
#' @param bubble_column Kolumn med värden som visas som proportionella cirklar i varje
#'   område, som text, t.ex. `"befolkning"`. `NULL` (standard) ger inga bubblor.
#' @param bubble_color Bubblornas fyllnadsfärg.
#' @param bubble_alpha Bubblornas opacitet (0–1).
#' @param bubble_max_size Radie i pixlar för den största bubblan.
#' @param bubble_line_color Bubblornas kantfärg.
#' @param legend_title_change Förklaringens rubrik i förändringsläget. `NULL` (standard)
#'   ger samma som `legend_title`. Används inte när `group` anges.
#' @param color_low Färg för den ljusa änden av färgskalan. Används inte när `group`
#'   anges.
#' @param color_negative Färg för klasser som helt ligger under noll, vilket ger en
#'   tvåfärgad skala (bra för förändring). `NULL` (standard) ger en färgskala. Används
#'   inte när `group` anges.
#' @param color_na Fyllnadsfärg för områden som saknar värde.
#' @param breaks_round_labels `TRUE` flyttar upp den nedre gränsen i varje intervall ett
#'   steg för renare etiketter (t.ex. "5,0-5,9" i stället för "4,9-5,9").
#' @param breaks_custom_change Numerisk vektor med egna klassgränser för förändringsläget,
#'   t.ex. `c(-5, -2, 0, 2, 5, 10)`. `NULL` (standard) ger samma metod som `breaks_method`,
#'   eller lika intervall när huvudkartan har egna klassgränser (`breaks_method =
#'   "custom"`). Används inte när `group` anges.
#' @param data_label_thousands_mark Tusentalsavgränsare, t.ex. `" "` (hårt mellanslag),
#'   `","` eller `"."`. `NULL` ger ingen.
#' @param data_label_zoom `TRUE` visar etiketterna först när man har zoomat in så långt
#'   att de flesta inte krockar: minst 75 % av etiketterna i kartutsnittet ska gå fri från
#'   sina grannar (två etiketter får överlappa varandra med högst `data_label_zoom_overlap`
#'   av bredden eller höjden). Innan dess visas texten "Zooma in för att visa värden".
#'   Passar kartor med många små områden, t.ex. RegSO. `FALSE` (standard) visar
#'   etiketterna direkt.
#' @param data_label_zoom_overlap Hur mycket två etiketter får överlappa varandra och ändå
#'   räknas som fria när `data_label_zoom = TRUE`, som andel av etikettens bredd eller höjd
#'   (0–0,9). Lägre värde ger mindre överlappning men kräver att man zoomar in mer innan
#'   etiketterna visas; `0` tillåter ingen överlappning. Standard är `0.4`.
#' @param tooltip_show Visa en tooltip när muspekaren hålls över diagrammet.
#' @param tooltip_name_column Kolumn med områdenas namn, som text. `NULL` (standard)
#'   letar efter `"kommunnamn"`, `"kommun"`, `"region"` eller `"namn"`.
#' @param tooltip_columns Extra kolumner som visas i tooltipen, gärna namngivna för
#'   visning, t.ex. `c("Befolkning" = "bef")`. `NULL` (standard) ger inga.
#' @param tooltip_rank Visa rangordning, t.ex. "Plats 12 av 49".
#' @param tooltip_rank_order Rangordning: `"desc"` (högst värde = plats 1) eller `"asc"`
#'   (lägst = plats 1).
#' @param tooltip_class Visa raden med områdets klass (förklaringens etikett, t.ex.
#'   "40,0-44,9"), eller gruppen när `group` anges. `FALSE` tar bort raden.
#' @param tooltip_value_label Etikett för värderaden. `NULL` (standard) ger
#'   förklaringens rubrik, annars namnet på `df_value` (alltid `df_value` när `group`
#'   anges).
#' @param tooltip_unit Enhet som läggs efter nivåvärden och absolut förändring, t.ex. `"
#'   kr"`. Procentuell förändring visas alltid med " %". `NULL` (standard) ger ingen
#'   enhet.
#' @param tooltip_unit_change Enhet för förändringsvärdet i förändringsläget, t.ex.
#'   `" procentenheter"` när `df_value` är en andel i procent: skillnaden mellan två
#'   andelar är då procentenheter, inte procent. `NULL` (standard) ger samma enhet som
#'   `tooltip_unit`, eller " %" när `percentage_growth = TRUE`.
#' @param tooltip_decimals Antal decimaler i tooltipens värden. `NULL` (standard) ger 0
#'   om alla värden är heltal, annars `data_label_decimals`, men aldrig färre än
#'   `breaks_decimals` eller klassetiketterna. Förändringsläget räknar sina egna decimaler.
#' @param interactive_zoom Zooma och panorera med mushjulet, och klicka på ett område
#'   för att zooma dit. Man kan zooma in så långt att även det minsta området fyller
#'   kartan (högst 1 000 gånger).
#' @param interactive_zoom_group_column Kolumn som grupperar områdena, som text (t.ex.
#'   `"kf"`). Ett klick på ett område zoomar då till hela gruppen. `NULL` (standard)
#'   zoomar till området.
#' @param interactive_search Visa en sökruta för områdenas namn. Medan man skriver visas en
#'   lista, som går att scrolla, med namn som börjar med eller innehåller texten (högst 250). När man väljer ett namn
#'   i listan, eller skriver hela namnet, zoomar kartan direkt till området; Enter behövs
#'   inte. Ett kryss i rutan tömmer den.
#' @param interactive_legend Håll muspekaren över en klass i förklaringen för att
#'   framhäva den, klicka för att dölja eller visa den.
#' @param interactive_export Visa knappar för att ladda ned diagrammet som PNG eller
#'   SVG.
#' @param interactive_mode_toggle Visa knappar för att växla mellan nivå och förändring
#'   sedan första året (kräver `time_column`). `NULL` (standard) ger knapparna när
#'   `time_column` har minst två år. Finns inte när `group` anges.
#' @param interactive_mode_labels Knapptexterna för de två lägena, som en namngiven
#'   teckenvektor med `niva` och `forandring`.
#' @param interactive_time_slider Visa tidsreglaget när `time_column` anges.
#' @param interactive_time_range `TRUE` (standard) ger tidsreglaget två handtag i
#'   förändringsläget, så att både startår och slutår kan väljas; förändringen räknas om
#'   för den valda perioden. I nivåläget har reglaget ett handtag. `FALSE` ger ett handtag
#'   och förändring från första året, som i tidigare versioner. Klassgränserna för
#'   förändringen räknas på första till sista året; ange `breaks_custom_change` om de ska
#'   passa även kortare perioder.
#' @param interactive_resize `TRUE` visar ett draghandtag i kartytans nedre högra hörn (ovanför
#'   en förklaring eller bildtext under kartan), så att användaren kan ändra kartytans bredd
#'   och höjd med muspekaren eller fingret. Medan man drar visar en streckad ram kartytans
#'   nya storlek, och kartan ritas om när man släpper. Med `resize_keep_scale = TRUE`
#'   (standard) behåller kartan sin skala och rutan visar ett större eller mindre utsnitt;
#'   med `FALSE` ritas kartan om så att den fyller den nya rutan. Rubriker, förklaring,
#'   bildtext och knappar behåller sin storlek, och läge, år, dolda klasser och zoom följer
#'   med. Dubbelklick på handtaget eller Återställ ger tillbaka
#'   `width` × `height`. Export till PNG och SVG får den dragna storleken. Gränserna sätts
#'   med `resize_min` och `resize_max`. `FALSE` (standard) ger inget handtag.
#' @param interactive_exclude Kryssrutor som tonar ned områden på kartan och räknar bort dem
#'   i summeringsraden, som en namngiven teckenvektor med kryssrutans text som namn och en
#'   logisk kolumn som värde, t.ex. `c("Exkludera studentområden" = "studentomrade")`. Ett
#'   område tas bort när rutan är ikryssad och kolumnen är `TRUE` för området något år (`NA`
#'   räknas som `FALSE`). Borttagna områden får ingen etikett eller bubbla. Kryssrutorna
#'   visas i summeringsraden under kartan (se `summary_text`). `NULL` (standard) ger inga.
#' @param summary_text Summeringsrad under kartan som beskriver de områden som syns i
#'   kartytan just nu. Raden räknas om när man zoomar, panorerar, byter år eller döljer
#'   klasser i förklaringen. Texten är en mall där `{period}` ersätts med året (eller
#'   perioden i förändringsläget), `{antal}` med antalet områden och övriga `{namn}` med
#'   summorna och kvoterna i `summary_sum` och `summary_ratio`. `" · "` i mallen ritas som en
#'   avgränsare. `NULL` (standard) ger en mall med alla summor och kvoter, när `summary_sum`
#'   eller `summary_ratio` anges; annars ingen rad (utom kryssrutorna i
#'   `interactive_exclude`).
#' @param summary_sum Kolumner som summeras över de områden som syns, som en namngiven
#'   teckenvektor där namnet är platshållaren i `summary_text`, t.ex.
#'   `c(bef = "bef2064")`. Saknade värden räknas som 0. `NULL` (standard) ger inga summor.
#' @param summary_ratio Kvoter mellan två summor, som en namngiven lista med
#'   teckenvektorer av längd 2 (täljare, nämnare), t.ex.
#'   `list(andel = c("sys_antal", "bef2064"))`. Bara områden där både täljaren och nämnaren
#'   finns räknas med. `NULL` (standard) ger inga kvoter.
#' @param summary_ratio_scale Tal som kvoterna multipliceras med. Standard är `100` (procent).
#' @param summary_ratio_decimals Antal decimaler i kvoterna. Standard är `1`.
#' @param summary_ratio_unit Text efter kvoterna. Standard är `" %"`.
#' @param summary_count_label Ordet efter antalet områden i `{antal}`, i singular och
#'   plural. Standard är `c("område", "områden")`.
#' @param summary_extent Vilka områden som räknas som synliga: `"inside"` (standard) bara
#'   områden som syns helt i kartytan, `"intersects"` även områden som syns delvis.
#' @param animation_duration Övergångstid i millisekunder när färger eller zoom ändras.
#'   0 = ingen animation.
#' @param animation_play_interval Tid i millisekunder per år när uppspelningsknappen
#'   används.
#' @param width Kartans bredd i pixlar. Med `responsive = TRUE` är det största bredden.
#' @param height Kartans höjd i pixlar vid bredden `width`.
#' @param responsive `TRUE` (standard) ritar om kartan i den bredd som finns när den inte
#'   får plats i `width`, t.ex. på en mobil eller i en smal spalt. Text, förklaring och
#'   knappar behåller då sin storlek i stället för att krympa, och höjden minskar i samma
#'   proportion. Kartan ritas om när fönstret ändrar storlek. `FALSE` ger en karta med fast
#'   storlek, som i tidigare versioner.
#' @param responsive_breakpoint Bredd i pixlar under vilken kartan räknas som smal (t.ex.
#'   en mobil i stående läge): en förklaring till vänster eller höger läggs då under kartan,
#'   så att kartan får hela bredden, och kartan blir högre än proportionellt (högst
#'   `height`). Gäller bara med `responsive = TRUE`. Standard är `600`.
#' @param resize_min Minsta bredd och höjd i pixlar när kartan dras till ny storlek, som en
#'   numerisk vektor av längd 2. Gränsen gäller hela kartan, med rubriker, förklaring och
#'   bildtext, inte bara kartytan i ramen. Gäller bara med `interactive_resize = TRUE`. Standard är
#'   `c(300, 250)`.
#' @param resize_max Största bredd och höjd i pixlar när kartan dras till ny storlek, som en
#'   numerisk vektor av längd 2, för hela kartan som `resize_min`. `NULL` (standard) begränsar kartan till ytan runt den: sidans
#'   eller spaltens bredd och, i en reveal.js-presentation, bildytans höjd. Kartan blir aldrig
#'   bredare än ytan runt den, även om `resize_max` är större. Gäller bara med
#'   `interactive_resize = TRUE`.
#' @param resize_keep_scale `TRUE` (standard) gör att kartan behåller sin skala när rutan dras
#'   till ny storlek: rutan visar då ett annat utsnitt av kartan, med övre vänstra hörnet kvar.
#'   En större ruta visar mer runt kartan, en mindre visar en del av den, och man kan panorera
#'   för att se resten. `FALSE` ritar om kartan så att den fyller den nya storleken (som i
#'   version 0.9.0). Gäller bara med `interactive_resize = TRUE`.
#' @param .rendera Intern. `TRUE` bygger diagrammet och används när objektet skrivs ut.
#'   Ändra inte.
#'
#' @details
#' **Pekskärm.** På en pekskärm visar det första trycket på ett område dess tooltip, och
#' nästa tryck på samma område zoomar dit. Ett tryck utanför områdena stänger tooltipen.
#' Nyp för att zooma och dra med ett finger för att flytta kartan.
#'
#' **Summeringsrad.** Med `summary_sum`, `summary_ratio` eller `interactive_exclude` får kartan
#' en rad under kartytan, t.ex. "Områdena som visas, 2023: 152 300 invånare 20–64 år · 81,4 %
#' sysselsatta · 96 områden". Områden i klasser som dolts i förklaringen och områden som tagits
#' bort med kryssrutorna räknas inte med. Raden följer inte med vid export till PNG eller SVG.
#'
#' **Flera kartor med samma underlag.** Kartans geometri (områdena, `basemap` och `overlay`)
#' skrivs i ett eget block på sidan. Har flera kartor i samma dokument (R Markdown, Quarto)
#' eller i samma fil från [func_d3_spara()] samma geometri, skrivs den bara en gång och de
#' andra kartorna använder samma block. Det gör filen mindre, t.ex. med flera RegSO-kartor i
#' samma presentation. Geometrin räknas som samma när områdena, deras ordning och, för
#' `basemap` och `overlay`, kartans utsträckning är desamma. Använder dokumentet knitr-cache
#' (`cache: true`) och bara en del av kartorna ritas om, kan en karta peka på ett block som
#' inte längre finns; rensa då cachen.
#'
#' @return Ett objekt av klassen `vgr_d3`. Det visas i RStudio:s Viewer när det skrivs ut,
#'   i HTML-dokument från R Markdown och Quarto och på sidan från "Run examples" i
#'   hjälpen. Spara det med [func_d3_spara()].
#'   Med `debug = TRUE` returneras i stället en lista med diagnostik.
#' @seealso [func_d3_karta_change()] för förändringskartor, [func_d3_karta_heatmap()] för
#'   täthetskartor, [d3_plot()] för `+`-syntaxen och [func_d3_spara()].
#' @export
#' @example inst/examples/func_d3_karta.R
func_d3_karta <- function(
    # === REQUIRED PARAMETERS ===
  df,
  df_value = NULL,
  basemap = NULL,
  geom_col,

  # === GROUPING (categorical map) ===
  group = NULL,
  colors = func_d3_farger_ru(),

  # === MAP STYLING ===
  basemap_color = "#d3d3d3",
  basemap_color_line = "white",
  data_color_line = "grey90",
  data_line_size = 0.2,

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
  margin_top = 2,
  margin_right = 2,
  margin_bottom = 2,
  margin_left = 2,

  # === LEGEND ===
  legend_title = NULL,
  legend_name = NULL,
  legend_position = "none",
  legend_key_width = 7,
  legend_key_height = 7,
  legend_text_size = 11,
  legend_key_shape = "square",

  # === COLOR SCHEME ===
  color = "#005b89",
  color_position = "highest",

  # === DATA BREAKS/CATEGORIES ===
  breaks_method = "equal",
  breaks_custom = NULL,
  breaks_num_group = 4,
  breaks_decimals = 0,

  # === DATA LABELS - BASIC ===
  data_label_show = TRUE,
  data_label_text_size = 3,
  data_label_text_color = "black",
  data_label_box = TRUE,
  data_label_box_color = "white",
  data_label_box_alpha = 1,
  data_label_decimals = 0,
  data_label_decimal = 0,

  # === DATA LABELS - GLOW EFFECT ===
  data_label_glow = FALSE,
  data_label_glow_color = "white",
  data_label_glow_sigma = 3,
  data_label_glow_expand = 1,

  # === DATA LABELS - CONTENT ===
  data_label_column = NULL,
  data_label_show_value = FALSE,
  data_label_separator = ": ",

  # === DATA LABELS - SMART POSITIONING ===
  data_label_repel = FALSE,
  data_label_force = 2,
  data_label_max_overlaps = 10,
  data_label_nudge_x = 0,
  data_label_nudge_y = 0,

  # === DATA LABELS - CONNECTING LINES ===
  data_label_line = TRUE,
  data_label_line_color = "gray50",
  data_label_line_size = 0.3,

  # === DATA LABELS - OUTSIDE MAP ===
  data_label_outside_map = FALSE,
  data_label_expand_factor = 0.5,

  # === DATA LABELS - FILTERING ===
  data_label_filter = NULL,
  data_label_filter_column = NULL,

  # === ADVANCED ===
  debug = FALSE,
  font = "Verdana",

  # ------------------------------------------------------------------
  # D3-tillägg: nya parametrar ligger efter originalets, så positionella
  # anrop träffar samma parametrar som i func_graf-originalet
  # ------------------------------------------------------------------

  # === D3: TIME / SLIDER ===
  time_column = NULL,
  time_default = NULL,
  time_label_show = TRUE,
  percentage_growth = FALSE,

  # === D3: MAP STYLING ===
  basemap_fit = TRUE,
  geom_id_column = NULL,
  geom_simplify_tolerance = NULL,

  # === D3: OVERLAY BOUNDARIES ===
  overlay = NULL,
  overlay_color = "#37474F",
  overlay_line_size = 0.6,
  overlay_label_column = NULL,
  overlay_label_size = 4,

  # === D3: BUBBLES ===
  bubble_column = NULL,
  bubble_color = "#ffc107",
  bubble_alpha = 0.8,
  bubble_max_size = 25,
  bubble_line_color = "white",

  # === D3: LEGEND ===
  legend_title_change = NULL,

  # === D3: COLOR SCHEME ===
  color_low = "white",
  color_negative = NULL,
  color_na = "grey80",

  # === D3: DATA BREAKS/CATEGORIES ===
  breaks_round_labels = FALSE,
  breaks_custom_change = NULL,

  # === D3: DATA LABELS - BASIC ===
  data_label_thousands_mark = " ",
  data_label_zoom = FALSE,
  data_label_zoom_overlap = 0.4,

  # === D3: TOOLTIP ===
  tooltip_show = TRUE,
  tooltip_name_column = NULL,
  tooltip_columns = NULL,
  tooltip_rank = TRUE,
  tooltip_rank_order = "desc",
  tooltip_class = TRUE,
  tooltip_value_label = NULL,
  tooltip_unit = NULL,
  tooltip_unit_change = NULL,
  tooltip_decimals = NULL,

  # === D3: INTERACTIVITY ===
  interactive_zoom = TRUE,
  interactive_zoom_group_column = NULL,
  interactive_search = TRUE,
  interactive_legend = TRUE,
  interactive_export = TRUE,
  interactive_mode_toggle = NULL,
  interactive_mode_labels = c(niva = "Nivå", forandring = "Förändring"),
  interactive_time_slider = TRUE,
  interactive_time_range = TRUE,
  interactive_resize = FALSE,
  interactive_exclude = NULL,

  # === D3: SUMMARY ===
  summary_text = NULL,
  summary_sum = NULL,
  summary_ratio = NULL,
  summary_ratio_scale = 100,
  summary_ratio_decimals = 1,
  summary_ratio_unit = " %",
  summary_count_label = c("område", "områden"),
  summary_extent = "inside",

  # === D3: ANIMATION ===
  animation_duration = 600,
  animation_play_interval = 1500,

  # === D3: SIZE ===
  width = 800,
  height = 900,
  responsive = TRUE,
  responsive_breakpoint = 600,
  resize_min = c(300, 250),
  resize_max = NULL,
  resize_keep_scale = TRUE,
  .rendera = FALSE
) {

  # Utan .rendera returneras ett vgr_d3-objekt med bara de angivna argumenten (så "+"-lager fungerar)
  if (!.rendera) {
    angivna <- setdiff(names(as.list(match.call()))[-1], ".rendera")
    obj <- func_d3_nytt_objekt("karta", mget(angivna, envir = environment()))
    if (debug) return(attr(func_d3_spec(obj), "debug"))
    return(obj)
  }

  # Resolve alternative parameter names (last-one-wins if both are supplied)
  title_size          <- if (!missing(title_text_size))    title_text_size    else title_size
  caption_size        <- if (!missing(caption_text_size))  caption_text_size  else caption_size
  legend_title        <- if (!missing(legend_name))        legend_name        else legend_title
  data_label_decimals <- if (!missing(data_label_decimal)) data_label_decimal else data_label_decimals
  # text_scale: en faktor för alla textstorlekar (som func_graf_karta)
  title_size           <- title_size           * text_scale
  caption_size         <- caption_size         * text_scale
  subtitle_text_size   <- subtitle_text_size   * text_scale
  legend_text_size     <- legend_text_size     * text_scale
  data_label_text_size <- data_label_text_size * text_scale
  # A categorical map without a legend is unreadable, so show it unless the user chose otherwise
  if (!is.null(group) && missing(legend_position)) legend_position <- "right"

  p <- mget(setdiff(names(formals()), c(".rendera", "title_text_size", "caption_text_size", "legend_name", "data_label_decimal", "colors")),
            envir = environment())
  # colors räknas bara ut med group (som i originalet), så att en vanlig gradientkarta inte räknar ut gruppfärgerna
  p$colors <- if (!is.null(group)) colors else NULL
  func_d3_karta_bygg(p, standardlage = "niva")
}

# ============================================================
#  BRYTPUNKTER OCH FÄRGER (delas med func_d3_karta_change)
# ============================================================

# dec_values: de värden som avgör om etiketterna behöver decimaler, om fler än de som
# klassgränserna räknas på (förändringsläget: alla år, inte bara det sista)
func_d3_karta_brytpunkter <- function(values, method, custom = NULL, n_groups = 4, decimals = 0, round_labels = FALSE,
                                      dec_values = values) {
  # Inf/-Inf (t.ex. division med noll) kan inte ge klassgränser — de hamnar ändå i yttersta klasserna via cut()
  # Med 15 värdesiffror, som värdena skrivs till JSON och visas: flyttalsbrus (2,0000000000001)
  # ska inte räknas som decimaler i etiketterna
  values <- signif(values[is.finite(values)], 15)
  if (length(values) == 0) stop("vgr_D3: inga ändliga värden — kan inte beräkna klassgränser.")
  min_val <- min(values, na.rm = TRUE)
  max_val <- max(values, na.rm = TRUE)

  # Min och max tas bort innan -Inf/Inf läggs till: n_groups inre punkter -> n_groups klasser via cut()
  inner <- switch(method,
                  "equal"    = { raw <- seq(min_val, max_val, length.out = n_groups + 1)
                                 round(raw[-c(1, length(raw))], decimals) },
                  "quantile" = { raw <- stats::quantile(values, probs = seq(0, 1, length.out = n_groups + 1), na.rm = TRUE, names = FALSE)
                                 round(raw[-c(1, length(raw))], decimals) },
                  "custom"   = { if (is.null(custom)) stop("Custom breaks required when breaks_method = 'custom'")
                                 sort(unique(custom[is.finite(custom)])) },   # -Inf/Inf läggs till nedan
                  stop("Invalid breaks_method. Choose 'equal', 'quantile', or 'custom'."))
  inner  <- unique(inner)
  breaks <- c(-Inf, inner, Inf)

  # Klassen omfattar sin nedre gräns men inte sin övre (cut med right = FALSE). Med
  # heltalsgränser skrivs den övre gränsen därför ett steg under nästa gräns: "40-44" när
  # värdena är heltal, "40,0-44,9" när de har decimaler (44,7 ingår ju i klassen).
  has_dec  <- any(inner != floor(inner))
  dp_grans <- if (has_dec) max(1, decimals) else 0   # gränsernas egna decimaler
  dec_values <- signif(dec_values[is.finite(dec_values)], 15)
  dp       <- if (has_dec || any(dec_values != round(dec_values))) max(1, decimals) else 0
  fmt      <- function(x, d = dp) format(x, nsmall = d, big.mark = " ", decimal.mark = ",",
                                       scientific = FALSE, trim = TRUE)

  n <- length(breaks)
  labels <- if (n == 2) {
    # En enda klass (n_groups = 1 eller alla värden lika)
    paste0(fmt(round(min_val, decimals), 0), "-", fmt(round(max_val, decimals), 0))
  } else {
    c(paste0("< ", fmt(breaks[2], dp_grans)),
      if (n > 3) vapply(2:(n - 2), function(i) {
        lower <- breaks[i]; upper <- breaks[i + 1]
        lo_disp <- if (round_labels && has_dec && i > 2) lower + 10^(-dp) else lower
        up_disp <- if (has_dec) upper else round(upper - 10^(-dp), dp)
        # Tankstreck när en gräns är negativ, annars blir det "-1,3--0,5"
        sep <- if (lower < 0 || up_disp < 0) " – " else "-"
        paste0(fmt(lo_disp), sep, fmt(up_disp))
      }, character(1)) else character(0),
      paste0(fmt(breaks[n - 1], dp_grans), " +"))
  }
  # decimals: etiketternas decimaler, så att tooltipens värde visas med minst lika många
  list(breaks = breaks, labels = labels, decimals = dp)
}

func_d3_karta_klass <- function(values, brp) {
  as.integer(cut(values, breaks = brp$breaks, labels = FALSE, include.lowest = TRUE, right = FALSE)) - 1L
}

func_d3_karta_farger <- function(brp, color, color_low, color_negative, color_position) {
  n <- length(brp$labels)
  if (n == 1) return(func_d3_farg(color))
  if (!is.null(color_negative)) {
    # Klasser vars övre gräns är <= 0 får den negativa skalan, övriga den positiva
    n_neg <- sum(brp$breaks[-1] <= 0)
    n_pos <- n - n_neg
    farger <- c(if (n_neg > 0) grDevices::colorRampPalette(c(color_negative, color_low))(n_neg + 1)[seq_len(n_neg)],
                if (n_pos > 0) grDevices::colorRampPalette(c(color_low, color))(n_pos + 1)[-1])
  } else {
    farger <- grDevices::colorRampPalette(if (color_position == "highest") c(color_low, color) else c(color, color_low))(n)
  }
  func_d3_farg(farger)
}

# ============================================================
#  SUMMERINGSRAD (summary_* och interactive_exclude)
# ============================================================

# Summor, kvoter och kryssrutor till raden under kartan. Värdena skickas per år som vektorer i
# samma ordning som reg_id (som level_values); karta.js räknar summorna över de områden som
# syns. NULL när varken summor, kvoter eller kryssrutor anges (även i func_d3_karta_change,
# som saknar parametrarna).
func_d3_karta_summering <- function(p, df_data, ar_av_rad, reg_id, ar) {
  summor <- p$summary_sum
  kvoter <- p$summary_ratio
  uteslut <- p$interactive_exclude
  if (is.null(summor) && is.null(kvoter) && is.null(uteslut)) return(NULL)

  namngiven <- function(x, arg) {
    if (!is.null(x) && (is.null(names(x)) || any(names(x) == "") || anyDuplicated(names(x))))
      stop("vgr_D3: ", arg, " måste ha unika namn, t.ex. ", switch(arg,
        summary_sum = 'c(bef = "bef2064")',
        summary_ratio = 'list(andel = c("sys_antal", "bef2064"))',
        interactive_exclude = 'c("Exkludera studentområden" = "studentomrade")'), ".")
  }
  namngiven(summor, "summary_sum")
  namngiven(kvoter, "summary_ratio")
  namngiven(uteslut, "interactive_exclude")
  for (k in c(unname(summor), unlist(kvoter, use.names = FALSE))) {
    if (!is.numeric(df_data[[k]])) stop("vgr_D3: kolumnen '", k, "' i summary_sum/summary_ratio måste vara numerisk.")
  }
  if (!is.null(kvoter) && !all(vapply(kvoter, function(x) is.character(x) && length(x) == 2, logical(1))))
    stop("vgr_D3: varje kvot i summary_ratio ska vara två kolumnnamn (täljare, nämnare), t.ex. c(\"sys_antal\", \"bef2064\").")
  if (length(intersect(names(summor), names(kvoter))) > 0)
    stop("vgr_D3: summary_sum och summary_ratio har samma namn: ", toString(intersect(names(summor), names(kvoter))), ".")
  if (!p$summary_extent %in% c("inside", "intersects"))
    stop("vgr_D3: summary_extent måste vara \"inside\" eller \"intersects\".")
  if (!is.character(p$summary_count_label) || length(p$summary_count_label) != 2)
    stop("vgr_D3: summary_count_label ska vara två ord (singular och plural), t.ex. c(\"område\", \"områden\").")

  per_ar <- function(kol) func_d3_per_ar_vektor(df_data[[kol]], df_data$.id, ar_av_rad, reg_id, ar)

  # Standardmall: alla summor och kvoter i den ordning de angavs, och antalet områden
  mall <- p$summary_text
  if (is.null(mall) && (length(summor) + length(kvoter)) > 0) {
    mall <- paste0("Områdena som visas, {period}: ",
                   paste0("{", c(names(summor), names(kvoter)), "}", collapse = " · "), " · {antal}")
  }

  # Kryssrutorna: områden där kolumnen är TRUE något år (som index från 0 i reg_id)
  kryss <- lapply(names(uteslut), function(etikett) {
    v <- df_data[[uteslut[[etikett]]]]
    if (!is.logical(v))
      stop("vgr_D3: kolumnen '", uteslut[[etikett]], "' i interactive_exclude måste vara logisk (TRUE/FALSE), ",
           "t.ex. mutate(", uteslut[[etikett]], " = ", uteslut[[etikett]], " == \"ja\").")
    i <- which(reg_id %in% df_data$.id[v %in% TRUE]) - 1L
    list(label = etikett, index = if (length(i) == 1) I(i) else i)
  })

  list(
    text = mall,
    sums = lapply(names(summor), function(n) {
      kol <- df_data[[summor[[n]]]]
      heltal <- all(is.na(kol) | kol == round(kol))
      list(key = n, decimals = if (heltal) 0 else 1, values = per_ar(summor[[n]]))
    }),
    ratios = lapply(names(kvoter), function(n) {
      list(key = n, num = per_ar(kvoter[[n]][1]), den = per_ar(kvoter[[n]][2]))
    }),
    ratio_scale = p$summary_ratio_scale,
    ratio_decimals = p$summary_ratio_decimals,
    ratio_unit = p$summary_ratio_unit,
    count_label = as.list(p$summary_count_label),
    extent = p$summary_extent,
    thousands = p$data_label_thousands_mark %||% " ",
    color = func_d3_farg(p$color %||% "#005b89"),
    exclude = kryss
  )
}

# ============================================================
#  BYGG SPEC (delas med func_d3_karta_change)
# ============================================================

func_d3_karta_bygg <- function(p, standardlage = "niva") {

  # --- Validering ---
  if (is.null(p$df) || is.null(p$geom_col)) stop("Missing required arguments")
  if (is.character(p$df)) stop("df must be a data frame, not a text string. Write df = ", p$df[1], " (without quotes).")
  if (is.null(p$df_value) && is.null(p$group)) {
    stop(if (standardlage == "forandring") "Missing required arguments: df_value" else "Either df_value or group must be given")
  }
  kolumner <- names(p$df)
  saknas   <- setdiff(c(p$df_value, p$group, p$geom_col, p$time_column, p$data_label_column, p$bubble_column,
                        p$interactive_zoom_group_column, p$tooltip_name_column, p$geom_id_column,
                        unname(p$tooltip_columns), unname(p$interactive_exclude), unname(p$summary_sum),
                        unlist(p$summary_ratio, use.names = FALSE)), kolumner)
  if (length(saknas) > 0) stop("Missing columns in data: ", paste(saknas, collapse = ", "))
  if (!is.null(p$df_value) && !is.numeric(p$df[[p$df_value]])) stop("vgr_D3: df_value ('", p$df_value, "') måste vara numerisk.")
  if (!p$legend_position %in% c("none", "top", "bottom", "left", "right")) stop("legend_position must be one of: none, top, bottom, left, right")
  if (!is.null(p$responsive) && !(is.logical(p$responsive) && length(p$responsive) == 1 && !is.na(p$responsive)))
    stop("vgr_D3: responsive måste vara TRUE eller FALSE.")
  rb <- p$responsive_breakpoint
  if (!is.null(rb) && !(is.numeric(rb) && length(rb) == 1 && !is.na(rb) && rb >= 0))
    stop("vgr_D3: responsive_breakpoint måste vara ett tal (pixlar) som är 0 eller större.")
  if (!is.null(p$interactive_resize) && !(is.logical(p$interactive_resize) && length(p$interactive_resize) == 1 &&
                                          !is.na(p$interactive_resize)))
    stop("vgr_D3: interactive_resize måste vara TRUE eller FALSE.")
  if (!is.null(p$resize_keep_scale) && !(is.logical(p$resize_keep_scale) && length(p$resize_keep_scale) == 1 &&
                                         !is.na(p$resize_keep_scale)))
    stop("vgr_D3: resize_keep_scale måste vara TRUE eller FALSE.")
  for (rz in c("resize_min", "resize_max")) {
    v <- p[[rz]]
    if (!is.null(v) && !(is.numeric(v) && length(v) == 2 && !anyNA(v) && all(v > 0)))
      stop("vgr_D3: ", rz, " måste vara två positiva tal (bredd och höjd i pixlar), t.ex. c(300, 250).")
  }
  rz_min <- p$resize_min %||% c(300, 250)
  if (!is.null(p$resize_max) && any(p$resize_max < rz_min))
    stop("vgr_D3: resize_max får inte vara mindre än resize_min.")
  zo <- p$data_label_zoom_overlap
  if (!is.numeric(zo) || length(zo) != 1 || is.na(zo) || zo < 0 || zo > 0.9) {
    stop("vgr_D3: data_label_zoom_overlap måste vara ett tal mellan 0 och 0.9.")
  }
  tid <- p$time_column

  # --- Geometri: sf med geom_col som aktiv geometri, projicerat koordinatsystem ---
  df <- sf::st_as_sf(as.data.frame(p$df), sf_column_name = p$geom_col)
  tomma <- sf::st_is_empty(df)
  if (any(tomma)) {
    warning("vgr_D3: ", sum(tomma), " rader med tom geometri tas bort.")
    df <- df[!tomma, ]
  }
  # Saknat koordinatsystem men värden som ser ut som lon/lat: anta WGS84 (annars avrundas polygonerna bort)
  if (is.na(sf::st_crs(df))) {
    bb <- sf::st_bbox(df)
    if (bb["xmin"] >= -180 && bb["xmax"] <= 180 && bb["ymin"] >= -90 && bb["ymax"] <= 90) {
      warning("vgr_D3: geometrin saknar koordinatsystem men ser ut som lon/lat — antar EPSG:4326.")
      sf::st_crs(df) <- 4326
    }
  }
  if (isTRUE(sf::st_is_longlat(df))) df <- sf::st_transform(df, 3006)   # lon/lat ritas förvrängt med geoIdentity
  crs <- sf::st_crs(df)
  forbered_lager <- function(x) {
    if (is.null(x)) return(NULL)
    x <- sf::st_as_sf(x)
    if (is.na(sf::st_crs(x)) && !is.na(crs)) {
      bb <- sf::st_bbox(x)
      # Lager utan koordinatsystem: lon/lat-värden antas vara WGS84, annars samma system som data
      sf::st_crs(x) <- if (bb["xmin"] >= -180 && bb["xmax"] <= 180 && bb["ymin"] >= -90 && bb["ymax"] <= 90) 4326 else crs
    }
    if (!is.na(crs) && !is.na(sf::st_crs(x)) && sf::st_crs(x) != crs) x <- sf::st_transform(x, crs)
    x
  }
  basemap <- forbered_lager(p$basemap)
  overlay <- forbered_lager(p$overlay)

  # --- Region-id: samma region ska kännas igen mellan år ---
  df_data  <- sf::st_drop_geometry(df)
  id_kol   <- p$geom_id_column %||% intersect(c("region_kod", "kommunkod", "kommun_kod", "regionkod", "kommun", "kommunnamn", "region"), kolumner)[1]
  if (is.na(id_kol)) {
    punkter    <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(df)))
    df_data$.id <- paste(round(punkter[, 1]), round(punkter[, 2]), sep = "_")
  } else {
    df_data$.id <- as.character(df_data[[id_kol]])
  }
  nyckel <- if (is.null(tid)) df_data$.id else paste(df_data$.id, df_data[[tid]])
  if (anyDuplicated(nyckel)) {
    stop("vgr_D3: flera rader per region", if (is.null(tid)) " — ange time_column om data innehåller flera år" else " och år",
         ". Id-kolumn: ", if (is.na(id_kol)) "(geometri)" else id_kol, " (ändra med geom_id_column).")
  }

  forsta_rad <- !duplicated(df_data$.id)
  regioner   <- df[forsta_rad, ]
  reg_data   <- df_data[forsta_rad, ]
  reg_id     <- reg_data$.id

  namn_kol <- p$tooltip_name_column %||% intersect(c("kommunnamn", "kommun", "region", "namn", "lansnamn", "name"), kolumner)[1]
  reg_namn <- if (is.na(namn_kol)) reg_id else as.character(reg_data[[namn_kol]])

  ankare <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(regioner)))
  # Etikettpunkter som platt vektor (x1, y1, x2, y2, ...) i samma ordning som reg_id
  ankare_vektor <- as.numeric(t(round(ankare[, 1:2, drop = FALSE], 1)))

  # --- År ---
  ar <- if (is.null(tid)) "all" else as.character(sort(unique(df_data[[tid]])))
  ar_av_rad <- if (is.null(tid)) rep("all", nrow(df_data)) else as.character(df_data[[tid]])

  # Värden per år: list(år = vektor i samma ordning som reg_id). Kompakt kodning: id:na
  # skickas en gång (spec$ids) i stället för vid varje värde; karta.js packar upp.
  per_ar <- function(data, rad_ar, kol, ar_lista) {
    func_d3_per_ar_vektor(data[[kol]], data$.id, rad_ar, reg_id, ar_lista)
  }
  har_na <- function(data, rad_ar, klass_kol, ar_lista) {
    any(is.na(data[[klass_kol]][rad_ar %in% ar_lista])) ||
      any(vapply(ar_lista, function(a) sum(rad_ar == a) < length(reg_id), logical(1)))
  }

  # Tooltipens decimaler (gemensam regel, minst lika många som klassgränserna); förändringsläget
  # räknar sina egna nedan eftersom förändringar sällan är heltal
  varden_alla  <- if (!is.null(p$df_value)) df_data[[p$df_value]] else numeric(0)   # grupperad karta utan df_value
  # breaks_decimals gäller inte grupperad karta (inga klassgränser)
  tt_decimaler <- func_d3_tooltip_decimaler(p$tooltip_decimals, varden_alla, p$data_label_decimals,
                                            if (is.null(p$group)) p$breaks_decimals)
  # Extra kolumner och bubblor avgörs per kolumn; heltalskolumner visas alltid utan decimaler
  kolumn_decimaler <- function(x) {
    heltal <- is.numeric(x) && all(is.na(x) | !is.finite(x) | x == round(x))
    func_d3_tooltip_decimaler(if (heltal) 0 else p$tooltip_decimals, x, p$data_label_decimals)
  }

  # --- Lägen: nivå och förändring ---
  mode_toggle <- p$interactive_mode_toggle %||% (!is.null(tid) && length(ar) >= 2)
  # Grupperad karta: förändring mellan år går inte att räkna på kategorier
  if (!is.null(p$group)) {
    if (isTRUE(p$interactive_mode_toggle)) message("vgr_D3: förändringsläget finns inte när group är angiven — visar bara grupperna.")
    mode_toggle <- FALSE
  }
  lagen_att_bygga <- unique(c(standardlage, if (isTRUE(mode_toggle)) c("niva", "forandring")))
  if ("forandring" %in% lagen_att_bygga && (is.null(tid) || length(ar) < 2)) {
    if (standardlage == "forandring")
      stop("vgr_D3: förändringskartan kräver en tidskolumn med minst två år (time_column/year_column).")
    lagen_att_bygga <- setdiff(lagen_att_bygga, "forandring")
  }
  # Knapptexter: namngiven vektor, eller onamngiven i ordningen nivå, förändring
  lagetexter <- p$interactive_mode_labels
  if (is.null(names(lagetexter))) names(lagetexter) <- c("niva", "forandring")[seq_along(lagetexter)]

  # Det extra läget (knappen) får inte stoppa huvudkartan: egna klassgränser (breaks_custom) är
  # gjorda för huvudläget, så det extra läget använder lika intervall; går det inte att räkna
  # (t.ex. ett år helt utan värden) hoppas läget över med ett meddelande. Förändringsläget kan
  # få egna klassgränser med breaks_custom_change (finns bara i func_d3_karta).
  brytpunkter_for <- function(lage, values, dec_values = values) {
    sekundart <- lage != standardlage
    egna_forandring <- if (lage == "forandring") p$breaks_custom_change else NULL
    metod <- if (!is.null(egna_forandring)) "custom"
             else if (sekundart && p$breaks_method == "custom") "equal" else p$breaks_method
    tryCatch(func_d3_karta_brytpunkter(values, metod, egna_forandring %||% p$breaks_custom, p$breaks_num_group,
                                       p$breaks_decimals, p$breaks_round_labels, dec_values),
             error = function(e) {
               if (!sekundart) stop(e)
               message("vgr_D3: läget '", lage, "' visas inte: ", conditionMessage(e))
               NULL
             })
  }

  lagen <- list()
  debug_info <- list()

  if (!is.null(p$group)) {
    # --- Grupperad karta: varje grupp är en egen klass med egen färg (samma regel som func_graf_karta) ---
    g <- df_data[[p$group]]
    if (is.numeric(g)) g <- factor(g)   # numeriska grupper sorteras numeriskt (1, 2, 10)
    # Etiketter från gruppkolumnen visar nivåtexten ("1.5", "2020") som legenden, inte avrundat/tusentalsformaterat
    # (som originalet). Egen kolumn, så att tooltip_columns/bubble_column på samma kolumn behåller sina tal
    df_data$.grupp_text <- as.character(g)
    grupp_nivaer <- if (is.factor(g)) setdiff(levels(droplevels(g)), NA) else unique(stats::na.omit(as.character(g)))
    farger <- p$colors %||% func_d3_farger_ru()
    if (!is.null(names(farger))) {
      saknas_farg <- setdiff(grupp_nivaer, names(farger))
      if (length(saknas_farg) > 0) stop("colors is named but has no color for: ", toString(saknas_farg))
      farger <- farger[grupp_nivaer]
    } else {
      farger <- rep(farger, length.out = length(grupp_nivaer))
    }
    df_data$.klass_niva <- match(as.character(g), grupp_nivaer) - 1L   # NA-grupp -> NA -> color_na
    leg_titel <- p$legend_title %||% p$group
    lagen$niva <- list(
      label        = unname(ifelse(is.na(lagetexter["niva"]), "Nivå", lagetexter["niva"])),
      class_labels = as.list(grupp_nivaer),
      colors       = as.list(func_d3_farg(unname(farger))),
      has_na       = har_na(df_data, ar_av_rad, ".klass_niva", ar),
      years        = as.list(ar),
      has_values   = !is.null(p$df_value),   # utan df_value: ingen värderad eller rangordning i tooltip
      # Värdena hämtas ur spec$level_values; gruppernas klasser går inte att räkna fram ur
      # värdena och skickas därför
      classes      = per_ar(df_data, ar_av_rad, ".klass_niva", ar),
      subtitles    = stats::setNames(lapply(ar, function(a) func_d3_rader(p$subtitle)), ar),
      legend_title = leg_titel,
      class_row_label = leg_titel,           # tooltipens "Klass"-rad heter som gruppkolumnen/legendtiteln
      value_label  = p$tooltip_value_label %||% p$df_value %||% "",
      unit         = p$tooltip_unit %||% "",
      decimals     = tt_decimaler
    )
    debug_info$group_levels <- grupp_nivaer
  }

  brp_niva <- if ("niva" %in% lagen_att_bygga && is.null(p$group)) brytpunkter_for("niva", df_data[[p$df_value]]) else NULL
  if (!is.null(brp_niva)) {
    brp   <- brp_niva
    df_data$.klass_niva <- func_d3_karta_klass(df_data[[p$df_value]], brp)
    leg_titel <- p$legend_title
    lagen$niva <- list(
      label        = unname(ifelse(is.na(lagetexter["niva"]), "Nivå", lagetexter["niva"])),
      class_labels = as.list(brp$labels),
      colors       = as.list(func_d3_karta_farger(brp, p$color, p$color_low, p$color_negative, p$color_position)),
      has_na       = har_na(df_data, ar_av_rad, ".klass_niva", ar),
      years        = as.list(ar),
      # Värdena hämtas ur spec$level_values och klasserna räknas i webbläsaren från de inre
      # klassgränserna, med samma regel som func_d3_karta_klass()
      breaks       = as.list(brp$breaks[is.finite(brp$breaks)]),
      subtitles    = stats::setNames(lapply(ar, function(a) func_d3_rader(p$subtitle)), ar),
      legend_title = leg_titel,
      value_label  = p$tooltip_value_label %||% leg_titel %||% p$df_value,
      unit         = p$tooltip_unit %||% "",
      # Minst lika många decimaler som klassetiketterna, så att 44,7 inte visas som "45" i
      # klassen "40,0-44,9" (gäller inte ett angivet tooltip_decimals)
      decimals     = func_d3_tooltip_decimaler(p$tooltip_decimals, varden_alla, p$data_label_decimals,
                                               max(p$breaks_decimals, brp$decimals))
    )
    debug_info$breaks_niva <- brp
  }

  if ("forandring" %in% lagen_att_bygga) {
    forsta <- ar[1]
    bas    <- df_data[ar_av_rad == forsta, c(".id", p$df_value)]
    names(bas)[2] <- ".bas"
    fd     <- df_data[ar_av_rad != forsta, ]
    fd$.bas <- bas$.bas[match(fd$.id, bas$.id)]
    # Samma beräkning som karta.js: på nivåvärdena som de skrivs till JSON (15 värdesiffror) och
    # med resultatet avrundat till 15 värdesiffror (toPrecision(15)). Klassgränserna räknas då på
    # samma värden som webbläsaren klassar och visar.
    fran <- signif(fd$.bas, 15); till <- signif(fd[[p$df_value]], 15)
    fd$.forandring <- till - fran
    if (p$percentage_growth) fd$.forandring <- fd$.forandring / fran * 100
    fd$.forandring[!is.finite(fd$.forandring)] <- NA
    fd$.forandring <- signif(fd$.forandring, 15)
    fd_ar  <- as.character(fd[[tid]])
    ar_for <- ar[-1]

    # Klassgränser från senaste året, som func_graf_karta_change (förändring första -> sista år).
    # Om etiketterna behöver decimaler avgörs av förändringarna alla år, och vid absolut
    # förändring av varje områdes värden räknat från dess första år med värde: förändringen
    # mellan två valfria år (startår i interactive_time_range) är en skillnad mellan sådana
    # värden, även för områden som saknar värde första året. Procentuell förändring: se
    # open_issues.md.
    dec_forandring <- fd$.forandring
    # Startåret går bara att byta när tidsreglaget har två handtag (karta.js: spec.time.slider
    # och spec.time.range)
    startar_valbart <- isTRUE(p$interactive_time_slider) && isTRUE(p$interactive_time_range)
    if (!p$percentage_growth && startar_valbart) {
      niva <- signif(df_data[[p$df_value]], 15)
      o    <- order(df_data$.id, match(ar_av_rad, ar))
      forsta_varde <- stats::ave(niva[o], df_data$.id[o], FUN = function(x) x[!is.na(x)][1])
      dec_forandring <- c(dec_forandring, signif(niva[o] - forsta_varde, 15))
    }
    brp <- brytpunkter_for("forandring", fd$.forandring[fd_ar == ar[length(ar)]], dec_forandring)
  }
  if ("forandring" %in% lagen_att_bygga && !is.null(brp)) {
    fd$.klass_for <- func_d3_karta_klass(fd$.forandring, brp)
    leg_titel <- p$legend_title_change %||% p$legend_title
    lagen$forandring <- list(
      label        = unname(ifelse(is.na(lagetexter["forandring"]), "Förändring", lagetexter["forandring"])),
      class_labels = as.list(brp$labels),
      colors       = as.list(func_d3_karta_farger(brp, p$color, p$color_low, p$color_negative, p$color_position)),
      # Med valbart startår (interactive_time_range) kan alla år som saknar värden ge saknade
      # förändringar
      has_na       = har_na(fd, fd_ar, ".klass_for", ar_for) ||
        (isTRUE(p$interactive_time_range) && har_na(df_data, ar_av_rad, p$df_value, ar)),
      years        = as.list(ar_for),
      all_years    = as.list(ar),
      # Förändring, från- och tillvärden och klasser räknas i webbläsaren ur spec$level_values
      # (samma beräkning som .forandring ovan); klassgränserna kommer härifrån
      breaks       = as.list(brp$breaks[is.finite(brp$breaks)]),
      percentage   = isTRUE(p$percentage_growth),
      from_year    = forsta,
      subtitles    = stats::setNames(lapply(ar_for, function(a) {
        func_d3_rader(paste0(if (!is.null(p$subtitle)) paste0(p$subtitle, " "), forsta, "-", a), p$title_width)
      }), ar_for),
      legend_title = leg_titel,
      value_label  = p$tooltip_value_label %||% (if (p$percentage_growth) "Förändring (%)" else "Förändring"),
      # Procentuell förändring har alltid " %" (som linjediagrammets förändringsläge); en egen
      # tooltip_unit gäller nivåvärden och absolut förändring. tooltip_unit_change går före
      # båda (t.ex. " procentenheter"). Från/till-raderna är nivåvärden.
      unit         = p$tooltip_unit_change %||% (if (p$percentage_growth) " %" else (p$tooltip_unit %||% "")),
      level_unit   = p$tooltip_unit %||% "",
      decimals     = func_d3_tooltip_decimaler(p$tooltip_decimals, fd$.forandring, p$data_label_decimals,
                                               max(p$breaks_decimals, brp$decimals))
    )
    debug_info$breaks_forandring <- brp
    debug_info$forandring <- fd
  }

  # --- Standardår ---
  ar_standard <- unlist(lagen[[standardlage]]$years)
  standard_ar <- if (!is.null(p$time_default) && as.character(p$time_default) %in% ar_standard) as.character(p$time_default) else ar_standard[length(ar_standard)]

  # --- Dataetiketter ---
  filter_kol <- p$data_label_filter_column %||%
    (if ("region_kod" %in% kolumner) "region_kod" else if ("kommun" %in% kolumner) "kommun" else kolumner[1])
  etikett_id <- reg_id
  if (!is.null(p$data_label_filter)) {
    traffar <- unique(df_data$.id[df_data[[filter_kol]] %in% p$data_label_filter])
    if (length(traffar) == 0) {
      warning("No matching regions found. Available: ", toString(head(unique(df_data[[filter_kol]]), 10)))
    } else etikett_id <- traffar
  }
  etikett_prefix <- NULL
  # Etiketter: data_label_column, annars df_value (värdet), annars gruppkolumnen (NA-grupp får ingen etikett)
  etikett_kol <- p$data_label_column %||% (if (is.null(p$df_value)) p$group)
  if (!is.null(p$group) && identical(etikett_kol, p$group)) etikett_kol <- ".grupp_text"
  if (!is.null(etikett_kol)) {
    etikett_prefix <- func_d3_kompakt_kolumn(df_data[[etikett_kol]], df_data$.id, ar_av_rad, reg_id, ar,
                                             p$data_label_decimals, p$data_label_thousands_mark %||% "")
  }

  # --- Tooltip-kolumner ---
  tt_kolumner <- list()
  if (!is.null(p$tooltip_columns)) {
    etiketter <- names(p$tooltip_columns) %||% rep("", length(p$tooltip_columns))
    etiketter[etiketter == ""] <- p$tooltip_columns[etiketter == ""]
    tt_kolumner <- lapply(seq_along(p$tooltip_columns), function(i) {
      # Decimaler för hela kolumnen (alla år), så att de inte hoppar när tidsreglaget flyttas;
      # heltalskolumner (t.ex. befolkning) visas alltid utan decimaler
      dec <- kolumn_decimaler(df_data[[p$tooltip_columns[i]]])
      c(list(label = etiketter[i]),
        func_d3_kompakt_kolumn(df_data[[p$tooltip_columns[i]]], df_data$.id, ar_av_rad, reg_id, ar, dec))
    })
  }

  # --- Bubblor ---
  bubblor <- NULL
  if (!is.null(p$bubble_column)) {
    if (!is.numeric(df_data[[p$bubble_column]])) stop("vgr_D3: bubble_column måste vara numerisk.")
    bubblor <- list(label = p$bubble_column,
                    in_tooltip = !p$bubble_column %in% p$tooltip_columns,   # visas redan som tooltip-kolumn annars
                    values = per_ar(df_data, ar_av_rad, p$bubble_column, ar),
                    decimals = kolumn_decimaler(df_data[[p$bubble_column]]),
                    max_value = max(abs(df_data[[p$bubble_column]]), na.rm = TRUE),
                    color = func_d3_farg(p$bubble_color), alpha = p$bubble_alpha,
                    max_size = p$bubble_max_size, line_color = func_d3_farg(p$bubble_line_color))
  }

  # --- Summeringsrad och kryssrutor under kartan ---
  summering <- func_d3_karta_summering(p, df_data, ar_av_rad, reg_id, ar)

  # --- Överlagrade gränser ---
  # Kompakt geometri på ett gemensamt rutnät; lager som inte är ytor skickas som GeoJSON
  rutnat <- func_d3_geo_rutnat(regioner, basemap, overlay)
  geo_lager <- function(x, id = NULL) {
    if (is.null(x)) return(NULL)
    func_d3_geo_kompakt(x, rutnat, p$geom_simplify_tolerance) %||%
      func_d3_geojson(x, id = id, toleransen = p$geom_simplify_tolerance)
  }

  overlay_spec <- NULL
  if (!is.null(overlay)) {
    overlay_etiketter <- list()
    if (!is.null(p$overlay_label_column)) {
      op <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(overlay)))
      overlay_etiketter <- lapply(seq_len(nrow(op)), function(i) {
        list(x = round(op[i, 1], 1), y = round(op[i, 2], 1), text = as.character(overlay[[p$overlay_label_column]][i]))
      })
    }
    overlay_spec <- list(geo = geo_lager(overlay),
                         color = func_d3_farg(p$overlay_color),
                         line_size = p$overlay_line_size * 72.27 / 25.4,
                         labels = overlay_etiketter,
                         label_size = func_d3_mm_till_px(p$overlay_label_size * (p$text_scale %||% 1)))
  }

  zoomgrupper <- if (!is.null(p$interactive_zoom_group_column))
    I(as.character(reg_data[[p$interactive_zoom_group_column]])) else NULL

  # --- Spec till JavaScript ---
  spec <- list(
    typ = "karta", js_fil = "karta",
    text_scale = p$text_scale %||% 1,   # för årsstämpeln i JS (karta_change har samma parameter)
    width = p$width, height = p$height, font = func_d3_typsnitt(p$font),
    margin = list(t = func_d3_pt_till_px(p$margin_top), r = func_d3_pt_till_px(p$margin_right),
                  b = func_d3_pt_till_px(p$margin_bottom), l = func_d3_pt_till_px(p$margin_left)),
    text = list(title = func_d3_rader(p$title, p$title_width),
                title_size = func_d3_pt_till_px(p$title_size),
                title_margin_top = func_d3_pt_till_px(p$title_margin_top),
                title_position = p$title_position,
                subtitle_size = func_d3_pt_till_px(p$subtitle_text_size),
                caption = func_d3_rader(p$caption),
                caption_size = func_d3_pt_till_px(p$caption_size)),
    # Kompakt kodning (packas upp av karta.js): områdenas id och namn en gång, värden som
    # vektorer i samma ordning, geometrin som deltakodade heltal
    compact = TRUE,
    ids = I(reg_id),
    names = I(reg_namn),
    level_values = if (!is.null(p$df_value)) per_ar(df_data, ar_av_rad, p$df_value, ar) else NULL,
    # Områden (index från 0) som saknar rad ett visst år. Konstanta tooltipkolumner och
    # förändringslägets frånvärde visas inte de åren, som när allt skickades per år.
    missing_rows = func_d3_saknade_rader(df_data$.id, ar_av_rad, reg_id, ar),
    geo = geo_lager(regioner, id = reg_id),
    basemap = geo_lager(basemap),
    basemap_color = func_d3_farg(p$basemap_color),
    basemap_color_line = func_d3_farg(p$basemap_color_line),
    basemap_fit = p$basemap_fit,
    data_color_line = func_d3_farg(p$data_color_line),
    data_line_size = p$data_line_size * 72.27 / 25.4,   # ggplot linewidth (mm-baserad) -> px
    na_color = func_d3_farg(p$color_na),
    na_label = "Uppgift saknas",
    modes = lagen,
    mode_order = as.list(names(lagen)),
    default_mode = standardlage,
    default_year = standard_ar,
    legend = list(position = p$legend_position,
                  key_w = func_d3_mm_till_px(p$legend_key_width),
                  key_h = func_d3_mm_till_px(p$legend_key_height),
                  text_size = func_d3_pt_till_px(p$legend_text_size),
                  shape = p$legend_key_shape),
    labels = list(show = p$data_label_show,
                  ids = as.list(etikett_id),
                  anchors = ankare_vektor,
                  prefix = etikett_prefix,
                  # Värdet läggs bara till en egen data_label_column, och bara när det finns ett df_value
                  show_value = p$data_label_show_value && !is.null(p$data_label_column) && !is.null(p$df_value),
                  separator = p$data_label_separator,
                  decimals = p$data_label_decimals,
                  thousands = p$data_label_thousands_mark,
                  size = func_d3_mm_till_px(p$data_label_text_size),
                  # "auto": färgen väljs i webbläsaren efter områdets fyllnadsfärg
                  color = if (identical(p$data_label_text_color, "auto")) "auto" else func_d3_farg(p$data_label_text_color),
                  zoom = isTRUE(p$data_label_zoom),
                  zoom_overlap = p$data_label_zoom_overlap,
                  box = p$data_label_box,
                  box_color = func_d3_farg(p$data_label_box_color),
                  box_alpha = p$data_label_box_alpha,
                  glow = p$data_label_glow,
                  glow_color = func_d3_farg(p$data_label_glow_color),
                  glow_width = p$data_label_glow_sigma + p$data_label_glow_expand,
                  repel = p$data_label_repel,
                  force = p$data_label_force,
                  max_overlaps = p$data_label_max_overlaps,
                  nudge_x = p$data_label_nudge_x,
                  nudge_y = p$data_label_nudge_y,
                  line = p$data_label_line,
                  line_color = func_d3_farg(p$data_label_line_color),
                  line_size = p$data_label_line_size * 72.27 / 25.4,
                  outside_map = p$data_label_outside_map && p$data_label_repel,
                  expand_factor = p$data_label_expand_factor),
    tooltip = list(show = p$tooltip_show,
                   columns = tt_kolumner,
                   rank = p$tooltip_rank,
                   rank_order = p$tooltip_rank_order,
                   class = isTRUE(p$tooltip_class),
                   # Nivåvärden (förändringslägets rader för start- och slutår) med samma
                   # decimaler som nivålägets värderad
                   decimals = lagen$niva$decimals %||% tt_decimaler),
    bubbles = bubblor,
    summary = summering,
    overlay = overlay_spec,
    zoom_groups = zoomgrupper,
    time = list(enabled = !is.null(tid) && length(ar) >= 2,
                label_show = p$time_label_show,
                # Som range: TRUE/FALSE, så att karta.js och R (startar_valbart) bedömer lika
                slider = isTRUE(p$interactive_time_slider),
                range = isTRUE(p$interactive_time_range)),
    interactive = list(zoom = p$interactive_zoom,
                       search = p$interactive_search,
                       legend = p$interactive_legend,
                       export = p$interactive_export,
                       mode_toggle = length(lagen) > 1),
    animation = list(duration = p$animation_duration,
                     play_interval = p$animation_play_interval),
    responsive = list(enabled = isTRUE(p$responsive %||% TRUE),
                      breakpoint = p$responsive_breakpoint %||% 600),
    resize = list(enabled = isTRUE(p$interactive_resize),
                  keep_scale = isTRUE(p$resize_keep_scale %||% TRUE),
                  min = as.numeric(rz_min),
                  max = if (is.null(p$resize_max)) NULL else as.numeric(p$resize_max))
  )

  if (isTRUE(p$debug)) {
    attr(spec, "debug") <- c(list(data = df_data, id_column = id_kol, name_column = namn_kol), debug_info,
                             list(spec = spec))
  }
  spec
}
