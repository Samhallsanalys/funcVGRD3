# funcVGRD3 0.14.0 (2026-11-18)

## Nytt

* `func_d3_karta()` och `func_d3_karta_change()`: ny parameter `tooltip_class`. `FALSE` tar
  bort raden med områdets klass (eller grupp) ur tooltipen. Standard är `TRUE`, som förut.
  Finns också i `d3_tooltip(class = FALSE)`.

## Ändrat

* `func_d3_karta()` och `func_d3_karta_change()`: förklaringens klassetiketter visar vilka
  värden som ingår i klassen. En klass omfattar sin nedre gräns men inte sin övre. När
  klassgränserna är heltal men värdena har decimaler skrivs klasserna nu med en decimal
  (minst `breaks_decimals`): "40,0-44,9", "-5,0 – -2,1" och "0,0-1,9" i stället för
  "40-44", "-5 – -3" och "0-1". Förut såg en minskning på 0,4 ut att höra till "-2 – -1"
  och en ökning på 1,8 till "0-1". Med heltalsvärden är etiketterna oförändrade ("40-44"),
  liksom de öppna klasserna ("< 40", "80 +") och etiketterna när gränserna själva har
  decimaler. I förändringsläget avgör alla år (inte bara det sista) om etiketterna får
  decimaler. Kartor där värdena har decimaler får alltså andra etiketter än i 0.13.0.
* Tooltipens värde visas med minst lika många decimaler som klassetiketterna (om inte
  `tooltip_decimals` anges), så att t.ex. 9,6 inte visas som "10" i klassen "7,0-9,9".
* `tooltip_class = NA` eller `NULL` tar bort klassraden, som `tooltip_rank`.
* `interactive_time_slider` visar tidsreglaget bara när värdet är `TRUE`, som
  `interactive_time_range`. Förut visade också t.ex. `1` reglaget.

## Rättat

* Förändringsläget: förändringen räknas med 15 värdesiffror både i R och i webbläsaren, och
  klassen räknas på samma värde som visas i tooltipen. Förut kunde t.ex. 3,3 - 8,3 =
  -5,0000000000000009 visas som -5,0 men färgas som "< -5". R räknar nu förändringen på
  samma värden som webbläsaren (nivåvärdena med 15 värdesiffror), så att klassgränserna och
  klasserna stämmer även för kvoter med många decimaler.

# funcVGRD3 0.13.0 (2026-10-26)

## Nytt

* `func_d3_karta()`: summeringsrad under kartan som beskriver de områden som syns i kartytan
  just nu, t.ex. "Områdena som visas, 2023: 152 300 invånare · 81,4 % sysselsatta · 96
  områden". Raden räknas om när man zoomar, panorerar, byter år eller döljer klasser i
  förklaringen. Nya parametrar:
  * `summary_sum`: kolumner som summeras, t.ex. `c(bef = "bef2064")`.
  * `summary_ratio`: kvoter mellan två summor, t.ex. `list(andel = c("sys_antal", "bef2064"))`,
    med `summary_ratio_scale`, `summary_ratio_decimals` och `summary_ratio_unit`.
  * `summary_text`: mall för raden med `{period}`, `{antal}` och namnen i `summary_sum` och
    `summary_ratio`.
  * `summary_count_label` och `summary_extent` (områden som syns helt eller delvis).
* `func_d3_karta()`: ny parameter `interactive_exclude` som ger kryssrutor i summeringsraden.
  En ikryssad ruta tonar ned områdena i en logisk kolumn, tar bort deras etiketter och
  räknar bort dem i raden, t.ex. `c("Exkludera studentområden" = "studentomrade")`. Valet
  följer med när kartan ritas om i en ny storlek.

# funcVGRD3 0.12.1 (2026-10-23)

## Rättat

* `func_d3_karta()` och `func_d3_karta_change()` med `interactive_resize = TRUE`: den
  streckade ramen som visas när man drar omfattar nu bara kartytan. Tidigare omfattade den
  hela kartan med rubrik, förklaring och bildtext, vilket syntes tydligast på en smal
  skärm där förklaringen ligger under kartan.
* Draghandtaget sitter i kartytans nedre högra hörn, ovanför en förklaring eller
  bildtext under kartan. Tidigare satt det i nedre högra hörnet av hela kartan.
* Kartytan blir så hög som rutan man drog. Tidigare tog förklaringen och bildtexten en del
  av höjden, så att kartytan blev lägre än rutan, särskilt när förklaringen fick fler rader
  i den nya bredden.
* Kartan står still medan man drar och ritas om när man släpper. Med
  `resize_keep_scale = FALSE` ändrades tidigare storleken på hela kartan medan man drog,
  även rubrikens och förklaringens text.

## Dokumentation

* Hjälpsidorna för `func_d3_karta()` och `func_d3_karta_change()` beskriver var handtaget
  sitter och vad ramen visar, och att `resize_min` och `resize_max` gäller hela kartan med
  rubriker och förklaring.

# funcVGRD3 0.12.0 (2026-10-21)

## Nytt

* `func_d3_spridningsdiagram()` (även under namnet `func_d3_scatterplot()`): nytt
  interaktivt spridningsdiagram med samma parametrar som `func_graf_spridningsdiagram()` i
  funcVGR (grupper, bubblor, etiketter, trendlinje och referenslinjer, beräknade på samma
  sätt). Tilläggen:
  * Tooltipen visar punktens värden och kolumnerna i `tooltip_columns`.
  * Dra en ruta i diagrammet för att zooma (`interactive_zoom`).
  * Sökrutan hittar en punkt efter namnet i `data_label_column` (`interactive_search`).
  * Klicka i förklaringen för att dölja en grupp.
  * `interactive_filter_column` ger en rullista med uppspelning, t.ex. för år. Med
    `data_label_column` glider punkterna till sina nya lägen.
  * `x_break_line_label` och `y_break_line_label` skriver text vid referenslinjerna.
* `func_d3_histogram()`: nytt interaktivt histogram med samma parametrar och samma
  intervall som `func_graf_histogram()` i funcVGR. Tilläggen:
  * Knappar växlar mellan antal och andel (`interactive_y_toggle`) och, med `group`, mellan
    staplade, grupperade och överlagrade staplar (`interactive_bar_type_toggle`).
  * Tooltipen visar intervallet, antalet och andelen; `tooltip_unit` läggs efter
    intervallet.
  * `interactive_filter_column` ger en rullista med uppspelning där intervallen ligger
    still och staplarna växer till nya höjder.
* `d3_scatter()` och `d3_histogram()`: de nya diagramtyperna i `+`-syntaxen, t.ex.
  `d3_plot(df) + d3_scatter(x_value = "x", y_value = "y")`.

## Dokumentation

* Tabellerna på hjälpsidorna för lagren (`d3_facet()`, `d3_labels()`, `d3_tooltip()`,
  `d3_interactive()`, `d3_animation()`) visar vilka parametrar som finns för de nya
  diagramtyperna.

# funcVGRD3 0.11.0 (2026-10-20)

## Nytt

* `func_d3_karta()` och `func_d3_karta_change()`: kartans geometri (områdena, `basemap` och
  `overlay`) skrivs i ett eget block på sidan. Har flera kartor i samma dokument (R Markdown,
  Quarto) eller i samma fil från `func_d3_spara()` samma geometri, lagras den bara en gång och
  de andra kartorna använder samma block. Förut fick varje karta en egen kopia, så två
  RegSO-kartor för hela Sverige i samma presentation lagrade geometrin två gånger. Kartorna
  ser ut och fungerar som förut. Använder dokumentet knitr-cache och bara en del av kartorna
  ritas om, kan en karta peka på ett geometriblock som inte längre finns på sidan. Kartan
  visar då ett felmeddelande; rensa cachen och rendera om.

# funcVGRD3 0.10.1 (2026-10-20)

## Rättat

* `func_d3_karta()` och `func_d3_karta_change()`: den streckade ramen som visas medan man
  drar i draghandtaget (`interactive_resize = TRUE`) hamnade nedanför kartan i stället för
  på den. Nu börjar den i kartans övre vänstra hörn.
* `func_d3_karta()` och `func_d3_karta_change()`: tooltipen som visas när man söker efter
  ett område, eller trycker på ett område på en pekskärm, placerades inte vid området utan
  blev kvar där den senast visades. Nu visas den vid området.

# funcVGRD3 0.10.0 (2026-10-20)

## Nytt

* `func_d3_karta()` och `func_d3_karta_change()`: ny parameter `resize_keep_scale`. Med
  `TRUE` (standard) behåller kartan sin skala när rutan dras till ny storlek med
  draghandtaget (`interactive_resize = TRUE`), och rutan visar ett annat utsnitt med övre
  vänstra hörnet och förklaringen kvar. En större ruta visar mer runt kartan, och i en
  mindre ruta kan man panorera till den del som hamnat utanför. `FALSE` ritar om kartan så
  att den fyller den nya storleken, som i 0.9.0. Finns också i `d3_size()`.

## Ändrat

* `func_d3_karta()` och `func_d3_karta_change()` med `interactive_resize = TRUE`:
  **ändrat beteende.** Draghandtaget ändrar nu kartytans utsnitt i stället för kartans
  storlek. `resize_keep_scale = FALSE` ger beteendet från 0.9.0.

## Dokumentation

* Exemplet för `func_d3_karta()` visar båda sätten att dra (`karta7` och `karta8`).

# funcVGRD3 0.9.0 (2026-10-20)

## Nytt

* `func_d3_karta()` och `func_d3_karta_change()`: ny parameter `interactive_resize`. Med
  `TRUE` får kartan ett draghandtag i nedre högra hörnet, så att den som tittar på kartan
  kan ändra bredd och höjd med muspekaren eller fingret. Kartan ritas om i den nya storleken
  när man släpper, och text, förklaring och knappar behåller sin storlek. Läge, år, startår,
  dolda klasser och zoom följer med. Dubbelklick på handtaget eller Återställ ger tillbaka
  `width` × `height`. Export till PNG och SVG får den dragna storleken. `FALSE` (standard)
  ger inget handtag, så befintliga kartor ser likadana ut. Finns också i
  `d3_interactive(resize = ...)`.
* `func_d3_karta()` och `func_d3_karta_change()`: nya parametrar `resize_min` (standard
  `c(300, 250)`) och `resize_max` (standard `NULL`) med minsta och största bredd och höjd
  vid dragning. Utan `resize_max` blir kartan aldrig bredare än ytan runt den, och i en
  reveal.js-presentation går den inte nedanför bildytan. Finns också i `d3_size()`.

## Ändrat

* `func_d3_karta()` och `func_d3_karta_change()`: när kartan ritas om för att fönstret har
  ändrat bredd (`responsive = TRUE`) behålls zoomen. Förut började den om.

## Dokumentation

* Exemplen för `func_d3_karta()`, `func_d3_karta_change()` och `d3_size()` visar
  `interactive_resize`.

# funcVGRD3 0.8.0 (2026-10-19)

## Nytt

* `func_d3_karta()` och `func_d3_karta_change()`: kartan anpassas till smala skärmar. Med
  den nya parametern `responsive = TRUE` (standard) ritas kartan om i den bredd som finns
  när den inte får plats i `width`, t.ex. på en mobil eller i en smal spalt. Text,
  förklaring och knappar behåller då sin storlek i stället för att krympa, och kartan ritas
  om när fönstret ändrar storlek (läge, år och dolda klasser följer med). Under den nya
  parametern `responsive_breakpoint` (600 px) läggs en förklaring till vänster eller höger
  under kartan och kartan blir högre. Båda finns också i `d3_size()`.
* `func_d3_karta()` och `func_d3_karta_change()`: när man väljer ett namn i sökrutans
  lista, eller skriver hela namnet, zoomar kartan direkt till området; Enter behövs inte.
  Ett kryss i sökrutan tömmer den.
* `func_d3_karta()` och `func_d3_karta_change()`: på en pekskärm visar det första trycket
  på ett område tooltipen och nästa tryck på samma område zoomar dit. Förut zoomade kartan
  direkt och tooltipen försvann. Ett tryck utanför områdena stänger tooltipen.
* Alla diagram: på pekskärmar är knapparna och sökrutan i kontrollraden större, och på
  smala skärmar blir tooltipen inte bredare än skärmen.

## Ändrat

* `func_d3_karta()` och `func_d3_karta_change()`: **ändrat utseende** i smala behållare.
  Förut hade kartan alltid storleken `width` × `height`, även när den inte fick plats. Nu
  ritas den om smalare och lägre (`responsive = TRUE`). `responsive = FALSE` ger den fasta
  storleken som förut.
* Kartornas sökruta: webbläsarens eget kryss (finns bara i Chrome och Edge) visas inte
  längre, eftersom sökrutan har ett eget.

## Rättat

* `func_d3_spara()`: HTML-filen visas i skärmens bredd på en mobil. Förut visades den som
  en ungefär 980 px bred sida, förminskad så att text och knappar blev mycket små.
* `func_d3_karta()` och `func_d3_karta_change()`: på pekskärm tonas inte längre alla andra
  klasser ned när man trycker på legenden för att dölja eller visa en klass.
* `func_d3_karta()` och `func_d3_karta_change()`: sökrutans förslag visas i en egen lista, som går att scrolla,
  under rutan, även på mobil. Förut användes webbläsarens lista (`<datalist>`), som inte
  visas som en lista i många mobila webbläsare.

# funcVGRD3 0.7.0 (2026-10-13)

## Nytt

* `func_d3_linjediagram()`, `func_d3_stapeldiagram()` och `func_d3_dumbbell()`: ny
  parameter `tooltip_columns` med extra kolumner i tooltipen, t.ex.
  `c("Antal arbetslösa" = "antal")`, som i kartorna. Talkolumner får egna decimaler
  (heltal utan decimaler), och i hanteldiagrammet visas värdet för båda perioderna. Finns
  också i `d3_tooltip(columns = ...)`.
* `func_d3_linjediagram()`: x-axeln får plats för etiketter som inte flyttas isär
  (`data_label_repel = FALSE`) och för namnen vid linjeslutet, så att de inte klipps vid
  kanten. Samma sak på y-axeln. En etikett som ändå inte ryms flyttas in mot mitten.
* `func_d3_stapeldiagram()`: samma sak för kategoriaxeln, och liggande staplar med
  etiketter `"outside"` får plats efter etiketternas längd.
* `func_d3_dumbbell()`: stående hantlar utan repel får plats för etiketterna vid första
  och sista kategorin.
* `func_d3_linjediagram()` och `func_d3_dumbbell()`: med paneler och delad axel visas
  axeltexten bara i första kolumnen (och nederaxeln bara under nedersta panelen i
  hanteldiagrammet), och panelerna delar på den frigjorda bredden, som i ggplot2.
* Kartorna: legenden placeras intill kartan i stället för vid ytans kant, så att det inte
  blir ett glapp när kartan är smalare eller lägre än ytan.
* `func_d3_karta()` och `func_d3_karta_change()`: ny parameter `data_label_zoom_overlap`
  som anger hur mycket etiketterna får överlappa varandra när `data_label_zoom = TRUE`, som
  andel av etikettens bredd eller höjd (0–0,9). Förut var gränsen fast 40 %. Lägre värde
  ger mindre överlappning men kräver mer inzoomning innan etiketterna visas. Standard är
  `0.4`, som förut. Finns också i `d3_labels(zoom_overlap = ...)`.
* `func_d3_karta()` och `func_d3_karta_change()`: i förändringsläget har tidsreglaget två
  handtag, så att både startår och slutår kan väljas. Förut räknades förändringen alltid
  från första året. Uppspelningen går från startåret. Ny parameter `interactive_time_range`;
  `FALSE` ger ett handtag som förut. Finns också i `d3_interactive(time_range = ...)`.
  Klassgränserna för förändringen räknas fortfarande på första till sista året.

## Ändrat

* `func_d3_stapeldiagram()`, staplade staplar med etiketter `"outside"`: en etikett per
  stapel med totalsumman, som i `func_graf_barchart()`. Förut fick varje del en etikett på
  samma ställe. `data_label_first_last` och `data_label_skip` räknar staplar, och med
  `sort_by` görs urvalet i den visade ordningen.
* `func_d3_stapeldiagram()`: med `bar_type = "filled"` fyller positiva och negativa delar
  var sin 100 %, som i `func_graf_barchart()`.
* `func_d3_dumbbell()`: perioderna ordnas i tidsordning, som i `func_graf_dumbbell()`:
  faktorer efter nivåordning, tal och datum stigande, och text som börjar med en siffra
  sorteras. Annan text, t.ex. `"Före"`/`"Efter"`, behåller ordningen i data. Har data fler
  än två perioder jämförs den första och den sista. **Ändrat resultat:** förut togs
  perioderna i dataordning och de två första jämfördes, så data sorterad med den senaste
  perioden först gav omvänd förändring.
* Avrundningen i etiketter och tooltip följer R:s `round()` exakt, även i fall som 80,45
  (blir 80,4 som i R).

## Rättat

* `func_d3_stapeldiagram()`: `color_palette = "plasma"` gav färgskalan cividis.
* `func_d3_stapeldiagram()`: liggande staplar i paneler tolkade `facet_scales` för de
  ritade axlarna fel ("free_x" frigjorde kategoriaxeln i stället för värdeaxeln).
* `func_d3_stapeldiagram()`: avståndet mellan stapel och etikett (`"outside"`) blev 0 när
  alla staplar var lika höga, t.ex. med `bar_type = "filled"`.
* `func_d3_linjediagram()`: zoom och etikettutrymme släpper inte in punkter utanför
  `x_axis_min`/`x_axis_max`.
* `text_scale` gäller även texten "Inga data att visa" i linje-, stapel- och
  cirkeldiagram, mitttextens minsta storlek i `func_d3_cirkeldiagram()`, och årsstämpeln i
  `func_d3_karta()` och `func_d3_karta_change()`.
* Text i etikettrutor mäts rätt även när diagrammet ritas om medan det är dolt.
* Tusentalsavgränsaren delar inte längre ett tal när en etikett radbryts.
* `func_d3_linjediagram()`: `x_axis_n_breaks` gäller även en datumaxel (förut alltid
  cirka 8 markeringar), och med `x_number_format = "whole"` blir det inte längre dubbla
  årtal av brutna brytpunkter (2022,5 visades som 2022), som i `func_graf_linjediagram()`.

## Dokumentation

* Exemplen för `func_d3_linjediagram()`, `func_d3_stapeldiagram()` och
  `func_d3_dumbbell()` visar `tooltip_columns`.
* Exemplet för `func_d3_karta()` visar `data_label_zoom_overlap` och nämner reglaget med
  två handtag.

# funcVGRD3 0.6.0 (2026-10-12)

## Nytt

* `func_d3_karta()`: ny parameter `breaks_custom_change` med egna klassgränser för
  förändringsläget, t.ex. `c(-5, -2, 0, 2, 5, 10)`. Förut fick förändringsläget lika stora
  intervall när huvudkartan hade egna klassgränser, vilket kunde ge en nästan enfärgad karta
  när några få områden hade extrema förändringar. Finns också i
  `d3_breaks(custom_change = ...)`.

## Dokumentation

* Exemplet för `func_d3_karta()` visar `breaks_custom_change`.

# funcVGRD3 0.5.0 (2026-10-12)

## Nytt

* `func_d3_karta()` och `func_d3_karta_change()`: ny parameter `tooltip_unit_change`, enheten
  för förändringsvärdet i tooltipen, t.ex. `" procentenheter"` när `df_value` är en andel i
  procent. Förut fick förändringen samma enhet som nivåvärdena (`tooltip_unit`), så en
  förändring från 82,9 % till 84,9 % visades som "2,0 %" fast den är 2,0 procentenheter.
  `NULL` (standard) ger samma enhet som förut. Finns också i `d3_tooltip(unit_change = ...)`.

## Dokumentation

* Exemplet för `func_d3_karta()` visar `tooltip_unit_change` och `legend_title_change` för
  en andel i procent.

# funcVGRD3 0.4.1 (2026-10-12)

## Rättat

* Tooltipen hamnar intill muspekaren även när sidan skalas om, t.ex. i en reveal.js-
  presentation som förstoras till skärmen. Förut placerades den i oskalade koordinater, så
  den hamnade allt längre från pekaren och kunde hamna utanför bilden. Gäller alla
  diagramtyper.
* Tooltipens värden radbryts inte längre (t.ex. "51,8 %" eller "Plats 3343 av 3363"), och
  tooltipen får vara något bredare (340 px i stället för 280 px).

# funcVGRD3 0.4.0 (2026-10-12)

## Nytt

* `func_d3_karta()` och `func_d3_karta_change()`: ny parameter `data_label_zoom`. Med
  `TRUE` visas etiketterna först när man har zoomat in så långt att minst 75 % av
  etiketterna i kartutsnittet går fria från sina grannar (två etiketter får överlappa
  högst 40 %). Innan dess visas texten "Zooma in för att visa värden". Finns också i
  `d3_labels(zoom = TRUE)`.
* `func_d3_karta()` och `func_d3_karta_change()`: `data_label_text_color = "auto"` ger mörk
  text på ljusa områden och vit text på mörka, efter områdets färg.

## Ändrat

* `func_d3_karta()` och `func_d3_karta_change()`: etiketterna klipps vid kartytans kant och
  hamnar inte längre över legenden. Etiketter utanför kartan (`data_label_outside_map`)
  visas som förut.

## Dokumentation

* Exemplet för `func_d3_karta()` visar `data_label_zoom` och `data_label_text_color =
  "auto"`, och vignettens avsnitt Koropletkarta nämner dem.

# funcVGRD3 0.3.3 (2026-10-12)

## Rättat

* Diagram och kartor som ligger på en dold del av sidan när den laddas, t.ex. på en
  reveal.js-bild som inte visas än, ritas nu när de blir synliga. Förut mättes texterna
  till noll bredd medan de var dolda, så att t.ex. kartan i `func_d3_karta()` ritades in
  över legenden.

# funcVGRD3 0.3.2 (2026-10-12)

## Rättat

* `func_d3_karta()` och `func_d3_karta_change()`: kartan ritas inte längre ovanpå
  legenden när man har zoomat in. Kartytan klipptes med `clip-path`, som Chrome inte
  alltid ritade om efter en zoomövergång; nu är kartytan ett eget SVG-element.
* `func_d3_karta()` och `func_d3_karta_change()`: zoomen är jämnare och snabbare på kartor
  med många områden. Linjebredderna sätts en gång per zoomsteg i stället för att räknas om
  för varje område.
* `func_d3_karta()` och `func_d3_karta_change()`: det går att zooma in så långt att även det
  minsta området fyller kartan (högst 1 000 gånger, förut 20), så att man t.ex. ser
  RegSO-områden inne i städer. Varje klick på mushjulet zoomar dubbelt så mycket som förut.

# funcVGRD3 0.3.1 (2026-10-12)

## Ändrat

* `func_d3_karta()` och `func_d3_karta_change()`: kartans data skickas i kompakt form, så
  att HTML-filen blir mycket mindre och kartan byggs snabbare. En karta över Sveriges
  3 363 RegSO-områden under 20 år med fem tooltipkolumner och kommungränser gick från
  21 MB till 4,6 MB och byggs på under 2 sekunder i stället för 17. Kartorna ser ut och
  fungerar som förut. Geometrin avrundas till ett rutnät på ca 1/200 000 av kartans största
  utsträckning (1 m för hela Sverige i SWEREF99 TM, förut 0,1 m), vilket inte syns.
  Bakgrundskartor och överlagrade gränser som inte är ytor (t.ex. linjer) skickas som
  förut.

## Dokumentation

* Hjälpsidan för `func_d3_karta()` och vignettens avsnitt Kartor beskriver den kompakta
  kodningen.

# funcVGRD3 0.3.0 (2026-10-06)

## Ändrat

* funcVGRD3 behöver inte längre paketet funcVGR. Standardfärgerna (färgtema "ru") och
  kommungränserna finns nu i paketet, så funcVGR behöver inte installeras först.
  Diagrammen och kartorna ser likadana ut som förut.
* `func_d3_dumbbell()`: mindre standardstorlekar på texten, samma som i
  `funcVGR::func_graf_dumbbell()` från funcVGR 0.1.6: `title_size` och `title_text_size`
  16 (förut 18), `subtitle_text_size` 13 (16), `x_axis_text_size` och `y_axis_text_size`
  11 (14), `caption_size` och `caption_text_size` 10 (14), `legend_text_size` och
  `legend_title_size` 11 (14), `data_label_text_size` 3,5 (6) och `facet_title_size` 12
  (11). Hanteldiagram där storlekarna inte anges får därför mindre text än förut. Ange
  de gamla värdena eller `text_scale` för att få större text.

## Nytt

* Datasetet `vg_kommuner`: Västra Götalands 49 kommuner som `sf`-objekt, samma som i
  funcVGR. Exemplen och vignetten använder det i stället för `funcVGR::vg_kommuner`.
* `func_d3_dumbbell()`: ny parameter `text_scale` som skalar alla textstorlekar
  (rubriker, axlar, förklaring, panelrubriker och etiketter), t.ex. `text_scale = 1.3`
  för stora exporter. Den fungerar även med `d3_theme(text_scale = 1.3)`.
* `func_d3_dumbbell()`: nya parametrar `x_axis_line` och `y_axis_line` som slår av eller
  på linjen längs x- respektive y-axeln.
* `func_d3_dumbbell()`: `show_major_grid` och `show_minor_grid` kan vara `"x"` eller `"y"`
  för stödlinjer i bara en riktning, som i linje- och stapeldiagrammen.

## Dokumentation

* Exemplet för `func_d3_dumbbell()` visar `text_scale`, `show_major_grid = "x"` och
  `y_axis_line`.

# funcVGRD3 0.2.0 (2026-10-05)

## Nytt

* `func_d3_oppna()`: ny funktion som öppnar en sparad fil i det program datorn använder
  för filtypen, för HTML-filer webbläsaren.
* `func_d3_spara()`: `plot` kan vara en lista med diagram, som sparas i samma HTML-fil.
  Namnen i listan blir rubriker. PNG och SVG sparas fortfarande ett diagram i taget.
* `func_d3_spara()`: i RStudio är den utskrivna sökvägen en länk som öppnar filen när man
  klickar på den. Utanför RStudio skrivs sökvägen ut som förut. RStudio kör inte länken om
  sökvägen innehåller parenteser eller semikolon; då visas koden, som går att kopiera till
  konsolen.
* `func_d3_spara()`: när exemplen körs med "Run examples" på en hjälpsida visas sökvägen
  som en länk till filen på sidan. "Run examples" visar resultatet som en HTML-sida, där
  länkarna i konsolen inte fungerar.
* Diagrammen ritas direkt på sidan när exemplen körs med "Run examples" på en hjälpsida.
  Tidigare blev de tomma rutor, eftersom D3 och JavaScript-filerna inte kom med på sidan;
  nu bäddas de in, en gång per sida. I R Markdown och Quarto fungerar diagrammen som förut.
* `func_d3_hjalp()` listar även `func_d3_oppna()`.

## Dokumentation

* Varje lager i `+`-syntaxen (`d3_map()`, `d3_labs()`, `d3_legend()` m.fl.) har en egen
  hjälpsida med ett eget exempel som visar just det lagret. Tidigare delade alla lager
  hjälpsida och exempel med `d3_plot()`. Familjelagrens hjälpsidor har en tabell över vilka
  argument som finns i vilken diagramtyp.
* `func_d3_oppna()` har en egen hjälpsida med exempel.
* Exemplen sparar alla sina diagram i samma HTML-fil, inte bara det sista.
* Exemplen för `d3_plot()`, `func_d3_karta()` och `func_d3_karta_change()`: alla kommuner
  förändras nu mellan åren. Tidigare var andelen i `d3_plot()`-kartan med tidsreglage
  densamma alla år, och en del kommuner ändrades aldrig i de andra exemplen.
  Förändringskartorna har nu ett mellanliggande år (2023 respektive 2019) i
  tidsreglaget.
* `func_d3_spara()`: beskrivningen av `plot` visades som en trasig punktlista på
  hjälpsidan.

# funcVGRD3 0.1.0 (2026-10-05)

* Första versionen som paket. Funktionerna i skripten i `vgr_D3` har samlats i paketet med
  samma namn och argument, och varje funktion har en hjälpsida med exempel. För samma
  anrop ger paketet samma diagram som skripten.
* `func_d3_barchart()` finns kvar som alias för `func_d3_stapeldiagram()`.
* En vignette (`vignette("funcVGRD3")`) visar alla diagramtyper och `+`-syntaxen.
