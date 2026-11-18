# Öppna frågor – funcVGRD3

| Datum | Punkt | Var | Status |
|---|---|---|---|
| 2026-11-18 | Värden med fler decimaler än etiketten: 44,95 ligger i "40,0-44,9" men visas som "45,0" med en decimal i tooltipen. Etiketterna har minst en decimal, inte så många som värdena. | `func_d3_karta_brytpunkter()` (R/func_d3_karta.R) | Öppen – kräver beslut om fler decimaler i etiketterna |
| 2026-11-18 | Procentuell förändring: om etiketterna får decimaler avgörs av förändringarna från första året; ett annat startår (interactive_time_range) kan ge decimaler som etiketterna inte visar. | `func_d3_karta_bygg()` | Öppen – låg prioritet |
| 2026-11-18 | Förändringslägets `has_na` (posten "Uppgift saknas" i förklaringen) tittar bara på `interactive_time_range`, inte på `interactive_time_slider`; utan tidsreglage kan posten visas fast inget startår går att välja. Fanns före 0.14.0. | `func_d3_karta_bygg()` (has_na i förändringsläget) | Öppen – fanns före |
| 2026-11-18 | Kartans dataetiketter följer `data_label_decimals` (standard 0), så en etikett "45" kan stå i klassen "40,0-44,9". | inst/js/karta.js (labelText) | Öppen – kräver beslut |
| 2026-11-18 | `breaks_round_labels = TRUE` flyttar upp den nedre gränsen ("6,0-6,9"), vilket förutsätter att klassen omfattar sin övre gräns; klasserna omfattar den nedre (cut med right = FALSE), så 5,9 hamnar i "6,0-6,9". Fanns före 0.14.0. | `func_d3_karta_brytpunkter()` | Öppen – fanns före |
| 2026-11-18 | En enda klass (n_groups = 1 eller alla värden lika) följer inte decimalregeln och saknar tankstreck vid negativa värden ("-5--1"). Fanns före 0.14.0. | `func_d3_karta_brytpunkter()` | Öppen – fanns före |
| 2026-11-18 | Klassgränser med decimaler skrivs med den övre gränsen inräknad ("4,9-5,9", "5,9-6,9"), så samma värde står i två etiketter. Fanns före 0.14.0. | `func_d3_karta_brytpunkter()` | Öppen – fanns före |
| 2026-11-18 | Histogrammet och linjediagrammet skriver intervall med tankstreck utan mellanslag och övre gräns = nästa gräns ("40–45"), kartan med bindestreck och övre gräns ett steg under ("40-44"). | inst/js/histogram.js:385, linjediagram.js:800 | Öppen – medveten skillnad? |
| 2026-11-18 | Regressionstesterna för karta.js (6a, 6b) söker bara i källkoden; JS-beteendet körs inte (kan köras med V8 eller i webbläsare). | tests/regression/test_0.14.0.R | Öppen – låg prioritet |
