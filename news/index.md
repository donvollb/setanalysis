# Changelog

## setanalysis 1.1.0

Überarbeitung von Dokumentation, Paketstruktur und Code. Die Ausgabe der
Funktionen bleibt unverändert, sofern unten nicht ausdrücklich als
Fehlerbehebung aufgeführt.

### Einheitliche Funktionsnamen

- Alle Funktionen heißen jetzt einheitlich in snake_case:
  [`table_freq()`](https://donvollb.github.io/setanalysis/reference/table_freq.md),
  [`table_stat_single()`](https://donvollb.github.io/setanalysis/reference/table_stat_single.md),
  [`table_stat_multi()`](https://donvollb.github.io/setanalysis/reference/table_stat_multi.md),
  [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md),
  [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md),
  [`label_test()`](https://donvollb.github.io/setanalysis/reference/label_test.md),
  [`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md),
  [`bsp_boxplot()`](https://donvollb.github.io/setanalysis/reference/bsp_boxplot.md),
  [`bsp_evasys_sk6()`](https://donvollb.github.io/setanalysis/reference/bsp_evasys_sk6.md),
  [`bsp_table_stat()`](https://donvollb.github.io/setanalysis/reference/bsp_table_stat.md)
  sowie die Umgebung `list_open_answers`.
- Die bisherigen Namen funktionieren weiterhin unverändert und sind in
  `?setanalysis-deprecated` aufgeführt. Argumentnamen bleiben gleich.

### Beispieldaten

- `BspDaten` enthält jetzt vollständig fiktive, zufällig erzeugte Daten
  (erfundene Fachbereiche und Fächer, offene Antworten aus Lorem ipsum)
  statt anonymisierter Befragungsdaten. Aufbau, Spaltennamen und
  Attribute bleiben gleich; die leere Spalte `dataLVE$ECTS` entfällt.
  Die Daten werden mit `data-raw/BspDaten.R` reproduzierbar erzeugt.
- In der Beispiel-Berichtstabelle
  (`inst/extdata/beispiel_berichte.xlsx`) heißt der spezielle Bericht
  jetzt „Sonderauswertung“.

### Fehlerbehebungen

- Sonderzeichen in Tabellen werden für Typst maskiert. Bisher brach das
  Erstellen des PDFs ab, wenn eine offene Antwort, eine Antwortoption
  oder ein Item-Text z. B. `//`, `$`, `#`, `_`, `*`, `<…>` oder `@…`
  enthielt („unclosed delimiter“), und ein einzelner Backslash
  verschwand. Betroffen waren alle Tabellen aus
  [`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md)
  sowie die Skalenbeschriftung im Tabellenkopf von
  [`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md).
  Die Kopfzeilen der Tabellen werden weiterhin nicht maskiert, damit sie
  Typst-Code enthalten können.
- [`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md)
  zeichnet die Zeilen „NAs“ und „Total“ nicht mehr als Balken ins
  Diagramm; sie stehen weiterhin in der Tabelle. Dadurch wird die
  Abbildung bei `fig.height = "default"` entsprechend niedriger. Die
  Beispieldaten `BspDaten$Plots$mc` wurden passend dazu bereinigt.
- [`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md)
  wurde neu strukturiert und bricht in folgenden Fällen nicht mehr ab:
  - die letzte Spalte ist eine Skalen- oder MC-Frage,
  - eine einzelne Skalenfrage steht zwischen anderen Fragen (sie wird
    jetzt einzeln ausgewertet),
  - in der globalen Umgebung existiert eine Variable `counter`.

  Spalten ohne Fragetyp führen zu einer verständlichen Fehlermeldung.
- [`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md)
  leert nach der Ausgabe den Speicher der offenen Antworten. Werden
  mehrere Berichte in derselben R-Sitzung erstellt (z. B. mit
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
  in einer Schleife), enthält der Anhang eines Berichts damit nicht mehr
  die offenen Antworten der vorherigen Berichte.
- [`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md)
  bricht nicht mehr ab, wenn eine offene Frage mit `nr` und
  `inkl = TRUE` gesammelt wurde (bisher: „object ‘inkl.x.y’ not found“,
  wenn es keine passende inkl.-Variable gab). Der Anhang übernimmt jetzt
  die beim Sammeln getroffene Entscheidung, statt die inkl.-Variable
  erneut abzufragen.
- [`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md)
  berücksichtigt das Argument `fig.height` (bisher war die Höhe der
  Abbildung immer 5; der Standardwert bleibt 5).
- [`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md)
  bricht bei einer ungültigen `group` jetzt mit einer verständlichen
  Fehlermeldung ab (bisher: „Objekt ‘caps’ nicht gefunden“).
- [`markdown_in_viewer()`](https://donvollb.github.io/setanalysis/reference/markdown_in_viewer.md)
  stellt die vorherigen knitr-Einstellungen nach der Vorschau wieder her
  (bisher wurde `knitr.duplicate.label` fest auf `"forbid"` gesetzt).
- [`label_test()`](https://donvollb.github.io/setanalysis/reference/label_test.md):
  Ausnahmen werden einfacher und robuster herausgefiltert. Die Meldungen
  sprechen jetzt von der „Berichtstabelle“ statt vom internen
  Objektnamen `personalized.info`.
- [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md)
  wertet die Bedingungen der Regeltabelle jetzt direkt mit den Spalten
  der Berichtstabelle aus, statt die Spaltennamen per Textersatz
  umzuschreiben. Bestehende Regeltabellen liefern dasselbe Ergebnis;
  zusätzlich funktionieren nun Bedingungen ohne Leerzeichen nach dem
  Spaltennamen (z. B. `Art=="alles"`, bisher Abbruch), und Spaltennamen
  innerhalb von Texten (z. B. `Fach == "Art und Weise"`) werden nicht
  mehr verfälscht.
- [`merge.open()`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  ist wieder von außen aufrufbar. roxygen hatte die Funktion fälschlich
  als S3-Methode von [`merge()`](https://rdrr.io/r/base/merge.html)
  registriert statt sie zu exportieren.

### Dokumentation

- Alle Hilfeseiten wurden neu geschrieben: klare Titel und
  Beschreibungen, vollständig beschriebene Argumente und Rückgabewerte,
  Querverweise und Funktionsgruppen (Auswertung, Tabellen, Grafiken,
  Legenden, Datenaufbereitung, Werkzeuge).

- Neue Paket-Hilfeseite
  [`?setanalysis`](https://donvollb.github.io/setanalysis/reference/setanalysis-package.md)
  mit dem typischen Ablauf und einer Erklärung der inkl.-Logik.

- Alle Beispiele laufen und legen keine Dateien mehr im
  Arbeitsverzeichnis an. Für
  [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
  und
  [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md)
  liegen fiktive Beispieldateien in `inst/extdata/`.

- Neue Vignette „Einen Evaluationsbericht erstellen“
  ([`vignette("bericht-erstellen", package = "setanalysis")`](https://donvollb.github.io/setanalysis/articles/bericht-erstellen.md)):
  Daten einlesen, Berichte über Berichts- und Regeltabelle festlegen,
  Bericht in Quarto schreiben, alle Berichte erstellen.

- README neu geschrieben, zusätzlich auf Englisch (`README.en.md`), mit
  Vorschaubildern aus den Beispieldaten (erzeugt mit
  `data-raw/readme_vorschau.R`).

- Alle veralteten Funktionsnamen (z. B.
  [`merge.sc()`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md),
  [`grade()`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md),
  [`open.answers()`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md))
  sind in einer gemeinsamen Hilfeseite `?setanalysis-deprecated` mit
  ihrer jeweils aktuellen Entsprechung dokumentiert. Die alten
  `merge.*()`-Namen und
  [`boxplot.ruecklauf()`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  leiten Aufrufe unverändert an die aktuellen Funktionen weiter; dadurch
  entfallen die S3-Warnungen von `R CMD check`.

### Paketinfrastruktur

- Weniger Abhängigkeiten: dplyr wird nicht mehr benötigt. htmltools,
  markdown und svglite sind nur noch optional (für die Vorschau mit
  [`markdown_in_viewer()`](https://donvollb.github.io/setanalysis/reference/markdown_in_viewer.md));
  fehlen sie, weist die Funktion darauf hin. Dadurch werden bei der
  Installation rund 30 Pakete weniger mitinstalliert.

- Automatisierte Tests mit testthat: Snapshot-Tests halten die Ausgabe
  aller Auswertungs-, Tabellen- und Grafikfunktionen fest (Typst-Text
  und SVG-Abbildungen), sodass unbeabsichtigte Änderungen sofort
  auffallen.

- GitHub Actions: `R CMD check` läuft bei jedem Push auf Windows, macOS
  und Linux (die SVG-Abbildungen werden dort nicht verglichen, weil ihre
  Textmaße von den installierten Schriften abhängen). Die Website
  <https://donvollb.github.io/setanalysis/> wird mit pkgdown erzeugt.

- `DESCRIPTION` vervollständigt: Titel, Beschreibung, Autoren, Links zum
  Repository und alle verwendeten Basis-Pakete unter `Imports`.

- [`subchunkify()`](https://donvollb.github.io/setanalysis/reference/subchunkify.md)
  legt keine Variable `sub.nr` mehr in der globalen Umgebung an; der
  Zähler für die Namen der Sub-Chunks liegt jetzt im Paket.

- `.Rbuildignore` ergänzt; die interne To-do-Liste `notes.txt` wurde
  entfernt.

- Quellcode thematisch neu geordnet (z. B. `plots_bar.R`, `tables.R`,
  `settings.R`); die großen `merge_*()`-Funktionen behalten eigene
  Dateien.

## setanalysis 1.0.0

- Erste als R-Paket versionierte Fassung (Git-Tag `v1.0.0`).
