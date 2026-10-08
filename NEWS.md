# setanalysis (Entwicklungsversion)

Überarbeitung von Dokumentation, Paketstruktur und Code. Die Ausgabe der
Funktionen bleibt unverändert, sofern unten nicht ausdrücklich als
Fehlerbehebung aufgeführt.

## Einheitliche Funktionsnamen

* Alle Funktionen heißen jetzt einheitlich in snake_case:
  `table_freq()`, `table_stat_single()`, `table_stat_multi()`,
  `evasys_read_data()`, `input_tabelle()`, `label_test()`,
  `change_analysis_defaults()`, `bsp_boxplot()`, `bsp_evasys_sk6()`,
  `bsp_table_stat()` sowie die Umgebung `list_open_answers`.
* Die bisherigen Namen funktionieren weiterhin unverändert und sind in
  `?setanalysis-deprecated` aufgeführt. Argumentnamen bleiben gleich.

## Beispieldaten

* `BspDaten` enthält jetzt vollständig fiktive, zufällig erzeugte Daten
  (erfundene Fachbereiche und Fächer, offene Antworten aus Lorem ipsum) statt
  anonymisierter Befragungsdaten. Aufbau, Spaltennamen und Attribute bleiben
  gleich; die leere Spalte `dataLVE$ECTS` entfällt. Die Daten werden mit
  `data-raw/BspDaten.R` reproduzierbar erzeugt.
* In der Beispiel-Berichtstabelle (`inst/extdata/beispiel_berichte.xlsx`)
  heißt der spezielle Bericht jetzt „Sonderauswertung“.

## Fehlerbehebungen

* Sonderzeichen in Tabellen werden für Typst maskiert. Bisher brach das
  Erstellen des PDFs ab, wenn eine offene Antwort, eine Antwortoption oder ein
  Item-Text z. B. ` // `, `$`, `#`, `_`, `*`, `<…>` oder `@…` enthielt
  („unclosed delimiter“), und ein einzelner Backslash verschwand. Betroffen
  waren alle Tabellen aus `lv_table()` sowie die Skalenbeschriftung im
  Tabellenkopf von `merge_aggr_sk()`. Die Kopfzeilen der Tabellen werden
  weiterhin nicht maskiert, damit sie Typst-Code enthalten können.
* `merge_mc()` zeichnet die Zeilen „NAs“ und „Total“ nicht mehr als Balken ins
  Diagramm; sie stehen weiterhin in der Tabelle. Dadurch wird die Abbildung bei
  `fig.height = "default"` entsprechend niedriger. Die Beispieldaten
  `BspDaten$Plots$mc` wurden passend dazu bereinigt.
* `merge_many()` wurde neu strukturiert und bricht in folgenden Fällen nicht
  mehr ab:
  - die letzte Spalte ist eine Skalen- oder MC-Frage,
  - eine einzelne Skalenfrage steht zwischen anderen Fragen (sie wird jetzt
    einzeln ausgewertet),
  - in der globalen Umgebung existiert eine Variable `counter`.

  Spalten ohne Fragetyp führen zu einer verständlichen Fehlermeldung.
* `appendix_open()` leert nach der Ausgabe den Speicher der offenen Antworten.
  Werden mehrere Berichte in derselben R-Sitzung erstellt (z. B. mit
  `rmarkdown::render()` in einer Schleife), enthält der Anhang eines Berichts
  damit nicht mehr die offenen Antworten der vorherigen Berichte.
* `appendix_open()` bricht nicht mehr ab, wenn eine offene Frage mit `nr` und
  `inkl = TRUE` gesammelt wurde (bisher: „object 'inkl.x.y' not found“, wenn es
  keine passende inkl.-Variable gab). Der Anhang übernimmt jetzt die beim
  Sammeln getroffene Entscheidung, statt die inkl.-Variable erneut abzufragen.
* `merge_fachsem()` berücksichtigt das Argument `fig.height` (bisher war die
  Höhe der Abbildung immer 5; der Standardwert bleibt 5).
* `merge_fachsem()` bricht bei einer ungültigen `group` jetzt mit einer
  verständlichen Fehlermeldung ab (bisher: „Objekt 'caps' nicht gefunden“).
* `markdown_in_viewer()` stellt die vorherigen knitr-Einstellungen nach der
  Vorschau wieder her (bisher wurde `knitr.duplicate.label` fest auf
  `"forbid"` gesetzt).
* `label_test()`: Ausnahmen werden einfacher und robuster herausgefiltert.
* `input_tabelle()` wertet die Bedingungen der Regeltabelle jetzt direkt mit
  den Spalten der Berichtstabelle aus, statt die Spaltennamen per Textersatz
  umzuschreiben. Bestehende Regeltabellen liefern dasselbe Ergebnis; zusätzlich
  funktionieren nun Bedingungen ohne Leerzeichen nach dem Spaltennamen
  (z. B. `Art=="alles"`, bisher Abbruch), und Spaltennamen innerhalb von
  Texten (z. B. `Fach == "Art und Weise"`) werden nicht mehr verfälscht.
* `merge.open()` ist wieder von außen aufrufbar. roxygen hatte die Funktion
  fälschlich als S3-Methode von `merge()` registriert statt sie zu exportieren.

## Dokumentation

* Alle Hilfeseiten wurden neu geschrieben: klare Titel und Beschreibungen,
  vollständig beschriebene Argumente und Rückgabewerte, Querverweise und
  Funktionsgruppen (Auswertung, Tabellen, Grafiken, Legenden,
  Datenaufbereitung, Werkzeuge).
* Neue Paket-Hilfeseite `?setanalysis` mit dem typischen Ablauf und einer
  Erklärung der inkl.-Logik.
* Alle Beispiele laufen und legen keine Dateien mehr im Arbeitsverzeichnis an.
  Für `evasys_read_data()` und `input_tabelle()` liegen fiktive
  Beispieldateien in `inst/extdata/`.
* Neue Vignette „Einen Evaluationsbericht erstellen“
  (`vignette("bericht-erstellen", package = "setanalysis")`): Daten einlesen,
  Berichte über Berichts- und Regeltabelle festlegen, Bericht in Quarto
  schreiben, alle Berichte erstellen.
* README neu geschrieben, zusätzlich auf Englisch (`README.en.md`), mit
  Vorschaubildern aus den Beispieldaten (erzeugt mit
  `data-raw/readme_vorschau.R`).

* Alle veralteten Funktionsnamen (z. B. `merge.sc()`, `grade()`,
  `open.answers()`) sind in einer gemeinsamen Hilfeseite
  `?setanalysis-deprecated` mit ihrer jeweils aktuellen Entsprechung
  dokumentiert. Die alten `merge.*()`-Namen und `boxplot.ruecklauf()` leiten
  Aufrufe unverändert an die aktuellen Funktionen weiter; dadurch entfallen die
  S3-Warnungen von `R CMD check`.

## Paketinfrastruktur

* Weniger Abhängigkeiten: dplyr wird nicht mehr benötigt. htmltools, markdown
  und svglite sind nur noch optional (für die Vorschau mit
  `markdown_in_viewer()`); fehlen sie, weist die Funktion darauf hin. Dadurch
  werden bei der Installation rund 30 Pakete weniger mitinstalliert.

* Automatisierte Tests mit testthat: Snapshot-Tests halten die Ausgabe aller
  Auswertungs-, Tabellen- und Grafikfunktionen fest (Typst-Text und
  SVG-Abbildungen), sodass unbeabsichtigte Änderungen sofort auffallen.
* `DESCRIPTION` vervollständigt: Titel, Beschreibung, Autoren, Links zum
  Repository und alle verwendeten Basis-Pakete unter `Imports`.
* `subchunkify()` legt keine Variable `sub.nr` mehr in der globalen Umgebung
  an; der Zähler für die Namen der Sub-Chunks liegt jetzt im Paket.
* `.Rbuildignore` ergänzt; die interne To-do-Liste `notes.txt` wurde entfernt.
* Quellcode thematisch neu geordnet (z. B. `plots_bar.R`, `tables.R`,
  `settings.R`); die großen `merge_*()`-Funktionen behalten eigene Dateien.

# setanalysis 1.0.0

* Erste als R-Paket versionierte Fassung (Git-Tag `v1.0.0`).
