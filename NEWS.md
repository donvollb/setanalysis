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

## Fehlerbehebungen

* `merge_mc()` zeichnet die Zeilen „NAs“ und „Total“ nicht mehr als Balken ins
  Diagramm; sie stehen weiterhin in der Tabelle. Dadurch wird die Abbildung bei
  `fig.height = "default"` entsprechend niedriger. Die Beispieldaten
  `BspDaten$Plots$mc` wurden passend dazu bereinigt.
* `merge.open()` ist wieder von außen aufrufbar. roxygen hatte die Funktion
  fälschlich als S3-Methode von `merge()` registriert statt sie zu exportieren.

## Dokumentation

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
* `.Rbuildignore` ergänzt; die interne To-do-Liste `notes.txt` wurde entfernt.
* Quellcode thematisch neu geordnet (z. B. `plots_bar.R`, `tables.R`,
  `settings.R`); die großen `merge_*()`-Funktionen behalten eigene Dateien.

# setanalysis 1.0.0

* Erste als R-Paket versionierte Fassung (Git-Tag `v1.0.0`).
