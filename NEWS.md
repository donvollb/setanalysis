# setanalysis (Entwicklungsversion)

Überarbeitung von Dokumentation, Paketstruktur und Code. Die Ausgabe der
Funktionen bleibt unverändert, sofern unten nicht ausdrücklich als
Fehlerbehebung aufgeführt.

## Fehlerbehebungen

* `merge_mc()` zeichnet die Zeilen „NAs“ und „Total“ nicht mehr als Balken ins
  Diagramm; sie stehen weiterhin in der Tabelle. Dadurch wird die Abbildung bei
  `fig.height = "default"` entsprechend niedriger. Die Beispieldaten
  `BspDaten$Plots$mc` wurden passend dazu bereinigt.

## Paketinfrastruktur

* Automatisierte Tests mit testthat: Snapshot-Tests halten die Ausgabe aller
  Auswertungs-, Tabellen- und Grafikfunktionen fest (Typst-Text und
  SVG-Abbildungen), sodass unbeabsichtigte Änderungen sofort auffallen.
* `DESCRIPTION` vervollständigt: Titel, Beschreibung, Autoren, Links zum
  Repository und alle verwendeten Basis-Pakete unter `Imports`.
* `.Rbuildignore` ergänzt; die interne To-do-Liste `notes.txt` wurde entfernt.

# setanalysis 1.0.0

* Erste als R-Paket versionierte Fassung (Git-Tag `v1.0.0`).
