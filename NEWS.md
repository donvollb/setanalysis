# setanalysis (Entwicklungsversion)

Überarbeitung von Dokumentation, Paketstruktur und Code. Die Ausgabe der
Funktionen bleibt unverändert, sofern unten nicht ausdrücklich als
Fehlerbehebung aufgeführt.

## Paketinfrastruktur

* Automatisierte Tests mit testthat: Snapshot-Tests halten die Ausgabe aller
  Auswertungs-, Tabellen- und Grafikfunktionen fest (Typst-Text und
  SVG-Abbildungen), sodass unbeabsichtigte Änderungen sofort auffallen.
* `DESCRIPTION` vervollständigt: Titel, Beschreibung, Autoren, Links zum
  Repository und alle verwendeten Basis-Pakete unter `Imports`.
* `.Rbuildignore` ergänzt; die interne To-do-Liste `notes.txt` wurde entfernt.

# setanalysis 1.0.0

* Erste als R-Paket versionierte Fassung (Git-Tag `v1.0.0`).
