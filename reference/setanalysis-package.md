# setanalysis: Personalisierte Berichte für Lehrevaluationen

setanalysis enthält die Bausteine, mit denen aus den Daten einer
Befragung (v. a. der Lehrveranstaltungsevaluation) automatisch
personalisierte Ergebnisberichte mit [Quarto](https://quarto.org)
erstellt werden. Für jede Frage erzeugt eine `merge_*()`-Funktion den
passenden Berichtsabschnitt: Überschrift, Tabelle und Abbildung. Die
Ausgabe ist für Berichte im Typst-Format gedacht.

## Typischer Ablauf

1.  **Daten einlesen:**
    [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
    liest Rohdaten und Codebuch aus evasys ein und versieht jede
    Variable mit Fragetext (`label`), Fragenummer (`nr`), Fragetyp
    (`type`) und Antwortcodes (`labels`). Achtung: Beim Auswählen von
    Zeilen (`daten[auswahl, ]`) entfernt R diese Attribute; wie man sie
    erhält, zeigt die Vignette
    [`vignette("bericht-erstellen", package = "setanalysis")`](https://donvollb.github.io/setanalysis/articles/bericht-erstellen.md).

2.  **Berichte festlegen** (optional):
    [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md)
    liest eine Berichtstabelle (eine Zeile pro Bericht) und eine
    Regeltabelle und legt damit fest, welche Fragen in welchen Bericht
    kommen.

3.  **Bericht schreiben:** Im Quarto-Dokument ruft man in Chunks mit
    `output: asis` die Auswertungsfunktionen auf, z. B.
    [`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md),
    [`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md),
    [`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md),
    [`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md)
    oder
    [`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md).
    [`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md)
    wertet mehrere Fragen auf einmal aus und erkennt den Fragetyp
    selbst.

4.  **Vorschau:**
    [`markdown_in_viewer()`](https://donvollb.github.io/setanalysis/reference/markdown_in_viewer.md)
    zeigt das Ergebnis einer Auswertungsfunktion direkt im
    RStudio-Viewer an.

## Welche Fragen kommen in einen Bericht? (inkl.-Logik)

Alle Auswertungsfunktionen haben die Argumente `inkl` und `nr`. Mit
`inkl = TRUE` bzw. `FALSE` wird eine Frage direkt ein- oder
ausgeschlossen. Beim Standard `inkl = "nr"` entscheidet die Variable
`inkl.<nr>` (z. B. `inkl.2.1` für `nr = "2.1"`), die im
Berichts-Template meist aus der Zeile der Berichtstabelle aus
[`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md)
gesetzt wird. Ohne Fragenummer (`nr = ""`) wird die Frage immer
ausgegeben.

## Einstellungen

Farben, Spaltenbreiten und Voreinstellungen (z. B. ob Abbildungen
gezeigt werden) stehen in
[setanalysis_defaults](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md)
und lassen sich mit
[`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md)
für einen Bericht anpassen.

## Beispieldaten

[BspDaten](https://donvollb.github.io/setanalysis/reference/BspDaten.md)
enthält fiktive, zufällig erzeugte Daten einer
Lehrveranstaltungsevaluation und einer Studieneingangsbefragung, mit
denen sich alle Funktionen ausprobieren lassen.

## See also

Useful links:

- <https://github.com/donvollb/setanalysis>

- Report bugs at <https://github.com/donvollb/setanalysis/issues>

## Author

**Maintainer**: Dominik Vollbracht <dominik@vollbracht.email>

Authors:

- Dominik Vollbracht <dominik@vollbracht.email>

- Simon Männle
