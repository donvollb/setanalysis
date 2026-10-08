#' setanalysis: Personalisierte Berichte für Lehrevaluationen
#'
#' @description
#' setanalysis enthält die Bausteine, mit denen aus den Daten einer Befragung
#' (v. a. der Lehrveranstaltungsevaluation) automatisch personalisierte
#' Ergebnisberichte mit [Quarto](https://quarto.org) erstellt werden. Für jede
#' Frage erzeugt eine `merge_*()`-Funktion den passenden Berichtsabschnitt:
#' Überschrift, Tabelle und Abbildung. Die Ausgabe ist für Berichte im
#' Typst-Format gedacht.
#'
#' @section Typischer Ablauf:
#' 1. **Daten einlesen:** [evasys_read_data()] liest Rohdaten und Codebuch aus
#'    evasys ein und versieht jede Variable mit Fragetext (`label`),
#'    Fragenummer (`nr`), Fragetyp (`type`) und Antwortcodes (`labels`).
#'    Achtung: Beim Auswählen von Zeilen (`daten[auswahl, ]`) entfernt R diese
#'    Attribute; wie man sie erhält, zeigt die Vignette
#'    `vignette("bericht-erstellen", package = "setanalysis")`.
#' 2. **Berichte festlegen** (optional): [input_tabelle()] liest eine
#'    Berichtstabelle (eine Zeile pro Bericht) und eine Regeltabelle und legt
#'    damit fest, welche Fragen in welchen Bericht kommen.
#' 3. **Bericht schreiben:** Im Quarto-Dokument ruft man in Chunks mit
#'    `output: asis` die Auswertungsfunktionen auf, z. B. [merge_sc()],
#'    [merge_mc()], [merge_sk()], [merge_aggr_sk()] oder [merge_open()].
#'    [merge_many()] wertet mehrere Fragen auf einmal aus und erkennt den
#'    Fragetyp selbst.
#' 4. **Vorschau:** [markdown_in_viewer()] zeigt das Ergebnis einer
#'    Auswertungsfunktion direkt im RStudio-Viewer an.
#'
#' @section Welche Fragen kommen in einen Bericht? (inkl.-Logik):
#' Alle Auswertungsfunktionen haben die Argumente `inkl` und `nr`. Mit
#' `inkl = TRUE` bzw. `FALSE` wird eine Frage direkt ein- oder ausgeschlossen.
#' Beim Standard `inkl = "nr"` entscheidet die Variable `inkl.<nr>`
#' (z. B. `inkl.2.1` für `nr = "2.1"`), die im Berichts-Template meist aus
#' der Zeile der Berichtstabelle aus [input_tabelle()] gesetzt wird. Ohne
#' Fragenummer (`nr = ""`) wird die Frage immer ausgegeben.
#'
#' @section Einstellungen:
#' Farben, Spaltenbreiten und Voreinstellungen (z. B. ob Abbildungen gezeigt
#' werden) stehen in [setanalysis_defaults] und lassen sich mit
#' [change_analysis_defaults()] für einen Bericht anpassen.
#'
#' @section Beispieldaten:
#' [BspDaten] enthält fiktive, zufällig erzeugte Daten einer
#' Lehrveranstaltungsevaluation und einer Studieneingangsbefragung, mit denen
#' sich alle Funktionen ausprobieren lassen.
#'
#' @keywords internal
"_PACKAGE"

# Paketweite Importe -------------------------------------------------------

# Häufig verwendete Funktionen laden, dass man sie auch ohne „::“ nutzen kann

#' @importFrom grDevices rgb adjustcolor
#' @importFrom graphics abline axis barplot box boxplot mtext par segments text title
#' @importFrom stats median na.omit sd setNames
#' @importFrom utils capture.output read.csv2
#' @importFrom tinytable tt style_tt tt_format
NULL
