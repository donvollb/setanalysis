#' Beispieldaten
#'
#' @description
#' Fiktive Daten einer Lehrveranstaltungsevaluation (LVE) und einer
#' Studieneingangsbefragung (SHOWUP), aufbereitet wie mit
#' [evasys_read_data()]: Jede Frage trägt die Attribute `label` (Fragetext),
#' `nr` (Fragenummer), `type` (Fragetyp) und ggf. `labels` (Antwortcodes).
#' Die Daten werden in den Beispielen und Tests des Pakets verwendet.
#'
#' Alle Antworten sind zufällig erzeugt, Fachbereiche und Fächer sind
#' erfunden und offene Antworten bestehen aus Lorem ipsum. Das Skript dazu
#' liegt im Quell-Repository unter `data-raw/BspDaten.R`.
#'
#' @format Eine Liste mit fünf Elementen:
#' \describe{
#'   \item{`dataLVE`}{Data Frame mit 4461 Antworten aus 449
#'     Lehrveranstaltungen in vier Fachbereichen, u. a. `Teilbereich`
#'     (Fachbereich), `Kennung` (Kennung der Lehrveranstaltung), `Teilnehmer`
#'     (angemeldete Teilnehmende), `FachSemN` (Fachsemester), `KF_01` bis
#'     `KF_03` (Kernfragen, 6-stufige Skala), `Note` (Gesamtnote 1–6), `V3_D`
#'     (Single Choice ja/nein) und `WL` (Workload in Stunden pro Woche).}
#'   \item{`dataSHOWUP`}{Data Frame mit 242 Antworten, u. a. `abschluss_1` bis
#'     `abschluss_8` (Multiple Choice), `fach1_2FB` und `fach2_2FB` (1. und
#'     2. Fach im 2-Fach-Bachelor), `offen` (offene Frage), `zugang_note`
#'     (Durchschnittsnote) und `info_ausr_studgang` (6-stufige Skala, Code 0 =
#'     „kann ich nicht beurteilen“).}
#'   \item{`pInfo`}{Beispiel einer Berichtstabelle mit vier Berichten (je
#'     Fachbereich). Die Spalte `FB.txt.falsch` enthält absichtlich einen
#'     Tippfehler zum Ausprobieren von [label_test()].}
#'   \item{`Plots`}{Vorbereitete Eingaben für die Grafikfunktionen, aus
#'     `dataLVE` und `dataSHOWUP` berechnet wie in den Auswertungsfunktionen:
#'     `aggr.data` (Mittelwerte von 15 Items je Lehrveranstaltung) mit
#'     `aggr.labels` und `aggr.skala`, `grade` (Gesamtnote), `rueck`
#'     (Rücklauf in Prozent) und `WL` (Workload) je Lehrveranstaltung, `num`
#'     (Durchschnittsnote in Klassen), `fsem` (Fachsemester), `sc` und `mc`
#'     (Häufigkeitstabellen).}
#'   \item{`Tabellen`}{Vorbereitete Eingaben für die Tabellenfunktionen:
#'     `multi` (Mittelwerte von 15 Items je Lehrveranstaltung) und `freq`
#'     (Faktor ja/nein).}
#' }
#'
#' @examples
#' str(BspDaten, max.level = 1)
#'
#' # Fragetext und Antwortcodes einer Frage
#' attr(BspDaten$dataLVE$KF_01, "label")
#' attr(BspDaten$dataLVE$KF_01, "labels")
"BspDaten"
