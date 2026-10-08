#' Beispieldaten
#'
#' @description
#' Anonymisierte Daten aus einer Lehrveranstaltungsevaluation (LVE,
#' Sommersemester 2024) und einer Studieneingangsbefragung (SHOWUP 2024/25),
#' aufbereitet wie mit [evasys_read_data()]: Jede Frage trägt die Attribute
#' `label` (Fragetext), `nr` (Fragenummer), `type` (Fragetyp) und ggf.
#' `labels` (Antwortcodes). Die Daten werden in den Beispielen und Tests des
#' Pakets verwendet.
#'
#' @format Eine Liste mit fünf Elementen:
#' \describe{
#'   \item{`dataLVE`}{Data Frame mit 4559 Antworten aus 449
#'     Lehrveranstaltungen, u. a. `Teilbereich` (Fachbereich), `Kennung`
#'     (pseudonymisierte Kennung der Lehrveranstaltung), `Teilnehmer`
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
#'   \item{`Plots`}{Vorbereitete Eingaben für die Grafikfunktionen, z. B.
#'     `aggr.data` (Mittelwerte je Lehrveranstaltung), `grade`, `rueck`, `WL`,
#'     `num`, `fsem`, `sc` und `mc`.}
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
