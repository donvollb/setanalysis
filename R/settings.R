# Paketeinstellungen und Speicher für offene Antworten ------------------

#' Einstellungen des Pakets
#'
#' @description
#' Umgebung mit den Einstellungen, die die Auswertungsfunktionen verwenden
#' (Farben, Spaltenbreiten und Voreinstellungen). Die Werte lassen sich mit
#' [change_analysis_defaults()] für einen Bericht ändern, z. B. eine eigene
#' Akzentfarbe je Befragung.
#'
#' @format Eine Umgebung mit folgenden Einträgen:
#' \describe{
#'   \item{`font.family`}{Schriftart der Abbildungen (`"Red Hat Text"`; wird
#'     beim Laden des Pakets registriert).}
#'   \item{`color.bars`}{Akzentfarbe für Balken, Boxen und die eingefärbten
#'     Tabellenzeilen.}
#'   \item{`col.width3`, `col.width4`}{Relative Spaltenbreiten der
#'     Häufigkeitstabellen mit drei bzw. vier Spalten ([table_freq()],
#'     [merge_mc()]).}
#'   \item{`col.width.sm`, `col.width.sm.alt1`, `col.width.sm.alt2`}{Relative
#'     Spaltenbreiten der Statistiktabellen für mehrere Items ohne bzw. mit
#'     einer oder zwei Ausweichoptionen ([table_stat_multi()]).}
#'   \item{`col1.width.tss`}{Wird derzeit nicht verwendet (aus früheren
#'     Versionen).}
#'   \item{`show.plot.sc`, `show.plot.mc`, `show.plot.sk`}{Voreinstellung für
#'     `show.plot` in [merge_sc()], [merge_mc()] und [merge_sk()].}
#'   \item{`open.appendix`}{Voreinstellung für `appendix` in [merge_open()]:
#'     offene Antworten im Anhang sammeln?}
#'   \item{`inkl.open`}{Voreinstellung für `inkl_global` in [merge_open()]:
#'     offene Fragen überhaupt ausgeben?}
#' }
#'
#' @family werkzeuge
#'
#' @examples
#' setanalysis_defaults$color.bars
#' ls(setanalysis_defaults)
#'
#' @export
setanalysis_defaults <- new.env(parent = emptyenv())

setanalysis_defaults$font.family <- "Red Hat Text"
setanalysis_defaults$col.width3 <- c(108, 18, 11)
setanalysis_defaults$col.width4 <- c(86, 18, 11, 18)
setanalysis_defaults$col.width.sm <- c(64, 11, 9, 9, 9, 9, 9)
setanalysis_defaults$col.width.sm.alt1 <- c(59, 8, 8, 8, 8, 8, 8, 15)
setanalysis_defaults$col.width.sm.alt2 <- c(52, 7, 7, 7, 5, 6, 6, 12, 12)
setanalysis_defaults$col1.width.tss <- 12
setanalysis_defaults$color.bars <- rgb(109, 172, 220, maxColorValue = 255)
setanalysis_defaults$show.plot.sc <- TRUE
setanalysis_defaults$show.plot.mc <- TRUE
setanalysis_defaults$show.plot.sk <- TRUE
setanalysis_defaults$open.appendix <- TRUE
setanalysis_defaults$inkl.open <- TRUE

#' Speicher für die offenen Antworten des Anhangs
#'
#' @description
#' Umgebung, in der [merge_open()] bei `appendix = TRUE` die offenen Fragen
#' eines Berichts sammelt, bis [appendix_open()] sie im Anhang ausgibt und den
#' Speicher wieder leert. Für die normale Nutzung muss man sie nicht direkt
#' ansprechen.
#'
#' @format Eine Umgebung mit dem Zähler `anchor.nr` (Anzahl der gesammelten
#'   Fragen) sowie den Einträgen `var.1`, `nr.1`, `var.2`, `nr.2`, … mit den
#'   Antworten und Fragenummern.
#'
#' @family werkzeuge
#'
#' @export
list_open_answers <- new.env(parent = emptyenv())

list_open_answers$anchor.nr <- 0

# Veralteter Name (zeigt auf dieselbe Umgebung, siehe ?setanalysis-deprecated)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @format NULL
#' @export list.open.answers
list.open.answers <- list_open_answers

# Funktion, um diese Einstellungen zu ändern ------------------------------

#' Einstellungen des Pakets ändern
#'
#' @description
#' Ändert einen oder mehrere Einträge in [setanalysis_defaults], z. B. die
#' Akzentfarbe oder ob Abbildungen gezeigt werden. Die Änderung gilt für alle
#' folgenden Aufrufe in der R-Sitzung, typischerweise also für den ganzen
#' Bericht. Nur bestehende Einstellungen können geändert werden.
#'
#' @param ... Einstellungen in der Form `name = wert`, z. B.
#'   `color.bars = "#507289"` oder `show.plot.sc = FALSE`. Die möglichen Namen
#'   stehen in [setanalysis_defaults].
#'
#' @returns Nichts (unsichtbar `NULL`).
#'
#' @family werkzeuge
#'
#' @examples
#' alte_werte <- mget(c("color.bars", "show.plot.sc"), envir = setanalysis_defaults)
#'
#' # Balken blaugrau, keine Abbildungen bei Single-Choice-Fragen
#' change_analysis_defaults(color.bars = "#507289", show.plot.sc = FALSE)
#' setanalysis_defaults$color.bars
#'
#' # Unbekannte Einstellungen führen zu einem Fehler
#' try(change_analysis_defaults(farbe = "red"))
#'
#' # Vorherige Werte wiederherstellen
#' do.call(change_analysis_defaults, alte_werte)
#'
#' @export
change_analysis_defaults <- function(...) {
  changes <- list(...)

  # Überprüfen, ob die Einstellungsvariablen überhaupt existieren ---------
  for (i in seq_along(changes)) {
    if (!exists(names(changes)[i], envir = setanalysis_defaults)) {
      stop(paste0("Die Einstellungsvariable „", names(changes)[i], "“ existiert nicht."))
    }
  }

  # Einstellungen ändern --------------------------------------------------
  for (i in seq_along(changes)) {
    assign(names(changes)[i], changes[[i]], envir = setanalysis_defaults)
  }
}
