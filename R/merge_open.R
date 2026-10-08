#' Funktion für offene Antworten
#' Die Funktion war ehemals auf zwei (jetzt veraltete) Funktionen aufgeteilt:
#' - `open.answers()`: Verweis auf Anhang bei Berichten mit Anhang
#' - `merge.open()`: Eigentliche Auswertung der offenen Antworten
#'
#' @param x Daten
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param inkl_global Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#' @param freq Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt.
#' "auto" führt zur Anzeige der Häufigkeiten, wenn Antworten mehrfach vorkommen, sonst nicht.
#' @param appendix Gibt es einen Extra-Anhang, in dem die offenen Antworten gesammelt werden sollen?
#' @param is_appendix Nur relevant, falls es einen Anhang gibt. Wenn TRUE, wird der Output für den Anhang erzeugt.
#' @param anchor Nur relevant, falls es einen Anhang gibt. Anker, damit auf den Output weiter oben im pdf Verlinkt werden kann.
#'
#' @examples
#'
#' # Beispiel für Bericht mit Anhang – Häufigkeiten werden angezeigt
#' {
#'   merge_open(BspDaten$dataSHOWUP$offen, appendix = TRUE)
#'   appendix.open()
#' } |> markdown_in_viewer()
#'
#' # Ergebnis für Bericht ohne Anhang – Ohne Häufigkeiten, weil jeder Eintrag nur einmal
#' merge_open(BspDaten$dataSHOWUP$offen, appendix = FALSE) |> markdown_in_viewer()
#'
#' @export merge_open
#'

merge_open <- function(x, # Daten
                       inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                       inkl_global = setanalysis_defaults$inkl.open, # Zweite inkl-Variable, die die globale Variable "inkl.open" abfragt. Kann auch in TRUE oder FALSE geändert werden
                       nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                       freq = "auto", # Sollen gleiche offene Antworten zusammengefasst werden? Dann werden auch Häufigkeiten angezeigt
                       appendix = setanalysis_defaults$open.appendix, # Gibt es einen Extra-Anhang, in dem die offenen Antworten gesammelt werden sollen?
                       is_appendix = FALSE, # Nur relevant, falls es einen Anhang gibt. Wenn TRUE, wird der Output für den Anhang erzeugt.
                       anchor = FALSE) # Nur relevant, falls es einen Anhang gibt. Wenn TRUE, wird der Output für den Anhang erzeugt.
{
  inkl <- .resolve_inkl(inkl, nr)

  if (inkl != TRUE || inkl_global != TRUE) {
    return(invisible())
  } # wenn nicht beide inkl-Arugmente TRUE sind, wird Funktion beendet

  # Erzeugung des Outputs für den Hauptteil der Berichte, falls ----------
  # es einen Extra Anhang für die offenen Antworten gibt -----------------

  if (appendix == TRUE && is_appendix == FALSE) {
    list_open_answers$anchor.nr <- list_open_answers$anchor.nr + 1
    anchor.nr <- list_open_answers$anchor.nr
    cat(paste0("### ", nr, " ", attr(x, "label"), " {#sec-", anchor.nr, ".top} \n\n"))

    if (length(na.omit(x)) > 0) {
      cat(paste0(
        "*Die offenen Antworten zu dieser Frage finden sich* ",
        "[im Anhang](#sec-", anchor.nr, ".bottom).  \n\n\\\n\n"
      ))
    } else {
      cat("*Keine offenen Antworten zu dieser Frage.*  \n\n\\\n\n")
    }

    assign(paste0("var.", anchor.nr), x, envir = list_open_answers)
    assign(paste0("nr.", anchor.nr), nr, envir = list_open_answers)
    return(invisible())
  }

  # Erzeugung des eigentlichen Outputs mit den offenen Antworten ---------
  # (ohne Anhang oder im Anhang selbst)

  if (anchor != FALSE) {
    cat("###", nr, attr(x, "label"), paste0("{#sec-", anchor, ".bottom}"), "\n \n")
    cat(paste0("[zurück nach oben](#sec-", anchor, ".top) \n\n"))
  } else {
    cat("###", nr, attr(x, "label"), "\n \n")
  }

  if (length(na.omit(x)) == 0) {
    cat("*Keine offenen Antworten zu dieser Frage.*  \n\n")
    return(invisible())
  }

  # NAs entfernen, Leerzeichen vorne und hinten entfernen, sortieren -----

  x <- trimws(x[!is.na(x)])
  x <- x[order(x)]

  # Herausfinden, ob Häufigkeitstabelle sinnvoll ist (Gibt es Antworten mehrmals?)

  if (freq == "auto") {
    freq <- length(unique(tolower(x))) < length(x)
  }

  # Tabelle mit oder ohne Häufigkeiten erzeugen -------------------------

  if (freq == TRUE) {
    # Gruppen nach Kleinbuchstaben bilden
    groups <- split(x, tolower(x))

    # für jede Gruppe: die häufigste Schreibweise auswählen
    most_used <- function(x) names(which.max(table(x)))
    main_spellings <- sapply(groups, most_used)

    # Häufigkeiten (aller Varianten) zählen
    counts <- lengths(groups)

    # Tabelle mit den Repräsentanten und den Häufigkeiten
    freq_table <- data.frame(
      Antwort = main_spellings,
      "Häufigkeit" = counts,
      row.names = NULL
    )

    # Nach Häufigkeit sortieren
    freq_table <- freq_table[order(-freq_table[["Häufigkeit"]], freq_table$Antwort), ]

    # Formatierung der Tabelle
    subchunkify(lv_table(freq_table, col.width = c(137, 18), striped = FALSE))
  } else {
    cat("*Die folgenden Antworten wurden jeweils nur einmal gegeben:*  \n\n")

    subchunkify(lv_table(data.frame(Antwort = x), col.width = 159, striped = FALSE))
  }

  cat(" \n\n")
}

#' Funktion um alle offenen Antworten unten in den Anhang zu packen
#' `appendix.open()` ist eine veraltete Schreibweise der gleichen Funktion
#'
#' @param freq Sollen die offenen Antworten nach Häufigkeit gruppiert werden?
#'
#' @examples
#' # Damit diese Funktion sinnvoll funktioniert, muss vorher mindestens eine
#' # offene Frage aufgerufen worden
#' invisible(capture.output(merge_open(BspDaten$dataSHOWUP$offen, appendix = TRUE)))
#' appendix_open() |> markdown_in_viewer()
#'
#' @export appendix_open

appendix_open <- function(freq = "auto") {
  anchor.nr <- list_open_answers$anchor.nr

  if (anchor.nr == 0) {
    return(invisible())
  } # stoppen, wenn keine offenen Fragen aufgerufen wurden

  cat("# Anhang: Fragen mit offenem Antwortformat  \n  \n")

  for (k in seq_len(anchor.nr)) {
    x <- list_open_answers[[paste0("var.", k)]]
    question_nr <- list_open_answers[[paste0("nr.", k)]]
    merge_open(x,
      nr = question_nr, anchor = k, freq = freq,
      appendix = TRUE, is_appendix = TRUE
    )
  }
}
