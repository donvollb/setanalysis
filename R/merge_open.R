#' Offene Frage auswerten
#'
#' @description
#' Gibt die Antworten auf eine offene Frage als Tabelle aus. Kommen Antworten
#' mehrfach vor (Groß- und Kleinschreibung wird dabei ignoriert), zeigt die
#' Tabelle jede Antwort einmal mit ihrer Häufigkeit, sortiert nach Häufigkeit.
#'
#' Bei Berichten mit Anhang (`appendix = TRUE`, Standard) erscheint an der
#' Stelle der Frage nur die Überschrift mit einem Link in den Anhang. Die
#' Antworten werden gesammelt und am Ende des Berichts mit [appendix_open()]
#' ausgegeben.
#'
#' @param x Vektor (Text) mit den Antworten. Erwartet das Attribut `label`
#'   (Fragetext). Fehlende Antworten (`NA`) werden ignoriert.
#' @param inkl_global Zweiter Schalter, der alle offenen Fragen eines Berichts
#'   gemeinsam ein- oder ausschließt. Voreinstellung aus
#'   [setanalysis_defaults] (`inkl.open`).
#' @param freq Sollen gleiche Antworten zusammengefasst und mit Häufigkeit
#'   gezeigt werden? `TRUE`, `FALSE` oder `"auto"` (nur, wenn mindestens eine
#'   Antwort mehrfach vorkommt).
#' @param appendix Sollen die Antworten im Anhang statt an Ort und Stelle
#'   stehen? Voreinstellung aus [setanalysis_defaults] (`open.appendix`).
#' @param is_appendix,anchor Werden intern von [appendix_open()] gesetzt, um
#'   die Ausgabe im Anhang mit Rücksprung-Link zu erzeugen.
#' @inheritParams merge_sc
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#' @seealso [appendix_open()] für die Ausgabe des Anhangs.
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' # Antworten direkt an Ort und Stelle
#' merge_open(BspDaten$dataSHOWUP$offen, appendix = FALSE)
#'
#' # Bericht mit Anhang: zuerst nur Überschrift und Link, am Ende der Anhang
#' merge_open(BspDaten$dataSHOWUP$offen, appendix = TRUE)
#' appendix_open()
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_open <- function(x,
                       inkl = "nr",
                       inkl_global = setanalysis_defaults$inkl.open,
                       nr = "",
                       freq = "auto",
                       appendix = setanalysis_defaults$open.appendix,
                       is_appendix = FALSE,
                       anchor = FALSE) {
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

#' Anhang mit den offenen Antworten ausgeben
#'
#' @description
#' Gibt den Anhang „Fragen mit offenem Antwortformat“ aus: alle offenen
#' Fragen, die zuvor mit `merge_open(..., appendix = TRUE)` aufgerufen wurden,
#' jeweils mit Link zurück zur Stelle im Bericht.
#'
#' `appendix_open()` gehört ans Ende jedes Berichts mit Anhang. Danach wird
#' der Speicher der offenen Antworten ([list_open_answers]) geleert, damit ein
#' weiterer Bericht in derselben R-Sitzung neu beginnt.
#'
#' @param freq Sollen gleiche Antworten zusammengefasst werden? Siehe
#'   [merge_open()].
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben. Wurde vorher keine offene Frage mit Anhang
#'   aufgerufen, wird nichts ausgegeben.
#'
#' @family auswertung
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' merge_open(BspDaten$dataSHOWUP$offen, appendix = TRUE)
#'
#' # … weitere Fragen des Berichts …
#'
#' appendix_open()
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
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

  # Gesammelte Antworten löschen, damit ein weiterer Bericht in derselben
  # R-Sitzung nicht die offenen Antworten dieses Berichts übernimmt
  rm(list = ls(list_open_answers, all.names = TRUE), envir = list_open_answers)
  list_open_answers$anchor.nr <- 0
}
