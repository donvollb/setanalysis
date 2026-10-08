#' Multiple-Choice-Frage auswerten
#'
#' @description
#' Erzeugt den Berichtsabschnitt für eine Frage, bei der mehrere Antworten
#' gewählt werden können (Multiple Choice): Überschrift, Tabelle mit der
#' Häufigkeit jeder Antwortoption und optional ein Balkendiagramm.
#'
#' @param x Data Frame mit einer Spalte pro Antwortoption. Ein Wert ungleich 0
#'   bedeutet „gewählt“, 0 „nicht gewählt“, `NA` „keine Angabe“. Jede Spalte
#'   braucht das Attribut `label` in der Form `"Fragetext : Antwortoption"`
#'   (wie von [evasys_read_data()] erzeugt).
#' @param head Fragetext für die Überschrift. Bei `"default"` wird er aus dem
#'   Label der ersten Spalte genommen (Text vor dem `:`).
#' @param col1.name Überschrift der Spalte mit den Antwortoptionen.
#' @param show.table Soll die Tabelle gezeigt werden?
#' @param fig.height Höhe der Abbildung in Zoll. Bei `"default"` wird sie aus
#'   der Anzahl der Antwortoptionen berechnet.
#' @param lime Liegen die Daten im Format eines LimeSurvey-Exports vor
#'   (1 = gewählt, 2 = nicht gewählt; Label `"[Antwortoption] Fragetext"`)?
#' @param filter Nur mit `lime = TRUE`: Text, der dem Fragetext in eckigen
#'   Klammern vorangestellt wird (z. B. ein Filterhinweis), oder `FALSE`.
#' @param valid.perc Sollen zusätzlich gültige Prozent (ohne fehlende Angaben)
#'   sowie die Zeilen „NAs“ und „Total“ gezeigt werden?
#' @param show.plot Soll ein Balkendiagramm gezeigt werden? Voreinstellung aus
#'   [setanalysis_defaults] (`show.plot.mc`).
#' @inheritParams merge_sc
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#' @seealso [merge_sc()] für Fragen mit genau einer Antwortmöglichkeit.
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' abschluesse <- BspDaten$dataSHOWUP[, paste0("abschluss_", 1:8)]
#'
#' merge_mc(abschluesse)
#'
#' # Nach Häufigkeit sortiert, ohne gültige Prozent und ohne Abbildung
#' merge_mc(abschluesse, order.table = "decreasing", valid.perc = FALSE, show.plot = FALSE)
#'
#' if (interactive()) markdown_in_viewer(merge_mc(abschluesse))
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_mc <- function(x,
                     head = "default",
                     col1.name = "Antwortoption",
                     col2.name = "n",
                     show.table = TRUE,
                     fig.height = "default",
                     inkl = "nr",
                     nr = "",
                     lime = FALSE,
                     filter = FALSE,
                     valid.perc = TRUE,
                     order.table = FALSE,
                     digits = 1,
                     show.plot = setanalysis_defaults$show.plot.mc) {
  inkl <- .resolve_inkl(inkl, nr)

  if (inkl != TRUE) {
    return(invisible())
  } # wenn inkl nicht TRUE, wird Funktion beendet

  if (lime == TRUE) {
    for (l in seq_len(ncol(x))) {
      label <- attr(x[, l], "label")
      answer <- sub("\\].*", "", label)
      answer <- sub("\\[", "", answer)
      label_cut <- sub(".*].", "", label)


      x[, l] <- as.numeric(x[, l], na.rm = TRUE)
      x[x[, l] == 2 & !is.na(x[, l]), l] <- 0
      x[x[, l] == 1 & !is.na(x[, l]), l] <- l


      attr(x[, l], "label") <- paste0(label_cut, " (Mehrfachantwort möglich): ", answer)
    }

    if (filter != FALSE) {
      attr(x[, 1], "label") <- paste0("[", filter, "] ", attr(x[, 1], "label"))
    }
  }

  if (head == "default") {
    head <- sub(":.*", "", attr(x[, 1], "label"))
  }

  cat("### ", nr, head, "\n \n")

  option_labels <- sub(".*: ", "", as.character(lapply(x, attr, which = "label")))


  # Wie oft wurde jede Antwortoption gewählt (Wert ungleich 0)?
  counts <- vapply(seq_len(length(x)), \(n) sum(x[, n] != 0, na.rm = TRUE), integer(1))

  if (valid.perc == TRUE) {
    results <- data.frame(matrix(nrow = length(x), ncol = 4))
    colnames(results) <- c(col1.name, col2.name, "%", "gültige %")
    results[, 1] <- option_labels
    results[, 2] <- counts
    results[, 3] <- counts / nrow(x) * 100
    results[, 4] <- counts / nrow(x[!is.na(x[, 1]), ]) * 100

    if (order.table != FALSE) {
      decreasing <- order.table == "decreasing"
      results <- results[order(results[, 2], decreasing = decreasing), ]
    }

    results[nrow(results) + 1, ] <- list(
      "NAs", nrow(x[is.na(x[, 1]), ]),
      nrow(x[is.na(x[, 1]), ]) / nrow(x) * 100, NA
    )

    results[nrow(results) + 1, ] <- list("Total", nrow(x), NA, NA)
  } else {
    results <- data.frame(matrix(nrow = length(x), ncol = 3))

    colnames(results) <- c(col1.name, "N_votes", "\\%")
    results[, 1] <- option_labels
    results[, 2] <- as.numeric(counts)
    results[, 3] <- counts / nrow(x) * 100
  }

  if (ncol(results) == 4) {
    col.width <- setanalysis_defaults$col.width4
  } else {
    col.width <- setanalysis_defaults$col.width3
  }

  if (show.table == TRUE) {
    subchunkify(
      lv_table(results,
        col.width = col.width,
        digits = digits
      ),
      fig_height = 7, fig_width = 9
    )
  }

  # Für die Abbildung nur die Antwortoptionen verwenden -------------------
  # (die Zeilen "NAs" und "Total" stehen nur in der Tabelle)

  results <- results[seq_len(length(x)), ]

  colnames(results) <- c("label", "freq", "perc")

  # Automatische Zeilenumbrüche einfügen ----------------------------------

  results[, 1] <- .wrap_labels(results[, 1], width = 40)

  # freq-Spale in numerische Daten umwandeln (bisher character) ---------

  results$freq <- as.numeric(results$freq)

  if (show.plot == TRUE) {
    # Automatisch die Höhe der Abbildung festlegen ----------------------

    if (fig.height == "default") {
      fig.height <- (1 + 0.75 * nrow(results))
    }

    subchunkify(barplot_scmc(x = results, xlab = "Häufigkeit"),
      fig_height = fig.height, fig_width = 9
    )
  }

  cat("  \n  \n")
}
