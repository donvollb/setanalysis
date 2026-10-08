#' Single-Choice-Frage auswerten
#'
#' @description
#' Erzeugt den Berichtsabschnitt für eine Frage mit genau einer
#' Antwortmöglichkeit (Single Choice): Überschrift mit dem Fragetext, eine
#' Häufigkeitstabelle und optional ein Balkendiagramm.
#'
#' Wie alle Auswertungsfunktionen schreibt `merge_sc()` den Code für den
#' Bericht direkt in die Ausgabe. Sie wird deshalb in einem Quarto-Chunk mit
#' `output: asis` aufgerufen.
#'
#' @param x Vektor mit den Antworten (Antwortcodes). Erwartet die Attribute
#'   `label` (Fragetext) und `labels` (benannte Antwortcodes), wie sie
#'   [evasys_read_data()] erzeugt.
#' @param inkl Soll die Frage im Bericht erscheinen? `TRUE` oder `FALSE`.
#'   Beim Standard `"nr"` entscheidet die Variable `inkl.<nr>` (z. B.
#'   `inkl.2.1` bei `nr = "2.1"`), die im Berichts-Template gesetzt ist, meist
#'   aus der Berichtstabelle von [input_tabelle()]. Ohne Fragenummer wird die
#'   Frage immer ausgegeben.
#' @param nr Fragenummer, z. B. `"2.1"`. Sie wird der Überschrift vorangestellt
#'   und bestimmt bei `inkl = "nr"`, welche Variable `inkl.<nr>` abgefragt
#'   wird. Standard: `""` (keine Nummer).
#' @param fig.height Höhe der Abbildung in Zoll. Bei `"default"` wird sie aus
#'   der Anzahl der Antwortoptionen berechnet.
#' @param already.labels Liegen die Antworten schon als Text bzw. Faktor vor
#'   (`TRUE`)? Standardmäßig (`FALSE`) werden die Antwortcodes mithilfe des
#'   Attributs `labels` in Antworttexte übersetzt.
#' @param col2.name Überschrift der Spalte mit den absoluten Häufigkeiten.
#' @param order.table Reihenfolge der Antwortoptionen in der Tabelle: `FALSE`
#'   (Reihenfolge der Antwortoptionen), `"decreasing"` (nach Häufigkeit
#'   absteigend) oder ein anderer Wert, z. B. `TRUE` (aufsteigend).
#' @param show.plot Soll ein Balkendiagramm gezeigt werden? Voreinstellung aus
#'   [setanalysis_defaults] (`show.plot.sc`).
#' @param pagebreak Soll nach der Frage ein Seitenumbruch eingefügt werden?
#' @param digits Anzahl der Nachkommastellen der Prozentangaben.
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#' @seealso [merge_mc()] für Fragen mit mehreren Antwortmöglichkeiten,
#'   [merge_many()] für die automatische Auswertung mehrerer Fragen.
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' # Code für den Bericht (im Quarto-Dokument in einem Chunk mit `output: asis`)
#' merge_sc(BspDaten$dataLVE$V3_D)
#'
#' # Nach Häufigkeit sortiert, ohne Abbildung
#' merge_sc(BspDaten$dataLVE$FachSemN, order.table = "decreasing", show.plot = FALSE)
#'
#' # Vorschau, wie der Abschnitt im Bericht aussieht
#' if (interactive()) markdown_in_viewer(merge_sc(BspDaten$dataLVE$V3_D))
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_sc <- function(x,
                     inkl = "nr",
                     nr = "",
                     fig.height = "default",
                     already.labels = FALSE,
                     col2.name = "n",
                     order.table = FALSE,
                     show.plot = setanalysis_defaults$show.plot.sc,
                     pagebreak = FALSE,
                     digits = 1) {
  inkl <- .resolve_inkl(inkl, nr)

  if (inkl != TRUE) {
    return(invisible())
  } # wenn inkl nicht TRUE, wird Funktion beendet

  if (sum(!is.na(x)) == 0) {
    return(invisible())
  } # selbiges bei fehlenden Werten
  if (already.labels == FALSE) {
    x_attributes <- attributes(x)
    x <- factor(x, levels = x_attributes$labels, labels = names(x_attributes$labels))
    attr(x, "label") <- x_attributes$label
  }

  cat("###", nr, attr(x, "label"), "\n \n")

  subchunkify(table_freq(x,
    col1.name = "Antwortoption", col2.name = col2.name,
    order.table = order.table, digits = digits
  ))

  freq_table <- descr::freq(x, plot = FALSE)
  results <- data.frame(rownames(freq_table), round(freq_table[, 1:2], digits = 2))

  # automatische Zeilenumbrüche bei langen Labels -------------------------

  results[, 1] <- .wrap_labels(results[, 1], width = 40)

  results <- results[!(rownames(results) %in% c("NA's", "Total")), ]
  colnames(results) <- c("label", "freq", "perc")

  if (show.plot == TRUE) {
    # Höhe der Abbildung: eine Zeile pro Antwortoption plus Rand
    if (fig.height == "default") {
      fig.height <- 1 + nrow(results)
    }

    subchunkify(barplot_scmc(results, xlab = "Häufigkeit"),
      fig_height = fig.height, fig_width = 9
    )
  }

  if (pagebreak == TRUE) {
    cat("\n {{< pagebreak >}}")
  }

  cat("\n \n")
}
