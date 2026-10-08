#' Numerische Frage auswerten
#'
#' @description
#' Erzeugt den Berichtsabschnitt für eine Frage mit Zahlen als Antwort (z. B.
#' Alter oder Abiturnote): Überschrift, Tabelle mit Kennwerten (n, M, SD,
#' Median, Minimum, Maximum) und ein Balkendiagramm der Häufigkeiten
#' ([barplot_freq()]). Für das Diagramm können die Werte in Klassen
#' eingeteilt (`cut.breaks`) oder ab einem Wert zusammengefasst werden
#' (`cutoff`). Die Tabelle beruht immer auf den ursprünglichen Werten.
#'
#' Der Abschnitt wird so ausgegeben, dass Überschrift, Tabelle und Abbildung
#' nicht durch einen Seitenumbruch getrennt werden. Enthält `x` nur fehlende
#' Werte, wird nichts ausgegeben.
#'
#' @param x Vektor mit den Antworten. Zahlen in Textform mit Dezimalkomma
#'   (z. B. `"1,7"`) werden umgewandelt. Erwartet das Attribut `label`.
#' @param xlab Beschriftung der x-Achse.
#' @param cut.breaks,cut.labels Grenzen und Beschriftungen der Klassen für das
#'   Diagramm (siehe [cut()], Argumente `breaks` und `labels`), oder `""` für
#'   keine Klassen.
#' @param show.table Soll die Tabelle gezeigt werden?
#' @param fig.height Höhe der Abbildung in Zoll.
#' @param cutoff Alle Werte ab diesem Wert im Diagramm zu einer Kategorie
#'   „`cutoff`+“ zusammenfassen, oder `FALSE`. Nicht zusammen mit
#'   `cut.breaks` möglich.
#' @inheritParams merge_sc
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#' @seealso [merge_fachsem()] für Fachsemester.
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' # Abiturnoten, im Diagramm in Notenbereiche eingeteilt
#' merge_num(BspDaten$dataSHOWUP$zugang_note,
#'   xlab = "Durchschnittsnote der Hochschulzugangsberechtigung",
#'   cut.breaks = c(0, 1.4, 1.9, 2.4, 2.9, 3.4, 4),
#'   cut.labels = c(
#'     "1,0 bis 1,4", "1,5 bis 1,9", "2,0 bis 2,4", "2,5 bis 2,9",
#'     "3,0 bis 3,4", "3,5 bis 4,0"
#'   )
#' )
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_num <- function(x,
                      inkl = "nr",
                      nr = "",
                      xlab = "",
                      cut.breaks = "",
                      cut.labels = "",
                      show.table = TRUE,
                      fig.height = 6,
                      cutoff = FALSE) {
  inkl <- .resolve_inkl(inkl, nr)

  if (inkl != TRUE) {
    return(invisible())
  } # wenn inkl nicht TRUE ist, wird die Funktion beendet

  if (cut.breaks[1] != "" && cutoff != FALSE) {
    stop('Es können nicht "cut.breaks" und "cutoff" != FALSE sein.')
  }

  if (!(sum(!is.na(x)) > 0)) {
    return(invisible())
  } # Falls es nur NA-Werte gibt,
  # wird die Funktion auch beendet

  # Seitenumbrüche zwischen Überschrift, Tabelle und Diagramm verhindern
  cat("::: {.block breakable=false}\n\n")


  # Fragetext mit Nummer und Label der Variable ausgeben
  cat("### ", nr, " ", attr(x, "label"), "\n \n")

  x <- as.numeric(gsub(",", ".", x)) # falls mit Komma

  if (show.table == TRUE) {
    subchunkify(table_stat_single(as.numeric(x, na.rm = TRUE),
      col1.name = "n", md = TRUE
    ))
    cat("  \n  \n")
  }

  if (cut.breaks[1] != "") {
    x <- cut(as.numeric(x, na.rm = TRUE),
      breaks = cut.breaks,
      labels = cut.labels
    )
  }

  if (cutoff != FALSE) {
    x[x >= cutoff] <- cutoff
    x <- factor(x)
    levels(x)[cutoff] <- paste0(cutoff, "+")
  }

  subchunkify(barplot_freq(x, xlab = xlab),
    fig_width = 9, fig_height = fig.height
  )

  cat("\n\n:::\n\n")
}
