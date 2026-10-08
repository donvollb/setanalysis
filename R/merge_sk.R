#' Skalenfrage auswerten (Einzelantworten)
#'
#' @description
#' Erzeugt den Berichtsabschnitt für eine einzelne Skalenfrage (z. B. eine
#' 6-stufige Likert-Skala): Überschrift, Tabelle mit Kennwerten (n, M, SD,
#' Median, Minimum, Maximum), optional die Häufigkeit von Ausweichoptionen
#' sowie eine Abbildung mit der Verteilung der Antworten, Mittelwert und
#' Standardabweichung ([barplot_sk()]).
#'
#' Der Abschnitt wird so ausgegeben, dass Überschrift, Tabelle und Abbildung
#' nicht durch einen Seitenumbruch getrennt werden.
#'
#' @param x Vektor mit den Antworten (Zahlen). Erwartet die Attribute `label`
#'   (Fragetext) und `labels` (benannte Antwortcodes; das erste und das
#'   `number`-te Label beschriften die Pole der Abbildung).
#' @param show.alt Sollen Ausweichoptionen und die Abbildung gezeigt werden?
#'   Bei `FALSE` werden nur Überschrift und Tabelle ausgegeben.
#' @param number Anzahl der Skalenstufen **ohne** Ausweichoptionen. Werte
#'   außerhalb von `1:number` gehen nicht in Tabelle und Abbildung ein.
#' @param alt1,alt2 Text der ersten bzw. zweiten Ausweichoption (z. B.
#'   `"kann ich nicht beurteilen"`) oder `FALSE`, wenn es sie nicht gibt.
#'   Ausgegeben wird, wie oft sie gewählt wurde.
#' @param alt1.num,alt2.num Code der ersten bzw. zweiten Ausweichoption in
#'   den Daten.
#' @param lime Liegen die Daten im Format eines LimeSurvey-Exports vor (Faktor
#'   mit den Antworttexten als Stufen)?
#' @param lime.brackets Nur mit `lime = TRUE`: Steht der eigentliche
#'   Fragetext in eckigen Klammern am Anfang des Labels? Dann wird nur dieser
#'   Teil verwendet.
#' @param show.plot Soll die Abbildung gezeigt werden? Voreinstellung aus
#'   [setanalysis_defaults] (`show.plot.sk`).
#' @inheritParams merge_sc
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#' @seealso [merge_aggr_sk()], um mehrere Skalenfragen gemeinsam oder auf
#'   Ebene von Lehrveranstaltungen aggregiert darzustellen.
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' merge_sk(BspDaten$dataSHOWUP$info_ausr_studgang)
#'
#' # Mit Ausweichoption (Code 0 in den Daten)
#' merge_sk(BspDaten$dataSHOWUP$info_ausr_studgang, alt1 = "kann ich nicht beurteilen")
#'
#' if (interactive()) markdown_in_viewer(merge_sk(BspDaten$dataSHOWUP$info_ausr_studgang))
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_sk <- function(x,
                     inkl = "nr",
                     nr = "",
                     show.alt = TRUE,
                     number = 6,
                     alt1 = FALSE,
                     alt2 = FALSE,
                     alt1.num = 0,
                     alt2.num = 7,
                     lime = FALSE,
                     lime.brackets = FALSE,
                     show.plot = setanalysis_defaults$show.plot.sk) {
  if (sum(!is.na(x)) == 0) {
    return(invisible())
  } # ohne gültige Daten nicht ausführen

  inkl <- .resolve_inkl(inkl, nr)

  if (inkl != TRUE) {
    return(invisible())
  } # nur ausführen, wenn inkl TRUE ist

  if (lime == TRUE) {
    label <- attr(x, "label")
    x_levels <- levels(x)
    x <- as.numeric(x, na.rm = TRUE)
    attr(x, "labels") <- setNames(seq_along(x_levels), x_levels)

    if (lime.brackets == TRUE) {
      label <- sub("^\\[", "", label)
      label <- sub("].*$", "", label)
    }

    attr(x, "label") <- label
  }

  # Seitenumbrüche innerhalb verhindern -----------------------------------

  cat("::: {.block breakable=false}\n\n")
  cat("###", nr, attr(x, "label"), "\n \n")
  cat("  \n  \n")

  xtab <- x
  xtab <- xtab[xtab %in% c(1:number)]

  subchunkify(table_stat_single(xtab, col1.name = "n", md = TRUE))

  cat("  \n  \n")

  if (show.alt == TRUE) {
    if (alt1 != FALSE) {
      cat("Die Ausweichoption *", alt1, "* wurde ",
        sum(x == alt1.num, na.rm = TRUE), " mal gewählt. \n\n",
        sep = ""
      )
    }

    if (alt2 != FALSE) {
      cat("Die Ausweichoption *", alt2, "* wurde ", sum(x == alt2.num, na.rm = TRUE),
        " mal gewählt. \n\n",
        sep = ""
      )
    }

    labels <- names(attributes(x)$labels)
    tmin <- labels[1]
    tmax <- labels[number]

    cat("  \n \n")

    if (show.plot == TRUE) {
      subchunkify(barplot_sk(x, tmin, tmax, number = number), fig_height = 2, fig_width = 9)
    }
  }
  cat("\n\n:::\n\n")
}
