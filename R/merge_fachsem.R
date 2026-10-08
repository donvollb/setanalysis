#' Fachsemester auswerten
#'
#' @description
#' Erzeugt den Berichtsabschnitt zur Frage nach dem Fachsemester (LVE):
#' Überschrift, Häufigkeitstabelle und Balkendiagramm. Alle Semester ab
#' `cutoff` werden zu einer Kategorie zusammengefasst (z. B. „12+“).
#'
#' @param x Vektor mit den Fachsemestern (Zahlen).
#' @param fig.height Höhe der Abbildung in Zoll. Beim Standard 5 passen
#'   Tabelle und Abbildung bei `cutoff = 12` auf eine Seite.
#' @param cutoff Ab diesem Fachsemester werden alle Werte zusammengefasst.
#' @param group Für welche Gruppe gilt die Auswertung? `"a"` (alle), `"b"`
#'   (nur Bachelor) oder `"m"` (nur Master). Bestimmt Überschrift und
#'   Tabellenkopf; die Daten müssen vorher entsprechend gefiltert sein.
#' @inheritParams merge_sc
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#' @seealso [merge_num()] für andere numerische Fragen.
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' merge_fachsem(BspDaten$dataLVE$FachSemN)
#'
#' # Bis zum 8. Semester einzeln, danach zusammengefasst
#' merge_fachsem(BspDaten$dataLVE$FachSemN, cutoff = 8)
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_fachsem <- function(x,
                          fig.height = 5,
                          cutoff = 12,
                          group = "a",
                          inkl = "nr",
                          nr = "") {
  inkl <- .resolve_inkl(inkl, nr)

  if (inkl != TRUE) {
    return(invisible())
  }

  # Beschriftungen je nach Gruppe -----------------------------------------

  captions <- c(a = "(alle)", b = "(nur Bachelor)", m = "(nur Master)")
  col_names <- c(
    a = "Fachsemester alle", b = "Fachsemester Bachelor", m = "Fachsemester Master"
  )
  if (!group %in% names(captions)) {
    stop('`group` muss "a" (alle), "b" (Bachelor) oder "m" (Master) sein.', call. = FALSE)
  }

  # Alle Werte ab `cutoff` zusammenfassen ---------------------------------

  x[x >= cutoff] <- cutoff
  x <- factor(x)
  levels(x)[cutoff] <- paste0(cutoff, "+")

  # Ausgabe ---------------------------------------------------------------

  cat(paste0("## Fachsemester ", captions[[group]], "  \n  \n"))
  cat("### Bezogen auf das Fach, dem die vorliegende Veranstaltung zugehört: in welchem Fachsemester sind Sie eingeschrieben?  \n  \n")

  subchunkify(
    table_freq(x,
      col1.name = col_names[[group]], col2.name = "n",
      cutoff = cutoff
    )
  )

  cat("  \n  \n")
  subchunkify(barplot_freq(x, xlab = "Fachsemester"), fig_height = fig.height, fig_width = 10)
  cat("  \n  \n")
}
