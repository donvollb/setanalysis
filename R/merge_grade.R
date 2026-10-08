#' Gesamtnote auswerten
#'
#' @description
#' Erzeugt den Berichtsabschnitt für eine Gesamtnote im Schulnotenformat
#' (1 = sehr gut bis 6 = ungenügend): eine Tabelle mit Kennwerten und einen
#' Boxplot ([boxplot_grade()]). Standardmäßig werden die Noten zuerst je
#' Lehrveranstaltung gemittelt, sodass jede Lehrveranstaltung gleich stark
#' eingeht.
#'
#' @param x Vektor mit den Noten. Das Attribut `label` wird als Fragetext in
#'   der Tabelle verwendet.
#' @param kennung Vektor mit der Kennung der Lehrveranstaltung für jede Note.
#'   Nicht nötig bei `already.aggr = TRUE`.
#' @param show.table Soll die Tabelle gezeigt werden?
#' @param already.aggr Sind die Noten schon je Lehrveranstaltung gemittelt?
#'   Dann wird nicht noch einmal aggregiert.
#' @inheritParams merge_sc
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#' @seealso [aggr_data()] für die Aggregierung.
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' merge_grade(BspDaten$dataLVE$Note, kennung = BspDaten$dataLVE$Kennung)
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_grade <- function(x,
                        kennung,
                        show.table = TRUE,
                        already.aggr = FALSE,
                        inkl = "nr",
                        nr = "") {
  inkl <- .resolve_inkl(inkl, nr)

  if (inkl != TRUE) {
    return(invisible())
  } # wenn inkl nicht TRUE ist, wird die Funktion beendet

  label <- attr(x, "label")

  if (already.aggr == FALSE) {
    x <- aggr_data(x, kennung)
  }

  if (length(x) == 0) {
    cat("**Tabelle wurde wegen fehlender Daten nicht erstellt.** \n\n")
    return(invisible())
  }

  if (show.table == TRUE) {
    subchunkify(
      table_stat_multi(x,
        labels = label,
        col1.name = '#text(weight: "bold")[Item] _[Skala: Schulnoten]_',
        col2.name = "n",
        bold.corner = FALSE
      )
    )
  }
  subchunkify(boxplot_grade(x), fig_height = 2, fig_width = 9)
}
