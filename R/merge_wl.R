#' Workload der Lehrveranstaltungen auswerten
#'
#' @description
#' Gibt den Fragetext als Überschrift und einen Boxplot des angegebenen
#' Workloads (Stunden pro Woche) aus ([boxplot_wl()]). Standardmäßig wird
#' zuerst der Median je Lehrveranstaltung gebildet, sodass jede
#' Lehrveranstaltung gleich stark eingeht.
#'
#' @param WL Vektor mit dem angegebenen Workload in Stunden pro Woche.
#'   Erwartet das Attribut `label` (Fragetext).
#' @param kennung Vektor mit der Kennung der Lehrveranstaltung für jede
#'   Antwort. Nicht nötig bei `already.aggr = TRUE`.
#' @param already.aggr Liegt der Workload schon je Lehrveranstaltung vor? Dann
#'   wird nicht noch einmal aggregiert.
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' merge_wl(BspDaten$dataLVE$WL, kennung = BspDaten$dataLVE$Kennung)
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_wl <- function(WL, kennung, already.aggr = FALSE) {
  # Label aus den Daten ziehen und als Überschrift drucken ----------------

  label <- attr(WL, "label")
  cat(paste("###", label, "\n\n\n\n"))

  # Noch nicht aggregierte Daten aggregieren ------------------------------

  # (Median je Kennung, in der Reihenfolge des ersten Auftretens)

  if (already.aggr == FALSE) {
    WL <- vapply(unique(kennung), \(k) median(WL[kennung == k], na.rm = TRUE), numeric(1))
  }

  # Boxplot erstellen -----------------------------------------------------

  subchunkify(boxplot_wl(WL), fig_width = 9, fig_height = 4)
}
