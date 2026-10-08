#' merge-Funktion für den Workload (funktioniert, sollte überarbeitet werden)
#' `merge.wl()` ist eine veraltete Schreibweise der gleichen Funktion

#'
#' @param WL WL der Daten
#' @param kennung Kennung/Fallnummer zum Aggregieren
#' @param already.aggr Sind die Daten bereits aggregiert?
#'
#' @examples

#' merge_wl(BspDaten$dataLVE$WL, already.aggr = FALSE,
#'          BspDaten$dataLVE$Kennung) |> markdown_in_viewer()
#'
#' @export merge_wl

merge_wl <- function(WL, # WL der Daten
                     kennung, # Kennung/Fallnummer zum Aggregieren
                     already.aggr = FALSE) # Sind die Daten bereits aggregiert?
{
  # Label aus den Daten ziehen und als Überschrift drucken ----------------

  wl.label <- attr(WL, "label")
  cat(paste("###", wl.label, "\n\n\n\n"))

  # Noch nicht aggregierte Daten aggregieren ------------------------------

  # (Median je Kennung, in der Reihenfolge des ersten Auftretens)

  if (already.aggr == FALSE) {
    WL <- vapply(unique(kennung), \(k) median(WL[kennung == k], na.rm = TRUE), numeric(1))
  }

  # Boxplot erstellen -----------------------------------------------------

  subchunkify(boxplot_wl(WL), fig_width = 9, fig_height = 4)
}
