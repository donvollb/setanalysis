#' merge-Funktion für den Workload (funktioniert, sollte überarbeitet werden)
#' `merge.wl()` ist eine veraltete Schreibweise der gleichen Funktion

#'
#' @param WL WL der Daten
#' @param kennung Kennung/Fallnummer zum Aggregieren
#' @param already.aggr Sind die Daten bereits aggregiert?
#'
#' @examples

#' merge_wl(BspDaten$dataLVE$WL, already.aggr = FALSE
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
  
    if(already.aggr == FALSE) {
    wl.aggr <- vector()

  ## Schleife, die alle Kennungen durchgeht und den Median berechnet ------
    
    for (Ausgewählte.Kennung in unique(kennung)) {
      
      wl.tmp <- WL[kennung == Ausgewählte.Kennung]
      md <- median(wl.tmp, na.rm = TRUE)
      wl.aggr[length(wl.aggr) + 1] <- md
    }
    WL <- wl.aggr
  }
  
  # Boxplot erstellen -----------------------------------------------------

  subchunkify(boxplot_wl(WL), fig_width = 9, fig_height = 4)

}

#' @noRd
#' @export

merge.wl <- merge_wl