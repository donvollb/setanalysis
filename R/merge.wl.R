#' merge-Funktion für den Workload (funktioniert, sollte überarbeitet werden)
#'
#' @param ECTS ECTS der Daten
#' @param WL WL der Daten (ECTS und WL müssen gleiche Länge haben, d.h. entweder ist bereits beides aggregiert oder keins)
#' @param kennung Kennung/Fallnummer zum Aggregieren
#' @param already.aggr Sind die Daten bereits aggregiert?
#'
#' @examples

#' merge.wl(BspDaten$dataLVE$WL,
#' BspDaten$dataLVE$Kennung, already.aggr = FALSE) |> markdown.in.viewer()
#' 
#' @export

merge.wl <- function(WL, # WL der Daten (ECTS und WL müssen gleiche Länge haben, d.h. entweder ist bereits beides aggregiert oder keins)
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

  subchunkify(boxplot.workload(WL), fig_width = 9, fig_height = 4)

}
