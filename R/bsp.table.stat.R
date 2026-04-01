#' Beispiel einer Statistik-Tabelle
#'
#' @param all Soll die Tabelle Fragen enthalten? Dann TRUE, sonst FALSE
#'
#' @returns
#' Eine Tabelle mit den Spalten "Häufigkeit", "Mittelwert", "Standardabweichung",
#' "Median", "kleinster beob. Wert", "größter beob. Wert"
#'
#' @examples bsp.table.stat()
#'
#' @export bsp.table.stat

bsp.table.stat <- function(all = TRUE)  # all = TRUE für eine Tabelle mit "Frage" und "Median", eher für LVE
{
  if(all == TRUE) {

    jim <- data.frame(cbind("Frage", "Häufigkeit", "Mittelwert",
                            "Standard-abweichung", "Median",
                            "kleinster be⁠ob. Wert", "größter be⁠ob. Wert"))
    
    colnames(jim) <- c("Item", "n", "M", "SD", "MD", "Min", "Max")
    
  } else {

    jim <- data.frame(cbind("Häufigkeit", "Mittelwert", "Standard-\nabweichung",
                            "kleinster\nbeob. Wert", "größter\nbeob. Wert"))
    
    colnames(jim) <- c("n", "M", "SD", "Min", "Max")
  }

  return(lv_table(jim))
}
