# Beispiel einer Statistik-Tabelle
bsp.table.stat <- function(all = TRUE)  # all = TRUE für eine Tabelle mit "Frage" und "Median", eher für LVE
{
  if(all == TRUE) {

    jim <- as.data.frame(cbind("Frage", "Häufigkeit", "Mittelwert", "Standard-\nabweichung", "Median", "kleinster beob. Wert", "größter beob. Wert"))
    colnames(jim) <- c("Item", "n", "M", "SD", "MD", "Min", "Max")
    #    tab <- lv.kable(jim, col.width = c("30pt", "50pt", "50pt", "60pt", "35pt", "60pt", "60pt"))
    tab <- lv.kable(jim, col.width = c(11, 18, 18, 21, 12, 21, 21))

  } else {

    jim <- as.data.frame(cbind("Häufigkeit", "Mittelwert", "Standard-\nabweichung", "kleinster beob. Wert", "größter beob. Wert"))
    colnames(jim) <- c("n", "M", "SD", "Min", "Max")
    #    tab <- lv.kable(jim, col.width = c("50pt", "50pt", "60pt", "60pt", "60pt"))
    tab <- lv.kable(jim, col.width = c(18, 18, 21, 21, 21))
  }

  return(tab)

}
