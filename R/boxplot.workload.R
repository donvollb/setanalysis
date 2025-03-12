#' Boxplot mit Workloads der LVs: Funktioniert, sollte überarbeitet werden
#' 
#' @param x Daten
#' @param q Skala x Achse
#' @param e Anzahl der Antwortoptionen der Skala
#' @param n Anzahl der Veranstaltungen
#'
#' @examples
#' boxplot.workload(x = BspDaten$dataLVE$WL)
#' 
#' @returns Boxplot
#' @export

boxplot.workload <- function(x, # Daten
                             q = c("0h", "1h", "2h", "3h", "4h", "5h", "6h", "7h",
                                   "8h", "9h", "10h", "11h", "12h", "mehr als\n12h"), # Skala x Achse
                             e = 13, # Anzahl der Antwortoptionen der Skala
                             n = NA) # Anzahl der Veranstaltungen
{
  
  # Berechnung der Anzahl der Veranstaltungen falls nicht angegeben -------
  
  if(is.na(n)) {n <- length(x)}
  
  # Bisherige Grafikparameter speichern -----------------------------------
  
  opar <- par(no.readonly = TRUE)

  # Grafikparameter für den Plot einstellen -------------------------------
  
  par(fg = "gray80",  # Farbe des Grid (die Hilfslinen) festlegen
      family = set.analysis.defaults$font.family, # Schriftart festlegen
      mar=c(7, 4.1, 2, 4.2)) # Seitenränder festlegen
  
  # Plot erstellen (nur damit gleich die Vertikalen Linien stimmen) -------
  
  boxplot(x, ylab = NULL, xlab = NULL, horizontal = TRUE, ylim = c(0,e),
          xaxt = "n", yaxt = "n", boxwex = 0.8)
  
  # Einfügen von vertikalen Linien
  
  abline(v = c(0:e))
  
  # Plot über die Vertikalen Linien drüber plotten ------------------------

  boxplot(x, ylab = NULL, xlab = NULL, horizontal = TRUE, ylim=c(0,e),
          xaxt = "n", yaxt = "n", boxwex = 0.8, border="black",
          pars=list(outcol = set.analysis.defaults$color.bars, outpch = 20),
          col = set.analysis.defaults$color.bars, add = TRUE)
  
  # Achsenmarkierungen und Beschriftungen einfügen ------------------------
  
  axis(side = 1, at = 0:e, labels = FALSE)
  mtext(q,side=1, line=1, at=0:e, las=1, col="black", cex=0.8)
  
  info1 <- "angegebener Workload der LV"
  info2 <- paste0("[n = ", n, "]")
  mtext(bquote(bold(.(info1)) ~ .(info2)), side = 1, line = 3, col="gray30")

  # Ursprüngliche Grafikparameter wiederherstellen ------------------------
  
  par(opar)
}