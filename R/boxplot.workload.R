#' Boxplot mit Workloads der LVs: Funktioniert, sollte überarbeitet werden
#' 
#' @param x Daten
#' @param skala Skala x-Achse
#'
#' @examples boxplot.workload(x = BspDaten$Plots$WL)
#' 
#' @returns Boxplot
#' 
#' @export boxplot.workload

boxplot.workload <- function(x, # Daten
                             skala = c("0h", "1h", "2h", "3h", "4h", "5h", "6h", "7h",
                                   "8h", "9h", "10h", "11h", "12h", "mehr als\n12h")) # Skala x-Achse

{
  
  # Berechnung der Anzahl der Veranstaltungen und Länge der Skala -------
  
  n <- length(x)
  n_skala <- length(skala)
  
  # Bisherige Grafikparameter speichern -----------------------------------
  
  opar <- par(no.readonly = TRUE)

  # Grafikparameter für den Plot einstellen -------------------------------
  
  par(fg = "gray80",  # Farbe des Grid (die Hilfslinen) festlegen
      family = set.analysis.defaults$font.family, # Schriftart festlegen
      mar=c(7, 4.1, 2, 4.2), # Seitenränder festlegen
      lend = "square", # Linienenden eckig
      ljoin = "mitre") # Linienschnitt eckig
  
  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------
  
  plot(
    1,  # Dummy-Wert
    type = "n",  # Keine Daten zeichnen
    xlim = c(1, n_skala),  # x-Achse: Skalenwerte (horizontaler Boxplot!)
    xlab = "", ylab = "",  # Keine Achsenbeschriftung
    xaxt = "n", yaxt = "n",  # Keine Achsen zeichnen
    bty = "n"  # Kein Rahmen
  )
  
  
  # Einfügen von vertikalen Linien ----------------------------------------
  
  abline(v = c(1:n_skala))
  
  # Plot über die Vertikalen Linien drüber plotten ------------------------

  boxplot(x, ylab = NULL, xlab = NULL, horizontal = TRUE, ylim=c(1:n_skala),
          xaxt = "n", yaxt = "n", #boxwex = 0.8,
          border="black",
          pars =list(outcol = set.analysis.defaults$color.bars, outpch = 20),
          col = set.analysis.defaults$color.bars, add = TRUE)
  
  # Achsenmarkierungen und Beschriftungen einfügen ------------------------
  
  #axis(side = 1, at = 1:n_skala, labels = FALSE)
  mtext(skala, side = 1, line = 1, at = 1:n_skala, las=1, col="black", cex=0.8)
  
  info1 <- "angegebener Workload der LV"
  info2 <- paste0("[n = ", n, "]")
  mtext(bquote(bold(.(info1)) ~ .(info2)), side = 1, line = 3, col="gray30")

  # Ursprüngliche Grafikparameter wiederherstellen ------------------------
  
  par(opar)
}