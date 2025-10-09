#' Boxplots für Skalenfragen auf aggregiertem Niveau: Funktioniert, sollte aber überarbeitet werden
#'
#' @param x Daten
#' @param item_labels Labels/Text/Beschriftungen der Y-Achse
#' @param skala Skala der x-Achse
#' @param color Farbe der Boxen
#' @param font_family Schriftart
#'
#' @returns Boxplot
#' 
#' @examples
#' 
#' boxplot.aggr.sk(BspDaten$Plots$aggr.data,
#'                 BspDaten$Plots$aggr.labels,
#'                 BspDaten$Plots$aggr.skala)
#' 
#' @export boxplot.aggr.sk

boxplot.aggr.sk <- function(x, # Daten
                            item_labels, # Labels/Text/Beschriftungen der Y-Achse
                            skala, # Skala der x-Achse
                            color = set.analysis.defaults$color.bars,
                            font_family = set.analysis.defaults$font.family)
{
  daten <- cbind(x)
  n_items <- ncol(daten)
  n_skala <- length(skala)
  
  opar <- par(no.readonly = TRUE)
  
  if(n_skala == 5) {
    par(mar = c(4, 21.5, 0.1, 2.1)) # mehr Platz für Hinweistext bei 5er Skala
    } else {
    par(mar = c(2.1, 21.5, 0.1, 2.1))
  }
 

  par(family = font_family,
          fg = "gray80",
        lend = "square", # Linienenden eckig
       ljoin = "mitre")  # Linienschnitt eckig
  
  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------
  
  plot(
    1,  # Dummy-Wert
    type = "n",  # Keine Daten zeichnen
    xlim = c(1, n_skala),  # x-Achse: Skalenwerte
    ylim = c(0.5, length(daten) + 0.5),  # y-Achse: Anzahl der Gruppen + Puffer
    xlab = "", ylab = "",  # Keine Achsenbeschriftung
    xaxt = "n", yaxt = "n",  # Keine Achsen zeichnen
    bty = "n"  # Kein Rahmen
  )
  
  
  abline(v = c(1:n_skala), col="gray80") #vertikale Hilfslinien bei jedem Skalenpunkt
  
  boxplot(daten,
          add = TRUE,
          xaxt = "n",
          yaxt = "n",
          xlim = c(1, n_skala), horizontal = TRUE,
        boxwex = 0.8, col = color, border = "black",
          pars = list(outcol = color, outpch = 20))
  
  mtext(item_labels,
        side = 2,
        line = 1,
          at = 1:n_items,
         las = 2,
         col = "gray30", # Farbe der Beschriftung
         cex = 1)        # Schriftgröße
  
  mtext(skala,
        side = 1,
        line = 1,
          at = 1:n_skala,
         las = 1,
         col = "gray30",
         cex = 1,
        font = 2)
  
  if (n_skala == 5) {
    mtext("Hinweis: andere Skalenlogik (im Vergleich zu den 6er-Skalen)",
          side = 1, line = 3, col = "gray40", font = 3)}
  
  par(opar)
}