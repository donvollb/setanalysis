#' Boxplots für Skalenfragen auf aggregiertem Niveau: Funktioniert, sollte aber überarbeitet werden
#'
#' @param x daten
#' @param p labels/Text/Beschriftungen Y Achse
#' @param q Skala x Achse
#' @param d Anzahl von p
#' @param e Anzahl von q
#' @param color Farbe, Standard ist der Wert von color.bars
#'
#' @returns Boxplot
#' 
#' @export boxplot.aggr.sk

boxplot.aggr.sk <- function(x, p, q, d, e, color = set.analysis.defaults$color.bars)
{ #x = daten, p = labels/Text/Beschriftungen Y Achse, q = Skala x Achse, d = Anzahl von p, e = Anzahl von q
  daten <- cbind(x)
  opar <- par(no.readonly = TRUE)
  par(mar=c(2.1, 21.5, 0.1, 2.1))
  if(e == 5) {par(mar=c(4, 21.5, 0.1, 2.1))}
  par(family = set.analysis.defaults$font.family)
  boxplot(daten, xaxt="n", yaxt="n", #x- und y-Achsentext nicht anzeigen
          ylim=c(1,e), #Länge y Achse von 1 bis e
          horizontal=TRUE, #horizontale Ausrichtung
          boxwex=0.8, #stellt Größe der Box des Boxplots ein
          border = "white")
  abline(v=c(1:e), col="gray80")#vertikale Hilfslinien
  par(new=TRUE)
  par(fg="gray80")
  par(family = set.analysis.defaults$font.family)
  boxplot(daten, xaxt="n", yaxt="n", ylim=c(1,e), horizontal=TRUE,
          boxwex=0.8, col= color, border="black",
          pars=list(outcol= color, outpch=20))
  mtext(p,
        side=2, line=1, at=1:d, las=2,
        col="gray30", #Beschriftung Farbe
        cex = 1) #Beschriftung Schriftgröße
  mtext(q,
        side=1, line=1, at=1:e, las=1, col="gray30", cex=1, font = 2)
  if (e == 5) {mtext("Hinweis: andere Skalenlogik (im Vergleich zu den 6er-Skalen)", side=1, line=3, col="gray40", font = 3)}
  par(opar)
}
