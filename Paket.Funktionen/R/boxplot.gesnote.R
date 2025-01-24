# Abbildung der Gesamtnote
boxplot.gesnote <- function(x) # Daten
{
  #Grafikparameter bestimmen
  opar <- par(no.readonly = TRUE)
  par( mar=c(2.1, 7 , 0.1, 2.1)) # 2. Zahl anpassen, wenn Aenderung der Breite gewünscht
  par(fg="gray80")
  par(family = font.family)
  #Boxplot zeichnen
  boxplot(x, width=NULL, outline=TRUE,
          boxwex=0.5,   #boxwex stellt die Groesse der Box vom boxplot ein
          horizontal=TRUE, #Orientierung: horizontal
          col= color.bars,  #Fuellfarbe
          ylim=c(1,6), #Laenge der y Achse
          xaxt="n", #keine Beschriftung der x Achse
          border="black", #Farbe der Aussenlinie
          pars=list(outcol= color.bars, outpch=20))#Spezifizierung fuer Ausreisser. "outpch=NA" > ohne Ausreiser
  abline(v=c(1,2,3,4,5,6), col="gray80") #Hilfslinien
  boxplot(x, width=NULL, outline=TRUE, boxwex=0.5,
          horizontal=TRUE, col= color.bars, ylim=c(1,6), xaxt="n", border="black",
          pars=list(outcol= color.bars, outpch=20), add = TRUE)

  mtext(c("Gesamtnote \nder LV"),
        side=2, # on which side of the plot (1=bottom, 2=left, 3=top, 4=right).
        line=1, #Linienart (durchgezogene Linie)
        at=1, #an x-Koordinate 1
        las=1, # Textausrichtung
        col="gray30",#Textfarbe
        cex = 1.1,
        font = 2)#Schriftgroesse
  mtext(c("sehr gut", "gut", "befriedigend", "ausreichend", "mangelhaft", "ungenügend"),
        side=1, line=1, at=c(1, 2, 3, 4, 5, 6), las=1, col="gray50", cex=1.1, font = 2)
  axis(side=1, at=c(1, 2, 3, 4, 5, 6), labels=FALSE)

  par(opar)

}
