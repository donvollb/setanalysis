#' Boxplot für Rücklaufsabbildung (LVE)
#'
#' @param x Objekt mit Teilnehmendenzahlen
#' @param kennung Kennungen
#'
#' @returns Boxplot der Rückläufe
#' @export

boxplot.ruecklauf <- function(x, # Objekt mit Teilnehmendenzahlen
                              kennung) #  Kennungen
{
  z <- data.frame(kennung, x) # Erstelle einen Datensatz aus beiden
  z.uni <- z[!duplicated(z$kennung),] # Nehme nur eine Zeile pro Kennung
  tb.kennung <- data.frame(table(kennung)) # Zähle, wie oft jede Kennung vorkommt
  all <- merge(z.uni, tb.kennung, by = "kennung") # Füge das mit dem Datensatz z zusammen

  # Datensatz z enthält an dieser Stelle:
  # Eine Spalte mit jeder LV-Kennung
  # Eine Spalte mit der zugelassenen Teilnehmendenzahl pro Kennung
  # Eine Spalte mit der bisherigen Teilnehmendenzahl

  x.new <- as.numeric(all$Freq)/as.numeric(all$x)*100 # Teile die bisherigende Tn-Zahl durch die Zugelassenen (mal hundert)
  x <- x.new



  # Ausgabe der Flextable
  #  print(table.stat.single(x, col1.name = "N\\textsubscript{courses}")) # kein print() bei Flextable
  #  table.stat.single(x, col1.name = "N_courses") # klappt nicht
  #  knitr::knit_print(table.stat.single(x, col1.name = "N_courses")) # klappt nicht
  #  flextable_to_rmd(table.stat.single(x, col1.name = "N_courses")) # für if und for bei Quarto erforderliche Funktion
  flextable::flextable_to_rmd( # Ausgabe der Flextable
    table.stat.single(x, col1.name = "N") %>%
      flextable::append_chunks(flextable::as_sub("courses"), i=1, j=1, part="header") # courses tiefergestellt
  )

  cat("  \n  \n")

  #Grafikparameter spezifizieren
  opar <- par(no.readonly = TRUE)
  par(mar=c(2.1, 5 , 0.1, 2.5)) # 2. Zahl anpassen, wenn Aenderung der Breite gewuenscht (b l t r )
  par(fg="gray80")
  #Boxplot zeichnen
  boxplot(x, width=NULL, outline=TRUE,
          boxwex=0.5,   #boxwex stellt die Größe der Box vom boxplot ein
          horizontal=TRUE, #Orientierung: horizontal
          col = set.analysis.defaults$color.bars, #Füllfarbe
          ylim=c(0,120), #Länge der y Achse
          xaxt="n", #keine Beschriftung der x Achse
          border="black", #Farbe der Außenlinie
          pars = list(outcol = set.analysis.defaults$color.bars, outpch=20))#Spezifizierung für Ausreißer. Farbe & Art/Charakter
  abline(v=c(0,20,40,60,80,100)) #vertikale Hilfslinien
  #Boxplot wird über Hilfslinie gezeichnet (Befehl von oben wiederholt)
  boxplot(x, width=NULL, outline=TRUE, boxwex=0.5,
          horizontal=TRUE, col = set.analysis.defaults$color.bars, ylim = c(0,120), xaxt="n",
          border="black", pars = list(outcol = set.analysis.defaults$color.bars, outpch=20), add = TRUE)
  mtext(c("Rücklauf \nin Prozent"), #Text
        side=2, #on which side of the plot (1=bottom, 2=left, 3=top, 4=right).
        line=1, #Linienart (durchgezogene Linie)
        at=1, #give location of each string in user coordinates
        las=1, #Schriftrotierung
        # las
        # numeric in {0,1,2,3}; the style of axis labels.
        # 0: always parallel to the axis [default],
        # 1: always horizontal,
        # 2: always perpendicular to the axis,
        # 3: always vertical.
        col="gray50", #Farbe der Schrift
        cex=0.8,
        font = 2) #cex = Schriftgroesse
  mtext(c("0", "20", "40", "60", "80", "100"), side=1, line=1, at=c(0, 20, 40, 60, 80, 100), las=1, col="gray50", cex=0.8)
  axis(side=1, at=c(0, 20, 40, 60, 80, 100), labels=FALSE)

  par(opar)
}
