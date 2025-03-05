#' Barplot zur Abbildung von von SC/MC-Fragen
#'
#' @param x Daten (data.frame mit Fragetexten, Häufigkeit und Prozent)
#' @param color Farbe Balken
#' @param xlab Beschriftung x-Achse
#'
#' @returns Barplot
#' @export


# Horizontaler Barplot für Abbildungen von SC/MC-Fragen
barplot.sc.mc <- function(x, # Daten (data.frame mit Fragetexten, Häufigkeit und Prozent)
                          color = set.analysis.defaults$color.bars, # Farbe der Balken
                          xlab = "") # Beschriftung x-Achse
{ 
  
  cody <- x
  
  #wenn alles Nullen(Daten gleich Nullvektor), dann schreibe "*Grafik wurde wegen ...
  if(all(cody[, 2] == 0)){ cat("*Grafik wurde wegen fehlender Daten nicht erstellt.*  \n  \n")}
  else{
    cody <- cody[cody$freq > 0, ]
    cody$label <- factor(cody$label, levels = rev(cody$label)) #Reihenfolge der Label festlegen
    
    ggplot2::ggplot(data=cody, ggplot2::aes(x=label, y=freq)) + #auf x-Achse wird Kategorie/Label dargestellt, auf y-Achse Haeufigkeiten
      #geom_hline(yintercept = v, color = "grey70")+
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white', colour = 'gray70'), #Hintergrund wird auf wei? gestellt
            panel.grid.major.x = ggplot2::element_line(color = "grey70")) + #Hilfslinien 
      ggplot2::theme(text=ggplot2::element_text(family = set.analysis.defaults$font.family)) +
      ggplot2::scale_color_manual(values = "grey50") +
      ggplot2::geom_bar(stat="identity", #Balkendiagramm
               fill = color, #Farbe
               width=0.8)+ #Breite der Balken
      ggplot2::geom_col(fill = color, color = "black", size = 0.3) +
      ggplot2::geom_text(ggplot2::aes(label = paste(perc, "%", sep = "")), #Prozentzahl über die Balken schreiben
                color ="grey30",#Farbe 
                size = 3.5, #Schriftgröße
                family = set.analysis.defaults$font.family,
                hjust = -0.5)+ #horizontale Verschiebung des Textes
      ggplot2::ylim(c(0, DescTools::RoundTo((max(x$freq)*1.15), multiple=DescTools::RoundTo(max(x$freq) * 0.3, multiple=5, ceiling), ceiling))) +#automatische Einstellung des Limites der y Achse
      ggplot2::theme(title = ggplot2::element_text(color = "blue"),
            axis.title.y = ggplot2::element_blank(),  #y-Achsen Titel, element_blank() meint kein Text
            axis.title.x = ggplot2::element_text(color = "grey30", size = 14, face = "bold"),  #x Achsen Titel
            axis.text.y = ggplot2::element_text(color = "grey40", #Beschriftung y Achse, element_text() meint schreibe einen Text, col = Farbe
                                       size = 12, face = "bold"), #Schriftgroesse
            axis.text.x = ggplot2::element_text(color = "black", size = 11), #Beschriftung x Achse
            axis.ticks.y = ggplot2::element_blank(), #Die kleinen Striche zur Unterteilung an der y Achse
            axis.ticks.x = ggplot2::element_line(color = "grey48"))+ #kleinen Striche zur Unterteilung an der x Achse
      ggplot2::labs(y=xlab)+
      ggplot2::coord_flip()+ #x- und y-Achse vertauschen
      ggplot2::theme(text=ggplot2::element_text(family = set.analysis.defaults$font.family)) 
  }
}