#' Barplot zur Abbildung von von SC/MC-Fragen
#'
#' @param x Daten (data.frame mit Fragetexten, Häufigkeit und Prozent)
#' @param color Farbe Balken
#' @param xlab Beschriftung x-Achse
#'
#' @returns Barplot
#' 
#' @examples
#' 
#' # Beispiel für Verwendung in merge_sc ----------------------------------
#' barplot_scmc(BspDaten$Plots$sc, xlab = "Häufigkeit")
#' 
#' # Beispiel für Verwending in merge_mc ----------------------------------
#' barplot_scmc(BspDaten$Plots$mc, xlab = "Häufigkeit")
#' 
#' @export barplot_scmc


# Horizontaler Barplot für Abbildungen von SC/MC-Fragen
barplot_scmc <- function(x, # Daten (data.frame mit Fragetexten, Häufigkeit und Prozent)
                         xlab = "") # Beschriftung x-Achse
{ 
  
  # wenn alles Nullen (Daten gleich Nullvektor),
  # dann Funktion abbrechen und Nachricht schreiben
  
  if(all(x[, 2] == 0)){ cat("*Grafik wurde wegen fehlender Daten nicht erstellt.*  \n  \n")
    return(invisible()) 
  }
  
  opar <- par(no.readonly = TRUE)

  # Maximale Anzahl Zeichen in den Labels ermitteln, ----------------------
  # um linken Rand entsprechend anzupassen --------------------------------
  maxAnzahlZeichen <- max(nchar(unlist(strsplit(x$label, "\n"))))
  
  .common_par(mar = c(2, 4 + maxAnzahlZeichen * 0.35, 0.5, 0.5)) 


  .empty_plot(ylim = c(0.05 * nrow(x), 0.2 + nrow(x) * 1.15),
              xlim = c(0, max(x$freq) * 7/6))
  
  # Hilflslinien -----------------------------------------------------------
  
  abline(v = pretty(c(0, max(x$freq)), n = 4), col = "gray70")
  
  # Eigentlicher Plot -----------------------------------------------------
  
  .costum_barplot(rev(x$freq), horiz = TRUE)
  
  # Achsenbeschriftungen --------------------------------------------------
  
  .text_left(rev(x$label), at = seq(0.7, -0.5 + 1.2 * nrow(x), by = 1.2))
  .text_bottom(pretty(c(0, max(x$freq)), n = 4), at = pretty(c(0, max(x$freq)), n = 4))
  
  # Prozentzahlen rechts neben die Balken schrieben -----------------------
  
  text(labels = paste(sprintf("%.1f", rev(x$perc)), "%"),
            x = rev(x$freq) + max(x$freq) * 0.03,
            y = seq(0.7, -0.5 + 1.2 * nrow(x), by = 1.2),
          adj = 0)
}