#' Abbildung des Rücklaufs
#'
#' @param x (aggregierte) Daten für den Boxplot
#'
#' @returns Boxplot der Rücklaufs
#'
#' @examples boxplot_rueck(BspDaten$Plots$rueck)
#' 
#' @export boxplot_rueck

boxplot_rueck <- function(x) # Daten
{
  # Aktuelle Grafikparameter speichern ------------------------------------
  
  opar <- par(no.readonly = TRUE)
  
  # Grafikparameter für den Plot einstellen -------------------------------

  .common_par(mar = c(2.1, 9, 0.1, 2.5))
  
  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------
  
  .empty_plot(xlim = c(0, 120))

  # Hilfslinien -----------------------------------------------------------
  
  abline(v = c(0, 20, 40, 60, 80, 100), col = "gray70")
  
  # Eigentlichen Boxplot zeichnen -----------------------------------------
  
  .costum_boxplot(x, boxwex = 0.8, ylim = c(0, 120))
  
  # Achsenbeschriftungen hinzufügen --------------------------------------
  
  .text_left("Rücklauf in Prozent", at = 1)
  
  .text_bottom(c("0","20","40","60","80","100"),
          at = c( 0 , 20,  40,  60,  80,  100 ))
  
  # Vorher gesicherte Grafikparameter wiederherstellen --------------------
  
  par(opar)
}