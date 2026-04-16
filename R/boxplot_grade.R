#' Abbildung der Gesamtnote
#'
#' @param x (aggregierte) Daten für den Boxplot
#'
#' @returns Boxplot der Gesamtnote
#'
#' @examples boxplot_grade(BspDaten$Plots$grade)
#' 
#' @export boxplot_grade

boxplot_grade <- function(x) # Daten
{
  # Aktuelle Grafikparameter speichern ------------------------------------
  
  opar <- par(no.readonly = TRUE)
  
  # Grafikparameter für den Plot einstellen -------------------------------
  
  .common_par(mar = c(2.1, 10.1, 0.1, 2.1))
  
  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------
  
  .empty_plot(xlim = c(1, 6))
  
  # Hilfslinien -----------------------------------------------------------
  
  abline(v = c(1, 2, 3, 4, 5, 6), col = "gray70")
  
  # Eigentlichen Boxplot zeichnen -----------------------------------------
  
  .costum_boxplot(x, boxwex = 0.8, ylim = c(1,6))
  
  # Achsenbeschriftungen hinzufügen --------------------------------------
  
  .text_left("Gesamtnote der LV", at = 1)
  .text_bottom(at = c(1, 2, 3, 4, 5, 6),
               c("sehr gut", "gut", "befriedigend",
                 "ausreichend", "mangelhaft", "ungenügend"))
  
  # Vorher gesicherte Grafikparameter wiederherstellen --------------------

  par(opar)
}