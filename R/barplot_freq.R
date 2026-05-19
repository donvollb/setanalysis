#' Barplot zur Abbildung von Häufigkeiten (kann auch für Balkendiagramme bei ordinalen Skalennivaus genutzt werden)
#'
#' @param x Daten
#' @param xlab Achsenbeschriftung
#' 
#'
#' @returns Barplot
#'
#' @examples
#' 
#' # Beispiel für Verwendung in merge_num ---------------------------------
#' 
#' barplot_freq(BspDaten$Plots$num, xlab = "Durchschnittsnote für Hochschulzugangsberechtigung")
#'
#' @export barplot_freq

barplot_freq <- function(x, # Daten
                         xlab = "") # Achsenbeschriftung
{
  
  # Maximale Antworthäufigkeit ermitteln (für y-Achsenskalierung) ---------

  x_max <- max(table(x))
  
  # Bisherige Grafikparameter speichern -----------------------------------

  opar <- par(no.readonly = TRUE)
  
  # Grafikparameter für den Plot einstellen -------------------------------

  .common_par(mar = c(4, 3, 0.5, 0.5))
  
  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------

  .empty_plot(ylim = c(0, 7/6 * x_max),
              xlim = c(0.2, nlevels(x) * 1.2))
  
  # Hilflinien zeichnen ---------------------------------------------------
  
  abline(h = pretty(c(0, x_max), n = 4), col = "gray70")

  # Eigentlichen Barplot zeichnen -----------------------------------------
  
  .costum_barplot(table(x))

  # Achsenbeschriftungen hinzufügen ---------------------------------------

  .text_left(pretty(c(0, x_max), n = 4), at = pretty(c(0, x_max), n = 4))
  .text_bottom(levels(x), at = seq(0.7, -0.5 + 1.2 * nlevels(x), by = 1.2))
  .text_bottom_2(xlab, line = 2.5)
  
  # Vorher gesicherte Grafikparameter wiederherstellen --------------------

  par(opar)
}
