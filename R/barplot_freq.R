#' Barplot zur Abbildung von Häufigkeiten (kann auch für Balkendiagramme bei ordinalen Skalennivaus genutzt werden)
#'
#' @param x Daten
#' @param main Titel der Abbildung
#'
#' @returns Barplot
#'
#' @examples
#' 
#' # Beispiel für Verwendung in merge_num ---------------------------------
#' 
#' barplot_freq(BspDaten$Plots$num, xlab = "Durchschnittsnote für Hochschulzugangsberechtigung")
#'
#' # Beispiel für Verwendung in merge_fsem ---------------------------------
#'
#' barplot_freq(BspDaten$dataLVE$FachSemN, xlab = "Fachsemester alle")

#' @export barplot_freq

barplot_freq <- function(x, # Daten
                         xlab = "") # Titel der Abbildung
{
  
  opar <- par(no.readonly = TRUE)
  .common_par(mar = c(4, 3, 0.5, 0.5))
  
  x_max <- max(table(x))
  
  .empty_plot(ylim = c(0, 7/6 * x_max),
              xlim = c(0.2, nlevels(x) * 1.2))
  
  abline(h = pretty(c(0, x_max), n = 4), col = "gray70")
  
  .costum_barplot(table(x))
  
  .text_left(pretty(c(0, x_max), n = 4), at = pretty(c(0, x_max), n = 4))
  .text_bottom(levels(x), at = seq(0.7, -0.5 + 1.2 * nlevels(x), by = 1.2))
  .text_bottom_2(xlab, line = 2.5)
  
  par(opar)
}
