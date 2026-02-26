#' Boxplots für Skalenfragen auf aggregiertem Niveau: Funktioniert, sollte aber überarbeitet werden
#'
#' @param x Daten
#' @param item_labels Labels/Text/Beschriftungen der Y-Achse
#' @param skala Skala der x-Achse
#'
#' @returns Boxplot
#' 
#' @examples
#' 
#' boxplot_aggr_sk(BspDaten$Plots$aggr.data,
#'                 BspDaten$Plots$aggr.labels,
#'                 BspDaten$Plots$aggr.skala)
#' 
#' @export boxplot_aggr_sk

boxplot_aggr_sk <- function(x,           # Daten
                            item_labels, # Labels/Text/Beschriftungen der Y-Achse
                            skala)       # Skala der x-Achse
{
  daten <- cbind(x)
  n_items <- ncol(daten)
  n_skala <- length(skala)
  
  # Bisherige Grafikparameter speichern -----------------------------------
  
  opar <- par(no.readonly = TRUE)
  
  # Grafikparameter für den Plot einstellen -------------------------------
  
  .common_par()
  
  if(n_skala == 5) {
    par(mar = c(4, 21.5, 0.1, 2.1)) # mehr Platz für Hinweistext bei 5er Skala
    } else {
    par(mar = c(2.1, 21.5, 0.1, 2.1))
  }

  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------
  
  .empty_plot(xlim = c(1, n_skala),
              ylim = c(0.5, n_items + 0.5))
  
  # Hilfslinien -----------------------------------------------------------
  
  abline(v = c(1:n_skala), col = "grey70")
  
  # Eigentlichen Boxplot zeichnen -----------------------------------------
  
  .costum_boxplot(x, boxwex = 0.8,
                  ylim = c(0.5, n_items + 0.5),
                  xlim = c(1, n_skala))
  
  # Achsenbeschriftungen hinzufügen --------------------------------------
  
  .text_left(item_labels, at = 1:n_items)
  
  .text_bottom(skala, at = 1:n_skala)
  
  if (n_skala == 5) {
    mtext("Hinweis: andere Skalenlogik (im Vergleich zu den 6er-Skalen)",
          side = 1, line = 3, col = "gray15", font = 3)}
  # Vorher gesicherte Grafikparameter wiederherstellen --------------------
  
  par(opar)
}