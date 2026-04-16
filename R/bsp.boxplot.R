#' Beispiel-Boxplot mit Beschriftung
#'
#' @param x Daten, bei "default" wird ein Beispieldatensatz genutzt
#'
#' @returns Beispiel-Boxplot
#'
#' @examples bsp.boxplot() |> markdown_in_viewer()
#' 
#' @export bsp.boxplot

bsp.boxplot <- function(x = "default") # Daten, bei "default" wird ein Beispieldatensatz genutzt

{

  if(x[1] == "default") {
    x <- c(1.7, 3.5, 3.6, 3.7, 4.0, 4.1, 4.2, 4.2, 4.3, 4.3, 4.4, 4.5,
           4.5, 4.7, 4.8, 4.9, 5.0, 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7)
  }
  

  subchunkify(c(
    
    
    

    opar <- par(no.readonly = TRUE),
    

    # Grafikparameter für den Plot einstellen -----------------------------

    .common_par(mar = c(7, 8, 5, 2.1)),
    
    # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) ---------------
    
    .empty_plot(xlim = c(1, 6)),

    # Hilfslinien ---------------------------------------------------------
    
    abline(v = 1:6, col = "gray70"),
    
    # Eigentlicher Plot ---------------------------------------------------
    
    .costum_boxplot(x, boxwex = 0.8, ylim = c(1, 6)),

    # Achsenbeschriftungen hinzufügen ------------------------------------
    
    .text_left("Beispiel-Boxplot", at = 1),
    .text_bottom(c("trifft gar nicht zu", "", "", "", "",
                   "trifft voll zu"), at = 1:6),
    par(xpd = TRUE),
    
    # Beschriftungslinien -------------------------------------------------
    
    segments(x0 = 1.7, y0 = 1.08, x1 = 1.7, y1 = 1.66, col = "grey15"),
    segments(x0 = 4.5, y0 = 1.26, x1 = 4.5, y1 = 1.66, col = "grey15"),
    segments(x0 = 5.7, y0 = 1.17, x1 = 5.7, y1 = 1.66, col = "grey15"),
    segments(x0 = 1.1, y0 = 0.25, x1 = 2.8, y1 = 0.00, col = "grey15"),
    segments(x0 = 5.9, y0 = 0.25, x1 = 4.2, y1 = 0.00, col = "grey15"),
    
    # Beschriftungstexte --------------------------------------------------
    
    text(x = 1.7, y = 1.8, col = "grey15", label = "Ausreißer"),
    text(x = 4.5, y = 1.8, col = "grey15", label = "Median"),
    text(x = 5.7, y = 1.8, col = "grey15", label = "Max"),
    text(x = 3.5, y = 0.0, col = "grey15", label = "6-stufige Skala"),

    # Vorher gesicherte Grafikparameter wiederherstellen ------------------

    par(opar)),
   
    fig_height = 3.8,
    fig_width  = 9,
    hide = TRUE)

  cat("  \n  \n")

}
