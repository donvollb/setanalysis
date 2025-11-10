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
    
    .common_par(mar = c(4.8, 11, 4.1, 6)),
    
    # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) ---------------
    
    .empty_plot(xlim = c(1, 6)),

    # Hilfslinien ---------------------------------------------------------
    
    abline(v = 1:6, col = "gray70"),
    
    # Eigentlicher Plot ---------------------------------------------------
    
    .costum_boxplot(x, boxwex = 0.5, ylim = c(1, 6)),

    # Achsenbeschriftungen hinzufügen ------------------------------------
    
    .text_left("Beispiel-Boxplot", at = 1),
    .text_bottom(c("trifft gar nicht zu", "", "", "", "",
                   "trifft voll zu"), at = 1:6),
    par(xpd = TRUE),
    
    # Beschriftungslinien -------------------------------------------------
    
    segments(x0 = 1.7, y0 = 1.08, x1 = 1.7, y1 = 1.76, col = "grey15"),
    segments(x0 = 4.5, y0 = 1.26, x1 = 4.5, y1 = 1.76, col = "grey15"),
    segments(x0 = 5.7, y0 = 1.17, x1 = 5.7, y1 = 1.76, col = "grey15"),
    segments(x0 = 1.1, y0 = 0.12, x1 = 2.8, y1 =-0.20, col = "grey15"),
    segments(x0 = 5.9, y0 = 0.12, x1 = 4.2, y1 =-0.20, col = "grey15"),
    
    # Beschriftungstexte --------------------------------------------------
    
    text(x = 1.7, y = 1.9, col = "grey15", label = "Ausreißer"),
    text(x = 4.5, y = 1.9, col = "grey15", label = "Median"),
    text(x = 5.7, y = 1.9, col = "grey15", label = "Max"),
    text(x = 3.5, y =-0.2, col = "grey15", label = "6-stufige Skala")),
    
    fig_height = 2.7, # optimal: 2.7
    fig_width = 10,   # optimal: 10
    hide = TRUE)      # damit nicht Text dazu "ausgespuckt" wird

  cat("  \n  \n")

}
