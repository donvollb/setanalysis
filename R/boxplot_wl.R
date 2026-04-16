#' Boxplot mit Workloads der LVs: Funktioniert, sollte überarbeitet werden
#' 
#' @param x Daten
#' @param skala Skala x-Achse
#'
#' @examples boxplot_wl(x = BspDaten$Plots$WL)
#' 
#' @returns Boxplot
#' 
#' @export boxplot_wl

boxplot_wl <- function(x, # Daten
                             skala = c("0h", "1h", "2h", "3h", "4h", "5h", "6h", "7h",
                                       "8h", "9h", "10h", "11h", "12h", "mehr\nals 12h")) # Skala x-Achse

{
  
  # Berechnung der Anzahl der Veranstaltungen und Länge der Skala ---------
  
  n <- length(x)
  n_skala <- length(skala)
  
  # Bisherige Grafikparameter speichern -----------------------------------
  
  opar <- par(no.readonly = TRUE)

  # Grafikparameter für den Plot einstellen -------------------------------
  
  .common_par(mar = c(7, 2, 0.1, 2))

  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------
  
  .empty_plot(xlim = c(1, n_skala))
  
  # Einfügen von vertikalen Linien ----------------------------------------
  
  abline(v = c(1:n_skala), col = "gray70")
  
  # Plot über die Vertikalen Linien drüber plotten ------------------------

  .costum_boxplot(x, boxwex = 0.8, ylim = c(1:n_skala))

  # Beschriftungen einfügen -----------------------------------------------
  
  #.text_bottom(skala, at = 1:n_skala)
  mtext(skala, side = 1, line = 0.5, font = 2, at = 1:n_skala,
        las = 1, padj = 1, col = "gray15")
  
  # info1 <- "angegebener Workload der LV"
  # info2 <- paste0("[n = ", n, "]")
  
  # mtext(bquote(bold(.(info1)) ~ .(info2)), side = 1, line = 3, col = "gray15")
  .text_bottom_2(paste("angegebener Workload der LV [n =", n, "]"), line = 3)

  # Vorher gesicherte Grafikparameter wiederherstellen --------------------

  par(opar)
}