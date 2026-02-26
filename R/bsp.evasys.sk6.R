#' Beispiel einer Sechserskala
#'
#' @param x Daten, bei "default" wird ein Beispieldatensatz genutzt 
#'
#' @returns Grafik
#' 
#' @examples bsp.evasys.sk6() |> markdown_in_viewer()
#' 
#' @export bsp.evasys.sk6

bsp.evasys.sk6 <- function(x = "default") # Daten, bei "default" wird ein Beispieldatensatz genutzt
{
  if(x[1] == "default") {
    x <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
           3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6)
  }
  
  number <- 6
  xtab <- table(c(x, 1:number))
  tmin <- "linker Pol"
  tmax <- "rechter Pol"

  subchunkify(c(

  # Bisherige Grafikparameter speichern -----------------------------------

  opar <- par(no.readonly = TRUE),

  # Grafikparameter für den Plot einstellen -------------------------------
  .common_par(mar = c(2.5, 6.5, 6.5, 6.5)),

  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------

  .empty_plot(xlim = c(0.2, number * 1.2), ylim = c(0, sum(table(x)))),
  
  # Hilfslinien -----------------------------------------------------------

  abline(v = seq(0.7, -0.5 + 1.2 * number, by = 1.2), col = "grey80"),
  
  # Eigentlichen Barplot zeichnen -----------------------------------------

  .costum_barplot(xtab),
  
  # X-Achsenbeschriftungen und Prozentzahlen hinzufügen -------------------
 
  .text_bottom(1:number, at = seq(0.7, -0.5 + 1.2 * number, by = 1.2)),
  .text_top(paste(sprintf("%.1f", 100 * prop.table(xtab)), "%"),
            at = seq(0.7, -0.5 + 1.2 * number, by = 1.2)),

  # Beschriftungen der Pole hinzufügen ------------------------------------
  
  .text_left(tmin),
  .text_right(tmax),
  
  # Kleinen Boxplot darüber hinzufügen ------------------------------------

  par(new = TRUE, bty = "n"),

  boxplot(c(mean(x) - sd(x), rep(mean(x), 3), mean(x) + sd(x)),
          yaxt = "n", xaxt = "n", medcol = "black",
          horizontal = TRUE, range = 0, ylim = c(0.6, number + 0.4), medlwd = 4,
          boxlwd = 0.01, xlim = c(0.3, 1.3), whisklty = 1, outline = FALSE),
  
  par(xpd = TRUE),
  
  # Erklärungen und Linien hinzufügen -------------------------------------

  text(x = 0.95, y = 2.30, col = "gray15", label = "Relative Häufigkeit der Antwort"),
  text(x = 3.35, y = 2.32, col = "gray15", label = "Mittelwert"),
  text(x = 4.66, y = 2.30, col = "gray15", label = "Standardabweichung"),
  segments(x0 = 0.95, y0 = 1.85, x1 = 0.95, y1 = 2.15, col = "gray15"),
  segments(x0 = 3.38, y0 = 1.26, x1 = 3.38, y1 = 2.15, col = "gray15"),
  segments(x0 = 4.64, y0 = 1.18, x1 = 4.64, y1 = 2.15, col = "gray15"),
  par(opar)), fig_width = 9, fig_height = 2.8, hide = TRUE)
}