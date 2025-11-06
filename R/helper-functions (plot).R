# potenzielle Hilfsfunktionen

#' @noRd
#' @export

.common_par <- function(...) {
  
  par(fg = "gray15", # Farbe Rand
     las = 1, # Schriftrotierung (keine Rotierung)
    lend = "square", # Linienenden eckig
   ljoin = "mitre", # Linienschnitt eckig
  family = setanalysis_defaults$font.family,
     ...) # Schriftart
}

#' @noRd
#' @export

.empty_plot <- function(...) {
  
  plot(1,  # Dummy-Wert
        bty = "n",  # Kein Rahmen
       type = "n",  # Keine Daten zeichnen
       xlab = "",
       ylab = "",   # Keine Achsenbeschriftungen
       xaxt = "n",
       yaxt = "n",  # Keine Achsen zeichnen
       ...) # z. B. xlim = c(1, 6) für Sechserskala
}

#' @noRd
#' @export

.costum_boxplot <- function(...) {

  boxplot(col = setanalysis_defaults$color.bars,
          add = TRUE,
         pars = list(outcol = setanalysis_defaults$color.bars, outpch = 20),
         xaxt = "n",
         yaxt = "n",
        width = NULL,
       border = "black",
      outline = TRUE,
   horizontal = TRUE,
         ...)
}
 
#' @noRd
#' @export

.text_left <- function(...) {
  
  mtext(side = 2,        # links plazieren
        line = 1,        # Abstand zum Plot
         las = 1,        # Textausrichtung (rechtsbündig)
         col = "gray15", # Textfarbe
         ...)            # z. B. Position und Text 
}

#' @noRd
#' @export

.text_bottom <- function(...) {
  
  mtext(side = 1,        # unten plazieren
        line = 1,        # Abstand zum Plot
        font = 2,        # fette Schrift
         col = "gray15", # Textfarbe
         ...)            # z. B. Position und Text
}