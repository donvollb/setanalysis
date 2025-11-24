# potenzielle Hilfsfunktionen

#' @noRd
#' @export

.common_par <- function(...) {
  
  par(fg = "gray15", # Farbe Rand
      las = 1,        # Schriftrotierung (keine Rotierung)
      lend = "square", # Linienenden eckig
      ljoin = "mitre",  # Linienschnitt eckig
      family = setanalysis_defaults$font.family, # Schriftart
      ...)            # sonstige Parameter, z. B. mar
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
          pars = list(outcol = setanalysis_defaults$color.bars, outpch = 20), # Ausreißerpunkte
          xaxt = "n",
          yaxt = "n",
          width = NULL,
          border = "black",
          outline = TRUE,
          horizontal = TRUE,
          ...)
  
  box(lwd = 1.5) # Etwas dicker Rand des Plots
  
}

.costum_barplot <- function(...) {
  
  barplot(col = setanalysis_defaults$color.bars,
          add = TRUE,
          border = "black",
          xaxt = "n",
          yaxt = "n",
          ...)
  
  box(lwd = 1.5, col = "black") # Etwas dicker Rand des Plots
  
}


#' @noRd
#' @export

.text_left <- function(...) {
  
  mtext(side = 2,        # links platzieren
        line = 1,        # Abstand zum Plot
        las = 1,        # Textausrichtung (horizontal, nicht gedreht)
        col = "gray15", # Textfarbe
        ...)            # z. B. Position und Text 
}

#' @noRd
#' @export

.text_bottom <- function(...) {
  
  mtext(side = 1,        # unten platzieren
        line = 1,        # Abstand zum Plot
        font = 2,        # fette Schrift
        col = "gray15", # Textfarbe
        ...)            # z. B. Position und Text
}

.text_bottom_2 <- function(...) {
  
  mtext(side = 1,        # unten platzieren
        font = 1,        # fette Schrift
        col = "gray15", # Textfarbe
        ...)            # z. B. Position und Text
}
