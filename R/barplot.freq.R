#' Barplot zur Abbildung von Häufigkeiten (kann auch für Balkendiagramme bei ordinalen Skalennivaus genutzt werden)
#'
#' @param x Daten
#' @param color Farbe Balken
#' @param xlab Beschriftung x-Achse
#' @param main Titel der Abbildung
#' @param cutoff Cutoff,z.B. werden bei 12alle Werte über 12 als "12+" abgebildet
#'
#' @returns Barplot
#' @export

barplot.freq <- function(x, # Daten
                         color = color.bars, # Farbe Balken
                         xlab = "", # Beschriftung x-Achse
                         main = "", # Titel der Abbildung
                         cutoff = FALSE) # cutoff, bei z.B. 12 werden alle Werte über 12 als "12+" abgebildet
{

  if(cutoff != FALSE) {
    xax <- ifelse(max(x, na.rm = TRUE) == cutoff, "n", "t")
    labs <- as.character(as.data.frame(table(x))[, 1])
    labs[length(labs)] <- paste0(cutoff, "+")
  } else {
    xax <- "t"
  }

  bp <- plot(descr::freq(x, plot = FALSE),
             col = color.bars,
             family = font.family,
             ylim = c(0, 5/4*max(table(x))),
             xlab = xlab,
             main = main,
             ylab = "Häufigkeit",
             xaxt = xax,
             border = "gray50",
             cex.lab = 1,
             font.lab = 2,
             col.lab = "gray30")

  if(cutoff != FALSE){
    if (max(x, na.rm = TRUE) == cutoff){ axis(1,
                                              at = bp[, 1],
                                              labels = labs, tick = FALSE,
                                              family = font.family)}}
}
