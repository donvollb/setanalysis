#' Barplot-Boxplot Hypbrid für die Darstellung von ordinalskalierten Variablen
#' (analog zu alten EvaSys-Skalen)
#'
#' @description
#' Die optimale Chunk-Einstellung hierfür ist: fig.width = 6, fig.height = 1.4
#' 
#' @param x Daten
#' @param tmin Beschriftung links
#' @param tmax Beschriftung rechts
#' @param number Skala (6 für Sechserskala etc.)
#'
#' @examples
#' barplot_sk(BspDaten$dataSHOWUP$info_ausr_studgang,
#'            tmin = "stimme gar nicht zu", tmax = "stimme voll zu")
#' 
#'
#' @returns Barplot-Boxplot-Hybrid
#' @export

barplot_sk <- function(x, # Daten
                       tmin, # Beschriftung links
                       tmax, # Beschriftung rechts
                       number = 6) # Skala (6 für Sechserskala etc.)
{
  x[!(x %in% c(1:number))] <- NA
  x <- x[!is.na(x)]
  
  tmin <- sapply(tmin, \(x) paste(strwrap(x, width = 15), collapse = "\n"))
  tmax <- sapply(tmax, \(x) paste(strwrap(x, width = 15), collapse = "\n"))

  

  
  xtab <- table(c(x, 1:number)) #- 1 # damit alle angezeigt werden
  
  # Bisherige Grafikparameter speichern -----------------------------------

  opar <- par(no.readonly = TRUE)

  # Grafikparameter für den Plot einstellen -------------------------------

  .common_par(mar = c(2.5, 6.5, 2.5, 6.5))

  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------

  .empty_plot(xlim = c(0.2, number * 1.2), ylim = c(0, sum(table(x))))
  
  # Hilfslinien -----------------------------------------------------------

  abline(v = seq(0.7, -0.5 + 1.2 * number, by = 1.2), col = "grey80")
  
  # Eigentlichen Barplot zeichnen -----------------------------------------

  .costum_barplot(xtab)
  
  # X-Achsenbeschriftungen und Prozentzahlen hinzufügen -------------------
 
  .text_bottom(1:number, at = seq(0.7, -0.5 + 1.2 * number, by = 1.2))  
  .text_top(paste(sprintf("%.1f", 100 * prop.table(xtab)), "%"),
            at = seq(0.7, -0.5 + 1.2 * number, by = 1.2))

  # Beschriftungen der Pole hinzufügen ------------------------------------
  
  .text_left(tmin)
  .text_right(tmax)
  
  # Kleinen Boxplot darüber hinzufügen ------------------------------------

  par(new = TRUE, bty = "n")

  boxplot(c(mean(x) - sd(x), rep(mean(x), 3), mean(x) + sd(x)),
          yaxt = "n", xaxt = "n",
          medcol = "black", # oder: medcol = setanalysis_defaults$color.bars,
          horizontal = TRUE, range = 0, ylim = c(0.6, number + 0.4), medlwd = 4,
          boxlwd = 0.01, xlim = c(0.3, 1.3), whisklty = 1, outline = FALSE)
          
  par(opar)
  cat("  \n  \n")
}
