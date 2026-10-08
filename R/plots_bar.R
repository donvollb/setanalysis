# Balkendiagramme --------------------------------------------------------

#' Barplot zur Abbildung von Häufigkeiten (kann auch für Balkendiagramme bei ordinalen Skalennivaus genutzt werden)
#'
#' @param x Daten
#' @param xlab Achsenbeschriftung
#'
#'
#' @returns Barplot
#'
#' @examples
#'
#' # Beispiel für Verwendung in merge_num ---------------------------------
#'
#' barplot_freq(BspDaten$Plots$num, xlab = "Durchschnittsnote für Hochschulzugangsberechtigung")
#'
#' @export barplot_freq

barplot_freq <- function(x, # Daten
                         xlab = "") # Achsenbeschriftung
{
  # Maximale Antworthäufigkeit ermitteln (für y-Achsenskalierung) ---------

  x_max <- max(table(x))

  # Bisherige Grafikparameter speichern -----------------------------------

  opar <- par(no.readonly = TRUE)

  # Grafikparameter für den Plot einstellen -------------------------------

  .common_par(mar = c(4, 3, 0.5, 0.5))

  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------

  .empty_plot(
    ylim = c(0, 7 / 6 * x_max),
    xlim = c(0.2, nlevels(x) * 1.2)
  )

  # Hilflinien zeichnen ---------------------------------------------------

  abline(h = pretty(c(0, x_max), n = 4), col = "gray70")

  # Eigentlichen Barplot zeichnen -----------------------------------------

  .custom_barplot(table(x))

  # Achsenbeschriftungen hinzufügen ---------------------------------------

  .text_left(pretty(c(0, x_max), n = 4), at = pretty(c(0, x_max), n = 4))
  .text_bottom(levels(x), at = seq(0.7, -0.5 + 1.2 * nlevels(x), by = 1.2))
  .text_bottom_2(xlab, line = 2.5)

  # Vorher gesicherte Grafikparameter wiederherstellen --------------------

  par(opar)
}

#' Barplot zur Abbildung von von SC/MC-Fragen
#'
#' @param x Daten (data.frame mit Fragetexten, Häufigkeit und Prozent)
#' @param xlab Beschriftung x-Achse
#'
#' @returns Barplot
#'
#' @examples
#'
#' # Beispiel für Verwendung in merge_sc ----------------------------------
#' barplot_scmc(BspDaten$Plots$sc, xlab = "Häufigkeit")
#'
#' # Beispiel für Verwending in merge_mc ----------------------------------
#' barplot_scmc(BspDaten$Plots$mc, xlab = "Häufigkeit")
#'
#' @export barplot_scmc


# Horizontaler Barplot für Abbildungen von SC/MC-Fragen
barplot_scmc <- function(x, # Daten (data.frame mit Fragetexten, Häufigkeit und Prozent)
                         xlab = "") # Beschriftung x-Achse
{
  # wenn alles Nullen (Daten gleich Nullvektor),
  # dann Funktion abbrechen und Nachricht schreiben

  if (all(x[, 2] == 0)) {
    cat("*Grafik wurde wegen fehlender Daten nicht erstellt.*  \n  \n")
    return(invisible())
  }

  # Maximale Anzahl Zeichen in den Labels ermitteln, ----------------------
  # um linken Rand entsprechend anzupassen --------------------------------

  maxAnzahlZeichen <- max(nchar(unlist(strsplit(x$label, "\n"))))

  # Bisherige Grafikparameter speichern -----------------------------------

  opar <- par(no.readonly = TRUE)

  # Grafikparameter für den Plot einstellen -------------------------------

  .common_par(mar = c(2, 1 + maxAnzahlZeichen * 0.45, 0.5, 0.5))

  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------

  .empty_plot(
    ylim = c(0.05 * nrow(x), 0.2 + nrow(x) * 1.15),
    xlim = c(0, max(x$freq) * 7 / 6)
  )

  # Hilflslinien -----------------------------------------------------------

  abline(v = pretty(c(0, max(x$freq)), n = 4), col = "gray70")

  # Eigentlicher Plot -----------------------------------------------------

  .custom_barplot(rev(x$freq), horiz = TRUE)

  # Achsenbeschriftungen hinzufügen ---------------------------------------

  .text_left(rev(x$label), at = seq(0.7, -0.5 + 1.2 * nrow(x), by = 1.2))
  .text_bottom(pretty(c(0, max(x$freq)), n = 4), at = pretty(c(0, max(x$freq)), n = 4))

  # Prozentzahlen rechts neben die Balken schrieben -----------------------

  text(
    labels = paste(sprintf("%.1f", rev(x$perc)), "%"),
    x = rev(x$freq) + max(x$freq) * 0.03,
    y = seq(0.7, -0.5 + 1.2 * nrow(x), by = 1.2),
    adj = 0
  )

  # Vorher gesicherte Grafikparameter wiederherstellen --------------------

  par(opar)
}

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
#'   tmin = "stimme gar nicht zu", tmax = "stimme voll zu"
#' )
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


  xtab <- table(c(x, 1:number)) - 1 # damit alle angezeigt werden

  # Bisherige Grafikparameter speichern -----------------------------------

  opar <- par(no.readonly = TRUE)

  # Grafikparameter für den Plot einstellen -------------------------------

  .common_par(mar = c(2.5, 6.5, 2.5, 6.5))

  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------

  .empty_plot(xlim = c(0.2, number * 1.2), ylim = c(0, sum(table(x))))

  # Hilfslinien -----------------------------------------------------------

  abline(v = seq(0.7, -0.5 + 1.2 * number, by = 1.2), col = "grey70")

  # Eigentlichen Barplot zeichnen -----------------------------------------

  .custom_barplot(xtab)

  # X-Achsenbeschriftungen und Prozentzahlen hinzufügen -------------------

  .text_bottom(1:number, at = seq(0.7, -0.5 + 1.2 * number, by = 1.2))
  .text_top(paste(sprintf("%.1f", 100 * prop.table(xtab)), "%"),
    at = seq(0.7, -0.5 + 1.2 * number, by = 1.2)
  )

  # Beschriftungen der Pole hinzufügen ------------------------------------

  .text_left(tmin)
  .text_right(tmax)

  # Kleinen Boxplot darüber hinzufügen ------------------------------------

  par(new = TRUE, bty = "n")

  boxplot(c(mean(x) - sd(x), rep(mean(x), 3), mean(x) + sd(x)),
    yaxt = "n", xaxt = "n",
    medcol = "black", # oder: medcol = setanalysis_defaults$color.bars,
    horizontal = TRUE, range = 0, ylim = c(0.6, number + 0.4), medlwd = 4,
    boxlwd = 0.01, xlim = c(0.3, 1.3), whisklty = 1, outline = FALSE
  )

  # Vorher gesicherte Grafikparameter wiederherstellen --------------------

  par(opar)
}
