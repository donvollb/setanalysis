# Balkendiagramme --------------------------------------------------------

#' Balkendiagramm der Häufigkeiten
#'
#' @description
#' Zeichnet ein senkrechtes Balkendiagramm mit der Häufigkeit jeder Kategorie,
#' z. B. für Fachsemester oder in Klassen eingeteilte Noten. Wird von
#' [merge_num()] und [merge_fachsem()] verwendet.
#'
#' @param x Faktor (die Stufen bestimmen Reihenfolge und Beschriftung der
#'   Balken).
#' @param xlab Beschriftung der x-Achse.
#'
#' @returns Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.
#'
#' @family grafiken
#'
#' @examples
#' barplot_freq(BspDaten$Plots$fsem, xlab = "Fachsemester")
#'
#' @export
barplot_freq <- function(x, xlab = "") {
  # Maximale Antworthäufigkeit ermitteln (für y-Achsenskalierung) ---------

  x_max <- max(table(x))

  # Grafikparameter speichern; sie werden beim Verlassen der Funktion
  # (auch bei einem Fehler) wiederhergestellt

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

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
  .text_bottom(levels(x), at = .bar_centers(nlevels(x)))
  .text_bottom_2(xlab, line = 2.5)
}

#' Waagerechtes Balkendiagramm für Single- und Multiple-Choice-Fragen
#'
#' @description
#' Zeichnet für jede Antwortoption einen waagerechten Balken mit der
#' absoluten Häufigkeit; rechts daneben steht der Prozentwert. Wird von
#' [merge_sc()] und [merge_mc()] verwendet. Wurde keine Option gewählt, wird
#' statt der Abbildung ein Hinweis ausgegeben.
#'
#' @param x Data Frame mit den Spalten `label` (Antwortoption, ggf. mit
#'   Zeilenumbrüchen), `freq` (absolute Häufigkeit) und `perc` (Prozent), eine
#'   Zeile pro Antwortoption.
#' @param xlab Beschriftung der x-Achse.
#'
#' @returns Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.
#'
#' @family grafiken
#'
#' @examples
#' barplot_scmc(BspDaten$Plots$mc, xlab = "Häufigkeit")
#'
#' @export
barplot_scmc <- function(x, xlab = "") {
  # wenn alles Nullen (Daten gleich Nullvektor),
  # dann Funktion abbrechen und Nachricht schreiben

  if (all(x[, 2] == 0)) {
    cat("*Grafik wurde wegen fehlender Daten nicht erstellt.*  \n  \n")
    return(invisible())
  }

  # Maximale Anzahl Zeichen in den Labels ermitteln, ----------------------
  # um linken Rand entsprechend anzupassen --------------------------------

  max_chars <- max(nchar(unlist(strsplit(x$label, "\n"))))

  # Grafikparameter speichern; sie werden beim Verlassen der Funktion
  # (auch bei einem Fehler) wiederhergestellt

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  # Grafikparameter für den Plot einstellen -------------------------------

  .common_par(mar = c(2, 1 + max_chars * 0.45, 0.5, 0.5))

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

  .text_left(rev(x$label), at = .bar_centers(nrow(x)))
  .text_bottom(pretty(c(0, max(x$freq)), n = 4), at = pretty(c(0, max(x$freq)), n = 4))

  # Prozentzahlen rechts neben die Balken schrieben -----------------------

  text(
    labels = paste(sprintf("%.1f", rev(x$perc)), "%"),
    x = rev(x$freq) + max(x$freq) * 0.03,
    y = .bar_centers(nrow(x)),
    adj = 0
  )
}

#' Verteilung einer Skalenfrage mit Mittelwert und Standardabweichung
#'
#' @description
#' Zeichnet die Antworten einer Skalenfrage im Stil der evasys-Berichte: ein
#' Balken pro Skalenstufe mit dem Prozentwert darüber, die Beschriftung der
#' beiden Pole links und rechts sowie eine Markierung für Mittelwert und
#' Standardabweichung. Wird von [merge_sk()] verwendet (Abbildungsgröße dort:
#' 9 × 2 Zoll).
#'
#' @param x Numerischer Vektor mit den Antworten. Werte außerhalb von
#'   `1:number` (z. B. Ausweichoptionen) werden nicht berücksichtigt.
#' @param tmin,tmax Beschriftung des linken bzw. rechten Pols.
#' @param number Anzahl der Skalenstufen.
#'
#' @returns Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.
#'
#' @family grafiken
#' @seealso [bsp_evasys_sk6()] für eine beschriftete Legende zu dieser
#'   Abbildung.
#'
#' @examples
#' barplot_sk(BspDaten$dataSHOWUP$info_ausr_studgang,
#'   tmin = "stimme gar nicht zu", tmax = "stimme voll zu"
#' )
#'
#' @export
barplot_sk <- function(x, tmin, tmax, number = 6) {
  x[!(x %in% c(1:number))] <- NA
  x <- x[!is.na(x)]

  tmin <- .wrap_labels(tmin, width = 15)
  tmax <- .wrap_labels(tmax, width = 15)


  xtab <- table(c(x, 1:number)) - 1 # damit alle angezeigt werden

  # Grafikparameter speichern; sie werden beim Verlassen der Funktion
  # (auch bei einem Fehler) wiederhergestellt

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  # Grafikparameter für den Plot einstellen -------------------------------

  .common_par(mar = c(2.5, 6.5, 2.5, 6.5))

  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------

  .empty_plot(xlim = c(0.2, number * 1.2), ylim = c(0, sum(table(x))))

  # Hilfslinien -----------------------------------------------------------

  abline(v = .bar_centers(number), col = "grey70")

  # Eigentlichen Barplot zeichnen -----------------------------------------

  .custom_barplot(xtab)

  # X-Achsenbeschriftungen und Prozentzahlen hinzufügen -------------------

  .text_bottom(1:number, at = .bar_centers(number))
  .text_top(paste(sprintf("%.1f", 100 * prop.table(xtab)), "%"),
    at = .bar_centers(number)
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
}
