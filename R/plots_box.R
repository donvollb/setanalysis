# Boxplots ---------------------------------------------------------------

#' Boxplots für mehrere Skalenfragen
#'
#' @description
#' Zeichnet für jedes Item einen waagerechten Boxplot auf einer gemeinsamen
#' Skala, beschriftet mit den Fragetexten. Wird von [merge_aggr_sk()]
#' verwendet. Bei einer 5-stufigen Skala erscheint unter der Abbildung ein
#' Hinweis, dass die Skalenlogik von den 6-stufigen Skalen abweicht.
#'
#' @param x Data Frame mit einer numerischen Spalte pro Item.
#' @param item_labels Beschriftungen der Items (y-Achse), gleiche Reihenfolge
#'   wie die Spalten von `x`.
#' @param skala Beschriftungen der Skalenstufen (x-Achse), z. B.
#'   `c("trifft gar nicht zu", "", "", "", "", "trifft voll zu")`. Die Länge
#'   bestimmt die Anzahl der Stufen.
#'
#' @returns Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.
#'
#' @family grafiken
#'
#' @examples
#' boxplot_aggr_sk(
#'   BspDaten$Plots$aggr.data[, 1:4],
#'   BspDaten$Plots$aggr.labels[1:4],
#'   BspDaten$Plots$aggr.skala
#' )
#'
#' @export
boxplot_aggr_sk <- function(x, item_labels, skala) {
  data_matrix <- cbind(x)
  n_items <- ncol(data_matrix)
  n_skala <- length(skala)

  # Grafikparameter speichern; sie werden beim Verlassen der Funktion
  # (auch bei einem Fehler) wiederhergestellt

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  # Grafikparameter für den Plot einstellen -------------------------------

  .common_par()

  if (n_skala == 5) {
    par(mar = c(4, 20, 0.1, 2.1)) # mehr Platz für Hinweistext bei 5er Skala
  } else {
    par(mar = c(2.1, 20, 0.1, 2.1))
  }

  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------

  .empty_plot(
    xlim = c(1, n_skala),
    ylim = c(0.5, n_items + 0.5)
  )

  # Hilfslinien -----------------------------------------------------------

  abline(v = c(1:n_skala), col = "grey70")

  # Eigentlichen Boxplot zeichnen -----------------------------------------

  .custom_boxplot(x,
    boxwex = 0.8,
    ylim = c(0.5, n_items + 0.5),
    xlim = c(1, n_skala)
  )

  # Achsenbeschriftungen hinzufügen --------------------------------------

  .text_left(item_labels, at = 1:n_items)
  .text_bottom(skala, at = 1:n_skala)

  # Hinweistext hinzufügen, wenn 5er Skala --------------------------------

  if (n_skala == 5) {
    mtext("Hinweis: andere Skalenlogik (im Vergleich zu den 6er-Skalen)",
      side = 1, line = 3, col = "gray15", font = 3
    )
  }
}

#' Boxplot der Gesamtnote
#'
#' @description
#' Zeichnet einen waagerechten Boxplot auf der Notenskala von „sehr gut“ (1)
#' bis „ungenügend“ (6). Wird von [merge_grade()] verwendet.
#'
#' @param x Numerischer Vektor (oder einspaltiger Data Frame) mit den Noten,
#'   meist schon je Lehrveranstaltung gemittelt.
#'
#' @returns Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.
#'
#' @family grafiken
#'
#' @examples
#' boxplot_grade(BspDaten$Plots$grade)
#'
#' @export
boxplot_grade <- function(x) {
  # Grafikparameter speichern; sie werden beim Verlassen der Funktion
  # (auch bei einem Fehler) wiederhergestellt

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  # Grafikparameter für den Plot einstellen -------------------------------

  .common_par(mar = c(2.1, 10.1, 0.1, 2.1))

  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------

  .empty_plot(xlim = c(1, 6))

  # Hilfslinien -----------------------------------------------------------

  abline(v = c(1, 2, 3, 4, 5, 6), col = "gray70")

  # Eigentlichen Boxplot zeichnen -----------------------------------------

  .custom_boxplot(x, boxwex = 0.8, ylim = c(1, 6))

  # Achsenbeschriftungen hinzufügen --------------------------------------

  .text_left("Gesamtnote der LV", at = 1)
  .text_bottom(
    at = c(1, 2, 3, 4, 5, 6),
    c(
      "sehr gut", "gut", "befriedigend",
      "ausreichend", "mangelhaft", "ungenügend"
    )
  )
}

#' Boxplot des Rücklaufs
#'
#' @description
#' Zeichnet einen waagerechten Boxplot des Rücklaufs in Prozent (Achse von 0
#' bis 120 %). Wird von [merge_rueck()] verwendet.
#'
#' @param x Numerischer Vektor mit dem Rücklauf je Lehrveranstaltung in
#'   Prozent.
#'
#' @returns Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.
#'
#' @family grafiken
#'
#' @examples
#' boxplot_rueck(BspDaten$Plots$rueck)
#'
#' @export
boxplot_rueck <- function(x) {
  # Grafikparameter speichern; sie werden beim Verlassen der Funktion
  # (auch bei einem Fehler) wiederhergestellt

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  # Grafikparameter für den Plot einstellen -------------------------------

  .common_par(mar = c(2.1, 9, 0.1, 2.5))

  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------

  .empty_plot(xlim = c(0, 120))

  # Hilfslinien -----------------------------------------------------------

  abline(v = c(0, 20, 40, 60, 80, 100), col = "gray70")

  # Eigentlichen Boxplot zeichnen -----------------------------------------

  .custom_boxplot(x, boxwex = 0.8, ylim = c(0, 120))

  # Achsenbeschriftungen hinzufügen --------------------------------------

  .text_left("Rücklauf in Prozent", at = 1)

  .text_bottom(c("0", "20", "40", "60", "80", "100"),
    at = c(0, 20, 40, 60, 80, 100)
  )
}

#' Boxplot des Workloads
#'
#' @description
#' Zeichnet einen waagerechten Boxplot des angegebenen Workloads in Stunden
#' pro Woche; unter der Achse steht die Anzahl der Lehrveranstaltungen. Wird
#' von [merge_wl()] verwendet.
#'
#' @param x Numerischer Vektor mit dem Workload je Lehrveranstaltung (Stunden
#'   pro Woche).
#' @param skala Beschriftungen der x-Achse, eine pro Stufe ab 0 Stunden.
#'
#' @returns Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.
#'
#' @family grafiken
#'
#' @examples
#' boxplot_wl(BspDaten$Plots$WL)
#'
#' @export
boxplot_wl <- function(x,
                       skala = c(
                         "0h", "1h", "2h", "3h", "4h", "5h", "6h", "7h",
                         "8h", "9h", "10h", "11h", "12h", "mehr\nals 12h"
                       )) {
  # Berechnung der Anzahl der Veranstaltungen und Länge der Skala ---------

  n <- length(x)
  n_skala <- length(skala)

  # Grafikparameter speichern; sie werden beim Verlassen der Funktion
  # (auch bei einem Fehler) wiederhergestellt

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  # Grafikparameter für den Plot einstellen -------------------------------

  .common_par(mar = c(7, 2, 0.1, 2))

  # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------

  .empty_plot(xlim = c(1, n_skala))

  # Einfügen von vertikalen Linien ----------------------------------------

  abline(v = c(1:n_skala), col = "gray70")

  # Plot über die Vertikalen Linien drüber plotten ------------------------

  .custom_boxplot(x, boxwex = 0.8, ylim = c(1:n_skala))

  # Beschriftungen einfügen -----------------------------------------------

  mtext(skala,
    side = 1, line = 0.5, font = 2, at = 1:n_skala,
    las = 1, padj = 1, col = "gray15"
  )

  .text_bottom_2(paste("angegebener Workload der LV [n =", n, "]"), line = 3)
}
