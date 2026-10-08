# Boxplots ---------------------------------------------------------------

#' Boxplots für Skalenfragen auf aggregiertem Niveau: Funktioniert, sollte aber überarbeitet werden
#'
#' @param x Daten
#' @param item_labels Labels/Text/Beschriftungen der Y-Achse
#' @param skala Skala der x-Achse
#'
#' @returns Boxplot
#'
#' @examples
#'
#' boxplot_aggr_sk(
#'   BspDaten$Plots$aggr.data,
#'   BspDaten$Plots$aggr.labels,
#'   BspDaten$Plots$aggr.skala
#' )
#'
#' @export boxplot_aggr_sk

boxplot_aggr_sk <- function(x, # Daten
                            item_labels, # Labels/Text/Beschriftungen der Y-Achse
                            skala) # Skala der x-Achse
{
  daten <- cbind(x)
  n_items <- ncol(daten)
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

#' Abbildung der Gesamtnote
#'
#' @param x (aggregierte) Daten für den Boxplot
#'
#' @returns Boxplot der Gesamtnote
#'
#' @examples boxplot_grade(BspDaten$Plots$grade)
#'
#' @export boxplot_grade

boxplot_grade <- function(x) # Daten
{
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

#' Abbildung des Rücklaufs
#'
#' @param x (aggregierte) Daten für den Boxplot
#'
#' @returns Boxplot der Rücklaufs
#'
#' @examples boxplot_rueck(BspDaten$Plots$rueck)
#'
#' @export boxplot_rueck

boxplot_rueck <- function(x) # Daten
{
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
                       skala = c(
                         "0h", "1h", "2h", "3h", "4h", "5h", "6h", "7h",
                         "8h", "9h", "10h", "11h", "12h", "mehr\nals 12h"
                       )) # Skala x-Achse
{
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

  # .text_bottom(skala, at = 1:n_skala)
  mtext(skala,
    side = 1, line = 0.5, font = 2, at = 1:n_skala,
    las = 1, padj = 1, col = "gray15"
  )

  # info1 <- "angegebener Workload der LV"
  # info2 <- paste0("[n = ", n, "]")

  # mtext(bquote(bold(.(info1)) ~ .(info2)), side = 1, line = 3, col = "gray15")
  .text_bottom_2(paste("angegebener Workload der LV [n =", n, "]"), line = 3)
}
