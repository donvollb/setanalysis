# Legenden und Beispielgrafiken für die Berichte ------------------------

#' Legende: beschrifteter Beispiel-Boxplot
#'
#' @description
#' Gibt einen Beispiel-Boxplot aus, an dem Ausreißer, Median und Maximum
#' beschriftet sind. Gedacht für den Abschnitt „Erläuterung zu Grafiken“ am
#' Anfang eines Berichts.
#'
#' @param x Werte für den Boxplot. Bei `"default"` werden feste
#'   Beispielwerte verwendet, zu denen die Beschriftungen passen.
#'
#' @returns Nichts (unsichtbar `NULL`). Die Abbildung wird als Sub-Chunk in
#'   den Bericht ausgegeben (siehe [subchunkify()]).
#'
#' @family legenden
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' bsp_boxplot()
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
bsp_boxplot <- function(x = "default") {
  if (x[1] == "default") {
    x <- c(
      1.7, 3.5, 3.6, 3.7, 4.0, 4.1, 4.2, 4.2, 4.3, 4.3, 4.4, 4.5,
      4.5, 4.7, 4.8, 4.9, 5.0, 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7
    )
  }


  subchunkify(
    c(
      opar <- par(no.readonly = TRUE),


      # Grafikparameter für den Plot einstellen -----------------------------

      .common_par(mar = c(7, 8, 5, 2.1)),

      # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) ---------------

      .empty_plot(xlim = c(1, 6)),

      # Hilfslinien ---------------------------------------------------------

      abline(v = 1:6, col = "gray70"),

      # Eigentlicher Plot ---------------------------------------------------

      .custom_boxplot(x, boxwex = 0.8, ylim = c(1, 6)),

      # Achsenbeschriftungen hinzufügen ------------------------------------

      .text_left("Beispiel-Boxplot", at = 1),
      .text_bottom(c(
        "trifft gar nicht zu", "", "", "", "",
        "trifft voll zu"
      ), at = 1:6),
      par(xpd = TRUE),

      # Beschriftungslinien -------------------------------------------------

      segments(x0 = 1.7, y0 = 1.08, x1 = 1.7, y1 = 1.66, col = "grey15"),
      segments(x0 = 4.5, y0 = 1.26, x1 = 4.5, y1 = 1.66, col = "grey15"),
      segments(x0 = 5.7, y0 = 1.17, x1 = 5.7, y1 = 1.66, col = "grey15"),
      segments(x0 = 1.1, y0 = 0.25, x1 = 2.8, y1 = 0.00, col = "grey15"),
      segments(x0 = 5.9, y0 = 0.25, x1 = 4.2, y1 = 0.00, col = "grey15"),

      # Beschriftungstexte --------------------------------------------------

      text(x = 1.7, y = 1.8, col = "grey15", label = "Ausreißer"),
      text(x = 4.5, y = 1.8, col = "grey15", label = "Median"),
      text(x = 5.7, y = 1.8, col = "grey15", label = "Max"),
      text(x = 3.5, y = 0.0, col = "grey15", label = "6-stufige Skala"),

      # Vorher gesicherte Grafikparameter wiederherstellen ------------------

      par(opar)
    ),
    fig_height = 3.8,
    fig_width = 9,
    hide = TRUE
  )

  cat("  \n  \n")
}

#' Legende: beschriftete Beispiel-Abbildung einer 6er-Skala
#'
#' @description
#' Gibt eine Beispiel-Abbildung im Stil von [barplot_sk()] aus, in der die
#' prozentuale Häufigkeit, der Mittelwert und die Standardabweichung
#' beschriftet sind. Gedacht für den Abschnitt „Erläuterung zu Grafiken“ am
#' Anfang eines Berichts.
#'
#' @param x Antworten auf einer 6-stufigen Skala. Bei `"default"` werden feste
#'   Beispielwerte verwendet, zu denen die Beschriftungen passen.
#'
#' @returns Nichts (unsichtbar `NULL`). Die Abbildung wird als Sub-Chunk in
#'   den Bericht ausgegeben (siehe [subchunkify()]).
#'
#' @family legenden
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' bsp_evasys_sk6()
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
bsp_evasys_sk6 <- function(x = "default") {
  if (x[1] == "default") {
    x <- c(
      1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6
    )
  }

  number <- 6
  xtab <- table(c(x, 1:number)) - 1
  tmin <- "linker Pol"
  tmax <- "rechter Pol"

  subchunkify(
    c(
      # Bisherige Grafikparameter speichern -----------------------------------

      opar <- par(no.readonly = TRUE),

      # Grafikparameter für den Plot einstellen -------------------------------

      .common_par(mar = c(2.5, 6.5, 6.5, 6.5)),

      # Leeren Plot zeichnen (um Hilfslinien drüber zu legen) -----------------

      .empty_plot(xlim = c(0.2, number * 1.2), ylim = c(0, sum(table(x)))),

      # Hilfslinien -----------------------------------------------------------

      abline(v = .bar_centers(number), col = "grey70"),

      # Eigentlichen Barplot zeichnen -----------------------------------------

      .custom_barplot(xtab),

      # X-Achsenbeschriftungen und Prozentzahlen hinzufügen -------------------

      .text_bottom(1:number, at = .bar_centers(number)),
      .text_top(paste(sprintf("%.1f", 100 * prop.table(xtab)), "%"),
        at = .bar_centers(number)
      ),

      # Beschriftungen der Pole hinzufügen ------------------------------------

      .text_left(tmin),
      .text_right(tmax),

      # Kleinen Boxplot darüber hinzufügen ------------------------------------

      par(new = TRUE, bty = "n"),
      boxplot(c(mean(x) - sd(x), rep(mean(x), 3), mean(x) + sd(x)),
        yaxt = "n", xaxt = "n", medcol = "black",
        horizontal = TRUE, range = 0, ylim = c(0.6, number + 0.4), medlwd = 4,
        boxlwd = 0.01, xlim = c(0.3, 1.3), whisklty = 1, outline = FALSE
      ),
      par(xpd = TRUE),

      # Erklärungen und Linien hinzufügen -------------------------------------

      text(x = 0.95, y = 2.30, col = "gray15", label = "Prozentuale Häufigkeit der Antwort"),
      text(x = 3.35, y = 2.32, col = "gray15", label = "Mittelwert"),
      text(x = 4.66, y = 2.30, col = "gray15", label = "Standardabweichung"),
      segments(x0 = 0.95, y0 = 1.85, x1 = 0.95, y1 = 2.15, col = "gray15"),
      segments(x0 = 3.38, y0 = 1.26, x1 = 3.38, y1 = 2.15, col = "gray15"),
      segments(x0 = 4.64, y0 = 1.18, x1 = 4.64, y1 = 2.15, col = "gray15"),

      # Vorher gesicherte Grafikparameter wiederherstellen --------------------

      par(opar)
    ),
    fig_width = 9, fig_height = 2.8, hide = TRUE
  )
}

#' Legende: Erklärung der Tabellenspalten
#'
#' @description
#' Erstellt eine Tabelle, die die Abkürzungen in den Kopfzeilen der
#' Statistiktabellen erklärt (n = Häufigkeit, M = Mittelwert usw.). Gedacht
#' für den Abschnitt „Legende zu Tabellen“ am Anfang eines Berichts.
#'
#' @param all `TRUE` für die Legende zu Tabellen mit Fragetext und Median
#'   (wie bei [table_stat_multi()], z. B. in der LVE), `FALSE` für die kurze
#'   Variante ohne beides.
#'
#' @returns Ein `tinytable`-Objekt (siehe [lv_table()]).
#'
#' @family legenden
#'
#' @examples
#' bsp_table_stat()
#' bsp_table_stat(all = FALSE)
#'
#' @export
bsp_table_stat <- function(all = TRUE) {
  if (all == TRUE) {
    legend_table <- data.frame(cbind(
      "Frage", "Häufigkeit", "Mittelwert",
      "Standard-abweichung", "Median",
      "kleinster be⁠ob. Wert", "größter be⁠ob. Wert"
    ))

    colnames(legend_table) <- c("Item", "n", "M", "SD", "MD", "Min", "Max")
  } else {
    legend_table <- data.frame(cbind(
      "Häufigkeit", "Mittelwert", "Standard-\nabweichung",
      "kleinster\nbeob. Wert", "größter\nbeob. Wert"
    ))

    colnames(legend_table) <- c("n", "M", "SD", "Min", "Max")
  }

  return(lv_table(legend_table))
}
