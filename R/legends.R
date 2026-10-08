# Legenden und Beispielgrafiken für die Berichte ------------------------

#' Beispiel-Boxplot mit Beschriftung
#'
#' @param x Daten, bei "default" wird ein Beispieldatensatz genutzt
#'
#' @returns Beispiel-Boxplot
#'
#' @examples bsp_boxplot() |> markdown_in_viewer()
#'
#' @export bsp_boxplot

bsp_boxplot <- function(x = "default") # Daten, bei "default" wird ein Beispieldatensatz genutzt
{
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

#' Beispiel einer Sechserskala
#'
#' @param x Daten, bei "default" wird ein Beispieldatensatz genutzt
#'
#' @returns Grafik
#'
#' @examples bsp_evasys_sk6() |> markdown_in_viewer()
#'
#' @export bsp_evasys_sk6

bsp_evasys_sk6 <- function(x = "default") # Daten, bei "default" wird ein Beispieldatensatz genutzt
{
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

#' Beispiel einer Statistik-Tabelle
#'
#' @param all Soll die Tabelle Fragen enthalten? Dann TRUE, sonst FALSE
#'
#' @returns
#' Eine Tabelle mit den Spalten "Häufigkeit", "Mittelwert", "Standardabweichung",
#' "Median", "kleinster beob. Wert", "größter beob. Wert"
#'
#' @examples bsp_table_stat()
#'
#' @export bsp_table_stat

bsp_table_stat <- function(all = TRUE) # all = TRUE für eine Tabelle mit "Frage" und "Median", eher für LVE
{
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
