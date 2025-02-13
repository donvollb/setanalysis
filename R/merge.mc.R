#' merge-Funktion für MC-Fragen
#'
#' @param x Daten (dataframe mit mehreren Spalten) -> Wichtig: Darauf achten, dass Labels enthalten sind
#' @param head Fragetext, bei "default" wird dieser automatisch aus den Labels gezogen
#' @param col1.name Name der Antwortoption in der Tabelle
#' @param col2.name Name der n-Spalte in der Tabelle
#' @param show.table Soll die Tabelle angezeigt werden?
#' @param fig.height Höhe der Abbildung, bei "default" ist es „Anzahl der Antwortoptionen“ * 0.75 + 1
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#' @param lime Für Daten im Format nach LimeSurvey Export (nach Syntax-Skript)
#' @param filter FILTER-Klammer für LimeSurvey
#' @param valid.perc Mit gültigen Prozent?
#' @param order.table Soll nach Häufigkeit sortiert werden? "decreasing" für absteigendes Sortieren
#' @param show.plot Soll der Plot angezeigt werden?
#'
#' @export

merge.mc <- function(x, # Daten (dataframe mit mehreren Spalten) -> Wichtig: Darauf achten, das Labels enthalten sind
                     head = "default", # Fragetext, bei "default wird dieser automatisch aus den Lables gezogen
                     col1.name = "Antwortoption", # Erste Zelle der ersten Spalte in Tabelle
                     col2.name = "n", # Name der n-Spalte in Tabelle
                     show.table = TRUE, # Soll Tabelle angezeigt werden?
                     fig.height = "default", # Höhe der Abbildung, bei "default" ist es Anzahl der Antwortoptionen*0.75 +1
                     inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                     nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                     lime = FALSE, # Für Daten im Format nach LimeSurvey Export (nach Syntax-Skript)
                     filter = FALSE, # FILTER-Klammer für LimeSurvey
                     valid.perc = TRUE, # mit gültigen Prozent?
                     order.table = FALSE, # Soll nach Häufigkeit sortiert werden? "decreasing" für absteigendes Sortieren
                     show.plot = show.plot.mc) # Soll der Plot angezeigt werden?
{
  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }

  if (inkl == TRUE) {

    if (lime == TRUE) {

      copy <- x

      for (l in 1:ncol(x)) {

        label <- attr(x[, l], "label")
        answer <- sub("\\].*", "", label)
        answer <- sub("\\[", "", answer)
        label_cut <- sub(".*].", "", label)


        x[, l] <- as.numeric(x[, l], na.rm = TRUE)
        x[x[, l] == 2 & !is.na(x[, l]), l] <- 0
        x[x[, l] == 1 & !is.na(x[, l]), l] <- l


        attr(x[, l], "label") <- paste0(label_cut, " (Mehrfachantwort möglich): ", answer)
      }

      if (filter != FALSE) {
        attr(x[, 1], "label") <- paste0("[", filter, "] ", attr(x[, 1], "label"))
      }


    }


    if (head == "default") {head <- sub(":.*", "", attr(x[, 1], "label"))}
    cat("\\subsubsection{", nr, replace.latex.issues(head), "}  \n  \n")

    val.labels <- sub('.*: ', '', as.character(lapply(x, attr, which = "label")))


    if (valid.perc == TRUE) {

      results <- data.frame(matrix(nrow = length(x), ncol = 4))
      colnames(results) <-
        c(col1.name, col2.name, "\\%", "gültige \\%")
      results[, 1] <- replace.latex.issues(val.labels)
      for (n in 1:length(x)) {
        results[n, 2] <- sum(x[, n] != 0, na.rm = TRUE)
        results[n, 3] <- round(results[n, 2] / nrow(x) * 100, digits = 2)
        results[n, 4] <-
          round(results[n, 2] / nrow(x[!is.na(x[, 1]),]) * 100, digits = 2)
      }

      if (order.table != FALSE) {

        decreasing <- ifelse(order.table == "decreasing", TRUE, FALSE)
        results <- results[order(results[, 2], decreasing = decreasing), ]

      }

      results[nrow(results)+1, ] <- c("NAs", nrow(x[is.na(x[, 1]),]),
                                      round(nrow(x[is.na(x[, 1]),]) / nrow(x) * 100, 2),
                                      "NA")
      results[nrow(results)+1, ] <- c("Total", nrow(x),
                                      "NA",
                                      "NA")

    } else {
      results <- data.frame(matrix(nrow = length(x), ncol = 3))
      colnames(results) <-
        c(col1.name, "N_votes", "\\%")
      results[, 1] <- replace.latex.issues(val.labels)
      for (n in 1:length(x)) {
        results[n, 2] <- sum(x[, n] != 0, na.rm = TRUE)
        results[n, 3] <-
          round(results[n, 2] / nrow(x) * 100, digits = 2)
      }

    }

    if(ncol(results) == 4) {col.width <- col.width4} else {col.width <- col.width3}

    if(show.table == TRUE) {subchunkify(lv.kable(results, col.width = col.width) , fig_height = 7, fig_width = 9)}

    colnames(results) <- c("label", "freq", "perc")
    results[, 1] <- as.character(auto.newline2(results[, 1], number = 40))

    if(show.plot == TRUE) {
      if(fig.height == "default")
      {subchunkify(barplot.sc.mc(x = results, xlab = "Häufigkeit"), fig_height = (1 + 0.75*nrow(results)), fig_width = 9)}
      else
      {subchunkify(barplot.sc.mc(x = results, xlab = "Häufigkeit"), fig_height = fig.height, fig_width = 9)}
    }

    cat("  \n  \n")

  }
}
