#' Funktion für die Abbildung numerischer (quasi metrisch, wie Alter, Abschlussnote, etc.)
#' Kann auch für Balkendiagramme bei ordinalen Skalen genutzt werden
#'
#' @param x Daten
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#' @param main Überschrift für Plot
#' @param xlab Beschriftung x-Achse
#' @param cut.breaks Soll es cuts geben? Wo sollen die "breaks" sein, siehe cut()
#' @param cut.labels Soll es cuts geben? Wo sollen die "labels" sein, siehe cut()
#' @param show.table Soll die Tabelle gezeigt werden
#' @param fig.height Höhe der Abbildung
#' @param cutoff Soll es einen cutoff geben? Alle Werte >= cutoff werden zusammengefasst; Ist nicht mit cuts möglich!
#'
#' @export

merge.num <- function(x, # Daten
                      inkl = "nr",  # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                      nr = "", # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
                      main = "", # Überschrift für Plot
                      xlab = "", # Beschriftung x-Achse
                      cut.breaks = "", # Soll es cuts geben? Wo sollen die "breaks" sein, siehe cut()
                      cut.labels = "", # Soll es cuts geben? Wo sollen die "labels" sein, siehe cut()
                      show.table = TRUE, # Soll die Tabelle gezeigt werden
                      fig.height = 6, # Höhe der Abbildung
                      cutoff = FALSE) # Soll es einen cutoff geben? Alle Werte >= cutoff werden zusammengefasst; Ist nicht mit cuts möglich!
{

  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }

  if (inkl == TRUE) {

    if(cut.breaks[1] != "" & cutoff != FALSE) {
      stop("Es können nicht \"cut.breaks\" und \"cutoff\" != FALSE sein.")
    }

    if (sum(!is.na(x)) > 0) {



      cat("### ", nr, " ", attr(x, "label"), "\n \n")

      x <- as.numeric(gsub(",", ".", x)) # falls mit Komma


      if(show.table == TRUE){print(table.stat.single(as.numeric(x, na.rm = TRUE),
                                                     col1.name = "n", md = TRUE))
        cat("  \n  \n")}

      if (cut.breaks[1] != "") {
        x <- cut(as.numeric(x, na.rm = TRUE),
                 breaks = cut.breaks,
                 labels = cut.labels)}
      if (cutoff != FALSE) {x[x >= cutoff] <- cutoff}

      subchunkify(barplot.freq(x, cutoff = cutoff, xlab = xlab, main = main), fig_width = 9, fig_height = fig.height)
      cat("  \n  \n")



    }
  }
}
