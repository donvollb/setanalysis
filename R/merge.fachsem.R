#' merge-Funktion für Fachsemester
#'
#' @param x Daten
#' @param fig.height Höhe des Plots im Dokument 
#' @param cutoff cutoff-Wert, alle Werte >= cutGoff werden zusammengefasst
#' @param group Gruppe: "a" für alle, "b" für Bachelor und "m" für Master
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#'
#' @export


merge.fachsem <- function(x, # Daten
                          fig.height = 5, # Höhe des Plots im Markdown, 5 ist optimal bei cutoff 12, damit Tabelle und Abbildung auf eine Seite passen
                          cutoff = 12, # cutoff-Wert, alle Werte >= cutoff werden zusammengefasst
                          group = "a", # Gruppe: "a" für alle, "b" für Bachelor und "m" für Master
                          inkl = "nr", # TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
                          nr = "") # Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
{
  if (inkl == "nr") {
    if (nr == "") {inkl <- TRUE} else {inkl <- eval(parse(text = paste0("inkl.", nr)))}
  }

  if (inkl == TRUE) {

    if(group == "a"){
      caps <- "(alle)"
      xl <- "Fachsemester alle"
    }

    if(group == "b"){
      caps <- "(nur Bachelor)"
      xl <- "Fachsemester Bachelor"
    }

    if(group == "m"){
      caps <- "(nur Master)"
      xl <- "Fachsemester Master"
    }


    x[x >= cutoff] <- cutoff


    cat(paste0("## Fachsemester ", caps, "  \n  \n"))
    cat("\\subsubsection{ Bezogen auf das Fach, dem die vorliegende Veranstaltung zugehoert: in welchem Fachsemester sind Sie eingeschrieben?}  \n  \n")

    # kein print bei Flextable
    #print(table.freq(x, col1.name = xl, cutoff = cutoff)) # main ist die Überschrift
    #table.freq(x, col1.name = xl, cutoff = cutoff)

    flextable::flextable_to_rmd( # Ausgabe der Flextable
      table.freq(x, col1.name = xl, cutoff = cutoff) %>%
        flextable::append_chunks(flextable::as_sub("votes"), i=1, j=2, part="header") # votes tiefergestellt
    )



    cat("  \n  \n")

    subchunkify(barplot.freq(x, xlab = xl, cutoff = cutoff), fig_height = 5, fig_width = 10) # xlab ist Label x-Achse

    cat("  \n  \n")

  }
}
