#' merge-Funktion für Fachsemester
#' `merge.fachsem()` ist eine veraltete Schreibweise der gleichen Funktion
#'
#' @param x Daten
#' @param fig.height Höhe des Plots im Dokument 
#' @param cutoff cutoff-Wert, alle Werte >= cutGoff werden zusammengefasst
#' @param group Gruppe: "a" für alle, "b" für Bachelor und "m" für Master
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param nr Nummer, die Grundlage für entsprechende inkl. Variable ist und vorne an den Fragetext gestellt wird
#'
#' @examples merge_fachsem(BspDaten$dataLVE$FachSemN) |> markdown_in_viewer()
#'
#' @export merge_fachsem

merge_fachsem <- function(x, # Daten
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
    x <- factor(x)
    levels(x)[cutoff] <- paste0(cutoff, "+")

    cat(paste0("## Fachsemester ", caps, "  \n  \n"))
    cat("### Bezogen auf das Fach, dem die vorliegende Veranstaltung zugehört: in welchem Fachsemester sind Sie eingeschrieben?  \n  \n")


    subchunkify( 
      table.freq(x, col1.name = xl, col2.name = "n",
                 cutoff = cutoff)
                )


    cat("  \n  \n")
    subchunkify(barplot_freq(x, xlab = "Fachsemester"), fig_height = 5, fig_width = 10) # xlab ist Label x-Achse
    cat("  \n  \n")

  }
}

#' @noRd
#' @export merge.fachsem
merge.fachsem <- merge_fachsem
