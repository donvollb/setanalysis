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
  inkl <- .resolve_inkl(inkl, nr)

  if (inkl != TRUE) {
    return(invisible())
  }

  # Beschriftungen je nach Gruppe -----------------------------------------

  captions <- c(a = "(alle)", b = "(nur Bachelor)", m = "(nur Master)")
  col_names <- c(
    a = "Fachsemester alle", b = "Fachsemester Bachelor", m = "Fachsemester Master"
  )
  if (!group %in% names(captions)) {
    stop('`group` muss "a" (alle), "b" (Bachelor) oder "m" (Master) sein.', call. = FALSE)
  }

  # Alle Werte ab `cutoff` zusammenfassen ---------------------------------

  x[x >= cutoff] <- cutoff
  x <- factor(x)
  levels(x)[cutoff] <- paste0(cutoff, "+")

  # Ausgabe ---------------------------------------------------------------

  cat(paste0("## Fachsemester ", captions[[group]], "  \n  \n"))
  cat("### Bezogen auf das Fach, dem die vorliegende Veranstaltung zugehört: in welchem Fachsemester sind Sie eingeschrieben?  \n  \n")

  subchunkify(
    table_freq(x,
      col1.name = col_names[[group]], col2.name = "n",
      cutoff = cutoff
    )
  )

  cat("  \n  \n")
  subchunkify(barplot_freq(x, xlab = "Fachsemester"), fig_height = fig.height, fig_width = 10)
  cat("  \n  \n")
}
