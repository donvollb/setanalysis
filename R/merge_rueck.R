#' Boxplot für Rücklaufsabbildung (LVE)
#'
#' @param x Objekt mit Teilnehmendenzahlen
#' @param kennung Kennungen
#'
#' @returns Boxplot der Rückläufe
#' 
#' @examples
#' merge_rueck(BspDaten$dataLVE$Teilnehmer,
#'             BspDaten$dataLVE$Kennung) |> markdown_in_viewer()
#' 
#' @export merge_rueck

merge_rueck <- function(x, # Objekt mit Teilnehmendenzahlen
                        kennung)
{
  z <- data.frame(kennung, x) # Erstelle einen Datensatz aus beiden
  z.uni <- z[!duplicated(z$kennung),] # Nehme nur eine Zeile pro Kennung
  tb.kennung <- data.frame(table(kennung)) # Zähle, wie oft jede Kennung vorkommt
  all <- merge(z.uni, tb.kennung, by = "kennung") # Füge das mit dem Datensatz z zusammen
  
  # Datensatz z enthält an dieser Stelle:
  # Eine Spalte mit jeder LV-Kennung
  # Eine Spalte mit der zugelassenen Teilnehmendenzahl pro Kennung
  # Eine Spalte mit der bisherigen Teilnehmendenzahl
  
  x.new <- as.numeric(all$Freq)/as.numeric(all$x)*100 # Teile die bisherigende Tn-Zahl durch die Zugelassenen (mal hundert)
  x <- x.new
  
  # Ausgabe der Flextable
  #  print(table.stat.single(x, col1.name = "N\\textsubscript{courses}")) # kein print() bei Flextable
  #  table.stat.single(x, col1.name = "N_courses") # klappt nicht
  #  knitr::knit_print(table.stat.single(x, col1.name = "N_courses")) # klappt nicht
  #  subchunkify(table.stat.single(x, col1.name = "N_courses")) # für if und for bei Quarto erforderliche Funktion
  subchunkify( # Ausgabe der Flextable
    table.stat.single(x, col1.name = "N") |>
      flextable::append_chunks(flextable::as_sub("courses"), i=1, j=1, part="header") # courses tiefergestellt
  )
  
  cat("  \n  \n")
  
  subchunkify(boxplot_rueck(x), fig_height = 2, fig_width = 9)
}

#' @noRd
#' @export

boxplot.ruecklauf <- merge_rueck