#' Boxplot für Rücklaufsabbildung (LVE)
#'
#' @param x Objekt mit Teilnehmendenzahlen
#' @param kennung Kennungen
#'
#' @returns Boxplot der Rückläufe
#'
#' @examples
#' merge_rueck(
#'   BspDaten$dataLVE$Teilnehmer,
#'   BspDaten$dataLVE$Kennung
#' ) |> markdown_in_viewer()
#'
#' @export merge_rueck

merge_rueck <- function(x, # Objekt mit Teilnehmendenzahlen
                        kennung) {
  participants <- data.frame(kennung, x) # Erstelle einen Datensatz aus beiden
  per_course <- participants[!duplicated(participants$kennung), ] # Nehme nur eine Zeile pro Kennung
  n_per_course <- data.frame(table(kennung)) # Zähle, wie oft jede Kennung vorkommt
  merged <- merge(per_course, n_per_course, by = "kennung") # Füge das mit dem Datensatz z zusammen

  # Datensatz z enthält an dieser Stelle:
  # Eine Spalte mit jeder LV-Kennung
  # Eine Spalte mit der zugelassenen Teilnehmendenzahl pro Kennung
  # Eine Spalte mit der bisherigen Teilnehmendenzahl

  x <- as.numeric(merged$Freq) / as.numeric(merged$x) * 100 # Teile die bisherigende Tn-Zahl durch die Zugelassenen (mal hundert)

  # Ausgabe der Tabelle ---------------------------------------------------
  subchunkify(table_stat_single(x, col1.name = "n", digits = 1))

  cat("  \n  \n")

  subchunkify(boxplot_rueck(x), fig_height = 2, fig_width = 9)
}
