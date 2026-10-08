#' Rücklauf der Lehrveranstaltungen auswerten
#'
#' @description
#' Berechnet für jede Lehrveranstaltung den Rücklauf (Anzahl der Antworten
#' geteilt durch die Zahl der angemeldeten Teilnehmenden, in Prozent) und gibt
#' eine Tabelle mit Kennwerten sowie einen Boxplot ([boxplot_rueck()]) aus.
#'
#' Da die Teilnehmendenzahl meist bei der Anmeldung zur Evaluation angegeben
#' wird, kann der Rücklauf über 100 % liegen.
#'
#' @param x Vektor mit der angegebenen Teilnehmendenzahl der jeweiligen
#'   Lehrveranstaltung (in jeder Zeile einer Lehrveranstaltung derselbe Wert).
#' @param kennung Vektor mit der Kennung der Lehrveranstaltung für jede
#'   Zeile.
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' merge_rueck(BspDaten$dataLVE$Teilnehmer, BspDaten$dataLVE$Kennung)
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_rueck <- function(x, kennung) {
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
