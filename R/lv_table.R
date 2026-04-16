#' Funktion zur Tabellenerstellung
#'
#' @param x Objekt (üblicherweise Dataframe)
#' @param col.width Vektor der Spaltenbreiten, bei "default" automatische Spaltenbreiten
#' @param bold Sollen die Kopfzeile fettgedruckt sein?
#' @param bold.corner Soll die Zelle links oben im Eck fettgedruckt sein?
#' @param digits Anzahl der Nachkommastellen
#' @param striped Soll die Tabelle Streifen (Schattierungen) erhalten
#'
#' @returns Tabelle
#' @export
#'
#' @examples lv_table(head(mtcars, 10))


lv_table <- function(x, # Objekt (am besten dataframe)
                     col.width = 1, # Spaltenbreite (Vektor, z.B. "c("30pt", "50pt")), bei "default" gibt es automatische Spaltenbreiten
                     bold = TRUE, # Soll der header fett sein?
                     bold.corner = TRUE, # Soll die Eckzelle fett sein?
                     digits = 2, # Wie viele Nachkommastellen in der Tabelle?
                     striped = TRUE) # gestrifte Tabelle?
{

  # Erkennen von ganzzahligen Werten und Umwandlung in Integer ------------
  
  for (Spalte in names(x)) {
    if (is.numeric(x[[Spalte]])) {
      if (all(is.na(x[[Spalte]]) | x[[Spalte]] == round(x[[Spalte]]))) {
        x[[Spalte]] <- as.integer(x[[Spalte]])
      }
    }
  }

  # Erstellen der Tabelle mit tinytable -----------------------------------
  
  Tabelle <- tinytable::tt(x, width = col.width)

  # Kopfzeile fett --------------------------------------------------------

  if (bold == TRUE) { Tabelle <- style_tt(Tabelle, i = 0, bold = TRUE) }

  # Eckzelle nicht fett ---------------------------------------------------

  if (bold.corner == FALSE) { Tabelle <- style_tt(Tabelle, i = 0, j = 1,
                                                  bold = FALSE) }
  
  # Streifenmuster hinzufügen (Akzentfarbe mit 90% Transparenz) -----------
  
  if (striped == TRUE) {
    Tabelle <- style_tt(Tabelle, i = seq(0, nrow(Tabelle), by = 2),
                        background = adjustcolor(setanalysis_defaults$color.bars,
                                                 alpha.f = 0.1))}

  # Anzahl der Nachkommastellen festlegen ---------------------------------

  Tabelle <- tt_format(Tabelle, num_zero = TRUE, num_fmt = "decimal", digits = digits)
  
  # Nur eine Zeile: Erste Spalte linksbündig, Rest zentriert --------------

  if (nrow(Tabelle) == 1) {
    Tabelle <- style_tt(Tabelle, j = 1, align = "l")

  if (ncol(Tabelle) > 2) {
    Tabelle <- style_tt(Tabelle, j = 2:(ncol(Tabelle)), align = "c")
  }
  
   # Ansonsten: Erste Spalte linksbündig, Rest rechtsbündig ----------------
  } else {
    Tabelle <- style_tt(Tabelle, j = 1, align = "l")

  if (ncol(Tabelle) > 2) {
    Tabelle <- style_tt(Tabelle, j = 2:(ncol(Tabelle)), align = "r")
  }
}  
  # Fertige Tabelle ausgeben ----------------------------------------------
  
  return(Tabelle)
}

