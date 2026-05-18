#' Einfache Statistiktabelle für mehrere Items mit Fragetexten
#'
#' @param x Daten
#' @param col1.name Name der ersten Zelle der Kopfzeile
#' @param col2.name Name der zweiten Zelle der Kopfzeile
#' @param alt1 Text für erste Ausweichoption
#' @param alt2 Text für zweite Ausweichoption
#' @param alt1.list Antworthäufigkeiten erste Ausweichoption
#' @param alt2.list Antworthäufigkeiten zweite Ausweichoption
#' @param digits Anzahl der Nachkommastellend
#' @param bold Sollen die Kopfzeile fettgedruckt sein? (siehe lv_table)
#' @param bold.corner Soll die Zelle ganz links in der Kopfzeile fettgedruckt sein?
#' @param labels Fragetexte, bei "labels" werden die Labels der Variablen genommen
#'
#' @returns Tabelle
#'
#' @examples
#' 
#' table.stat.multi(BspDaten$Tabellen$multi)
#' 
#' @export table.stat.multi

table.stat.multi <- function(x,
                             col1.name = "Item", # Name der ersten Zelle des headers
                             col2.name = "N_votes", # Name der zweiten Zelle des headers
                             alt1 = FALSE, # Text für erste Ausweichoption
                             alt2 = FALSE, # Text für zweite Ausweichoption
                             alt1.list = NULL, # Antworthäufigkeiten erste Ausweichoption
                             alt2.list = NULL, # Antworthäufigkeiten zweite Ausweichoption
                             digits = 2, # Anzahl der Nachkommastellen
                             bold = TRUE, # fetter header? (siehe lv_table)
                             bold.corner = TRUE, # fette erste Zeile im header? (siehe lv.kable)
                             labels = "labels") # Fragetexte, bei "labels" werden die labels der Variablen genommen
{

  if(labels == "labels") {labels <- as.character(lapply(x, attr, which = "label"))}

  bob <- as.data.frame(psych::describe(x))[c(2:5,8:9)]
  bob <- cbind(labels, bob)
  colnames(bob) <- c(col1.name, col2.name, "M", "SD", "MD", "Min", "Max")

  widths <- setanalysis_defaults$col.width.sm

  if (alt1 != FALSE) {
    bob <- cbind(bob, alt1.list)
    colnames(bob)[length(colnames(bob))] <- alt1
    widths <- setanalysis_defaults$col.width.sm.alt1
  }

  if (alt2 != FALSE) {

    if (alt1 == FALSE) {stop("alt1 ist FALSE, alt2 aber nicht. Bitte bei nur einer Ausweichoption alt1 verwenden.")}
    bob <- cbind(bob, alt2.list)
    colnames(bob)[length(colnames(bob))] <- alt2
    widths <- setanalysis_defaults$col.width.sm.alt2
  }


  lv_table(bob,
           col.width = widths,
           bold = bold,
           digits = digits,
           bold.corner = bold.corner)
}
