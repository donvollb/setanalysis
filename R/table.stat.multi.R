#' Einfache Statistiktabelle für mehrere Items mit Fragetexten
#'
#' @param x Daten
#' @param caption Überschrift der Tabelle (siehe lv.kable) 
#' @param col1.name Name der ersten Zelle der Kopfzeile
#' @param col2.name Name der zweiten Zelle der Kopfzeile
#' @param alt1 Text für erste Ausweichoption
#' @param alt2 Text für zweite Ausweichoption
#' @param alt1.list Antworthäufigkeiten erste Ausweichoption
#' @param alt2.list Antworthäufigkeiten zweite Ausweichoption
#' @param bold Sollen die Kopfzeile fettgedruckt sein? (siehe lv.kable)
#' @param bold.col1 Soll die erste Zelle der Kopfzeile fettgedruckt sein? (siehe lv.kable)
#' @param labels Fragetexte, bei "labels" werden die Labels der Variablen genommen
#'
#' @returns Tabelle
#' @export

table.stat.multi <- function(x, caption = NULL, # caption der Tabelle (siehe lv.kable)
                             col1.name = "Item", # Name der ersten Zelle des headers
                             col2.name = "N_votes", # Name der zweiten Zelle des headers
                             alt1 = FALSE, # Text für erste Ausweichoption
                             alt2 = FALSE, # Text für zweite Ausweichoption
                             alt1.list = NULL, # Antworthäufigkeiten erste Ausweichoption
                             alt2.list = NULL, # Antworthäufigkeiten zweite Ausweichoption
                             bold = TRUE, # fetter header? (siehe lv.kable)
                             bold.col1 = TRUE, # fette erste Zeile im header? (siehe lv.kable)
                             labels = "labels") # Fragetexte, bei "labels" werden die labels der Variablen genommen
{

  if(labels == "labels") {labels <- as.character(lapply(x, attr, which = "label"))}

  bob <- as.data.frame(round(psych::describe(x), digits = 2))[c(2:5,8:9)]
  bob <- cbind(labels, bob)
  bob[, 1] <- replace.latex.issues(bob[, 1])
  colnames(bob) <- c(col1.name,
                     col2.name,
                     "M",
                     "SD",
                     "MD",
                     "Min",
                     "Max")

  widths <- settings$col.width.sm

  if (alt1 != FALSE) {
    bob <- cbind(bob, alt1.list)
    colnames(bob)[length(colnames(bob))] <- alt1
    widths <- settings$col.width.sm.alt1
  }

  if (alt2 != FALSE) {

    if (alt1 == FALSE) {stop("alt1 ist FALSE, alt2 aber nicht. Bitte bei nur einer Ausweichoption alt1 verwenden.")}
    bob <- cbind(bob, alt2.list)
    colnames(bob)[length(colnames(bob))] <- alt2
    widths <- settings$col.width.sm.alt2
  }


  lv.kable(bob, caption = caption,
           col.width = widths,
           bold = bold,
           bold.col1 = bold.col1)
}
