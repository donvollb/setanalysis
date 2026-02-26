#' Einfache Statistiktabelle für ein Item ohne Fragetext in Tabelle
#'
#' @param x Daten
#' @param md Mit Median?
#' @param col1.name Name der ersten Zelle der Kopfzeile
#' @param bold Fettdruck der Kopfzeile
#'
#' @returns Tabelle
#' @export table.stat.single

table.stat.single <- function(x, # Daten
                              md = FALSE, # Mit Median?
                              col1.name = "N_votes", # Name der ersten Zelle des headers
                              bold = TRUE) # Fette Kopfzeile?
{

  if (md == FALSE) {
    bob <- data.frame(round(psych::describe(x),2))[c(2:4, 8:9)]
    colnames(bob) <- c(col1.name, "M", "SD", "Min", "Max")
             } else {

    bob <- data.frame(round(psych::describe(x),2))[c(2:5, 8:9)]
           colnames(bob) <- c(col1.name, "M", "SD", "MD", "Min", "Max")}

  lv_table(bob, col.width = 0.5)
}
