# Einfache Statistiktabelle für ein Item ohne Fragetext in Tabelle
table.stat.single <- function(x, # Daten
                              caption = NULL, # caption der Tabelle (siehe lv.kable)
                              md = FALSE, # Mit Median?
                              col1.name = "N_votes", # Name der ersten Zelle des headers
                              col1.width = col1.width.tss, # Breite der ersten Zeile
                              bold = TRUE, # fetter header? (siehe lv.kable)
                              bold.col1 = TRUE) # fette erste Zeile im header? (siehe lv.kable)
{

  if (md == FALSE) {
    bob <- data.frame(round(psych::describe(x),2))[c(2:4, 8:9)]
    colnames(bob) <- c(col1.name, "M", "SD", "Min", "Max")

    lv.kable(bob, caption = caption,
             #             col.width = c(col1.width, "25pt", "25pt", "25pt", "25pt"),
             col.width = c(col1.width, 9, 9, 9, 9),
             bold = bold,
             bold.col1 = bold.col1) } else {

               bob <- data.frame(round(psych::describe(x),2))[c(2:5, 8:9)]
               colnames(bob) <- c(col1.name, "M", "SD", "MD", "Min", "Max")

               lv.kable(bob, caption = caption,
                        #                        col.width = c(col1.width, "25pt", "25pt", "25pt", "25pt", "25pt"),
                        col.width = c(col1.width, 9, 9, 9, 9, 9),
                        bold = bold,
                        bold.col1 = bold.col1)
             }
}
