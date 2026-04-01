#' Einfache Häufigkeitstabelle
#'
#' @param x Daten
#' @param cutoff Soll es einen "cutoff" geben? z.B. werden bei 12 alle Werte >= 12 in "12 oder höher" dargestellt
#' @param show.all Sollen auch nicht gewählte Antwortoptionen angezeigt werden?
#' @param col1.name Name der ersten Zelle der Kopfzeile
#' @param col2.name Name der zweiten Zelle der Kopfzeile (standardmäßig N)
#' @param col.width Spaltenbreiten (siehe lv_table)
#' @param order.table Soll nach Häufigkeit sortiert werden? "decreasing" für absteigendes Sortieren
#' @param bold Soll die Kopfzeile fett sein? (siehe lv_table)
#' @param digits Nachkommastellen
#' 
#' @returns Tabelle
#' 
#' @examples table.freq(BspDaten$Tabellen$freq)
#' 
#' @export table.freq

table.freq <- function(x, # Daten
                       cutoff = FALSE, # Soll es einen "cutoff" geben? z.B. werden bei 12 alle Werte >= 12 in "12 oder höher" dargestellt
                       show.all = TRUE, # Bei TRUE werden auch nicht gewählte Antwortoptionen angezeigt
                       col1.name = "", # Name der ersten Zelle des headers
                       col2.name = "n", # Name der zweiten Zelle des headers
                       col.width = "default", # Spaltenbreiten (siehe lv_table)
                       order.table = FALSE, # Soll nach Häufigkeit sortiert werden? "decreasing" für absteigendes Sortieren
                       bold = TRUE, # Soll die Kopfzeile fett sein? (siehe lv_table)
                       digits = 1) # Anzahl Nachkommastellen
{

  jim <- data.frame(descr::freq(x, plot = FALSE))
  jim <- data.frame(rownames(jim), jim)
  rownames(jim) <- NULL

  jim[jim == "NA's"] <- "NAs"


  if(show.all == FALSE)
  {
    jim <- jim[jim[, 2] != 0 & jim[, 2] != "0", ] # Falls nur gewählte Optionen angezeigt werden sollen
  }

  names.jim <- c(col1.name,
                 col2.name,
                 "%",
                 "gültige %")

  if(length(jim) == 4){
    colnames(jim) <- names.jim
  } else{
    colnames(jim) <- names.jim[1:3]
  }

  if (is.numeric(x) == TRUE) {
    if (cutoff != FALSE & max(x, na.rm = TRUE) == cutoff){jim[nrow(jim)-2, 1] <- paste0(cutoff, " oder höher")}
  }



  if (order.table != FALSE) {

    decreasing <- ifelse(order.table == "decreasing", TRUE, FALSE)
    jim <- jim[c(order(jim[1:(nrow(jim)-ncol(jim)+2), 2],
                       decreasing = decreasing),
                 (nrow(jim)-ncol(jim)+3):nrow(jim)
    ), ]
  }

  if (col.width[1] == "default" & length(jim) == 4) {col.width <- setanalysis_defaults$col.width4}
  if (col.width[1] == "default" & length(jim) == 3) {col.width <- setanalysis_defaults$col.width3}

  lv_table(jim, col.width = col.width, bold = bold, digits = digits)

}
