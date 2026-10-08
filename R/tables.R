# Tabellen ---------------------------------------------------------------

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

  for (column in names(x)) {
    if (is.numeric(x[[column]])) {
      if (all(is.na(x[[column]]) | x[[column]] == round(x[[column]]))) {
        x[[column]] <- as.integer(x[[column]])
      }
    }
  }

  # Erstellen der Tabelle mit tinytable -----------------------------------

  tab <- tinytable::tt(x, width = col.width)

  # Kopfzeile fett --------------------------------------------------------

  if (bold == TRUE) {
    tab <- style_tt(tab, i = 0, bold = TRUE)
  }

  # Eckzelle nicht fett ---------------------------------------------------

  if (bold.corner == FALSE) {
    tab <- style_tt(tab,
      i = 0, j = 1,
      bold = FALSE
    )
  }

  # Streifenmuster hinzufügen (Akzentfarbe mit 90% Transparenz) -----------

  if (striped == TRUE) {
    tab <- style_tt(tab,
      i = seq(0, nrow(tab), by = 2),
      background = adjustcolor(setanalysis_defaults$color.bars,
        alpha.f = 0.1
      )
    )
  }

  # Anzahl der Nachkommastellen festlegen ---------------------------------

  tab <- tt_format(tab, num_zero = TRUE, num_fmt = "decimal", digits = digits)

  # Nur eine Zeile: Erste Spalte linksbündig, Rest zentriert --------------

  if (nrow(tab) == 1) {
    tab <- style_tt(tab, j = 1, align = "l")

    if (ncol(tab) > 2) {
      tab <- style_tt(tab, j = 2:(ncol(tab)), align = "c")
    }

    # Ansonsten: Erste Spalte linksbündig, Rest rechtsbündig ----------------
  } else {
    tab <- style_tt(tab, j = 1, align = "l")

    if (ncol(tab) > 2) {
      tab <- style_tt(tab, j = 2:(ncol(tab)), align = "r")
    }
  }
  # Fertige Tabelle ausgeben ----------------------------------------------

  return(tab)
}

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
#' @examples table_freq(BspDaten$Tabellen$freq)
#'
#' @export table_freq

table_freq <- function(x, # Daten
                       cutoff = FALSE, # Soll es einen "cutoff" geben? z.B. werden bei 12 alle Werte >= 12 in "12 oder höher" dargestellt
                       show.all = TRUE, # Bei TRUE werden auch nicht gewählte Antwortoptionen angezeigt
                       col1.name = "", # Name der ersten Zelle des headers
                       col2.name = "n", # Name der zweiten Zelle des headers
                       col.width = "default", # Spaltenbreiten (siehe lv_table)
                       order.table = FALSE, # Soll nach Häufigkeit sortiert werden? "decreasing" für absteigendes Sortieren
                       bold = TRUE, # Soll die Kopfzeile fett sein? (siehe lv_table)
                       digits = 1) # Anzahl Nachkommastellen
{
  freq_table <- data.frame(descr::freq(x, plot = FALSE))
  freq_table <- data.frame(rownames(freq_table), freq_table)
  rownames(freq_table) <- NULL

  freq_table[freq_table == "NA's"] <- "NAs"


  if (show.all == FALSE) {
    freq_table <- freq_table[freq_table[, 2] != 0 & freq_table[, 2] != "0", ] # Falls nur gewählte Optionen angezeigt werden sollen
  }

  header <- c(
    col1.name,
    col2.name,
    "%",
    "gültige %"
  )

  if (length(freq_table) == 4) {
    colnames(freq_table) <- header
  } else {
    colnames(freq_table) <- header[1:3]
  }

  if (is.numeric(x) == TRUE) {
    if (cutoff != FALSE & max(x, na.rm = TRUE) == cutoff) {
      freq_table[nrow(freq_table) - 2, 1] <- paste0(cutoff, " oder höher")
    }
  }


  if (order.table != FALSE) {
    decreasing <- ifelse(order.table == "decreasing", TRUE, FALSE)
    freq_table <- freq_table[c(
      order(freq_table[1:(nrow(freq_table) - ncol(freq_table) + 2), 2],
        decreasing = decreasing
      ),
      (nrow(freq_table) - ncol(freq_table) + 3):nrow(freq_table)
    ), ]
  }

  if (col.width[1] == "default" & length(freq_table) == 4) {
    col.width <- setanalysis_defaults$col.width4
  }
  if (col.width[1] == "default" & length(freq_table) == 3) {
    col.width <- setanalysis_defaults$col.width3
  }

  lv_table(freq_table, col.width = col.width, bold = bold, digits = digits)
}

#' Einfache Statistiktabelle für ein Item ohne Fragetext in Tabelle
#'
#' @param x Daten
#' @param md Mit Median?
#' @param col1.name Name der ersten Zelle der Kopfzeile
#' @param bold Fettdruck der Kopfzeile
#' @param digits Anzahl der Nachkommastellen in der Tabelle
#'
#' @returns Tabelle
#' @export table_stat_single

table_stat_single <- function(x, # Daten
                              md = FALSE, # Mit Median?
                              col1.name = "N_votes", # Name der ersten Zelle des headers
                              bold = TRUE, # Fette Kopfzeile?
                              digits = 2) # Anzahl der Nachkommastellen in der Tabelle
{
  if (md == FALSE) {
    stats_table <- data.frame(round(psych::describe(x), 2))[c(2:4, 8:9)]
    colnames(stats_table) <- c(col1.name, "M", "SD", "Min", "Max")
  } else {
    stats_table <- data.frame(round(psych::describe(x), 2))[c(2:5, 8:9)]
    colnames(stats_table) <- c(col1.name, "M", "SD", "MD", "Min", "Max")
  }

  lv_table(stats_table, col.width = 0.5, bold = bold, digits = digits)
}

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
#' table_stat_multi(BspDaten$Tabellen$multi)
#'
#' @export table_stat_multi

table_stat_multi <- function(x,
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
  if (labels == "labels") {
    labels <- as.character(lapply(x, attr, which = "label"))
  }

  stats_table <- as.data.frame(psych::describe(x))[c(2:5, 8:9)]
  stats_table <- cbind(labels, stats_table)
  colnames(stats_table) <- c(col1.name, col2.name, "M", "SD", "MD", "Min", "Max")

  widths <- setanalysis_defaults$col.width.sm

  if (alt1 != FALSE) {
    stats_table <- cbind(stats_table, alt1.list)
    colnames(stats_table)[length(colnames(stats_table))] <- alt1
    widths <- setanalysis_defaults$col.width.sm.alt1
  }

  if (alt2 != FALSE) {
    if (alt1 == FALSE) {
      stop("alt1 ist FALSE, alt2 aber nicht. Bitte bei nur einer Ausweichoption alt1 verwenden.")
    }
    stats_table <- cbind(stats_table, alt2.list)
    colnames(stats_table)[length(colnames(stats_table))] <- alt2
    widths <- setanalysis_defaults$col.width.sm.alt2
  }


  lv_table(stats_table,
    col.width = widths,
    bold = bold,
    digits = digits,
    bold.corner = bold.corner
  )
}
