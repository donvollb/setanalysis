# Tabellen ---------------------------------------------------------------

#' Tabelle im Stil der Berichte formatieren
#'
#' @description
#' Wandelt einen Data Frame in eine mit [tinytable::tt()] formatierte Tabelle
#' im einheitlichen Stil der Berichte um: fette Kopfzeile, abwechselnd
#' eingefärbte Zeilen (Akzentfarbe `color.bars` aus [setanalysis_defaults]
#' mit 10 % Deckkraft), erste Spalte linksbündig, übrige Spalten rechtsbündig
#' (bei nur einer Zeile zentriert). Spalten mit ganzen Zahlen werden ohne
#' Nachkommastellen gezeigt.
#'
#' Sonderzeichen in den Zellen (z. B. `$`, `#`, `_` oder `//` in offenen
#' Antworten) werden maskiert und erscheinen im Bericht als normaler Text.
#' Die Kopfzeile wird nicht maskiert, sie darf also Typst-Code enthalten
#' (z. B. `'#text(weight: "bold")[Item]'`).
#'
#' Alle anderen Tabellenfunktionen des Pakets nutzen `lv_table()`.
#'
#' @param x Data Frame mit dem Tabelleninhalt.
#' @param col.width Spaltenbreiten, wie bei `width` in [tinytable::tt()]:
#'   eine Zahl (Breite der ganzen Tabelle als Anteil der Seitenbreite) oder ein
#'   Vektor mit relativen Breiten je Spalte.
#' @param bold Soll die Kopfzeile fett gedruckt werden?
#' @param bold.corner Soll auch die Zelle oben links fett sein? Bei `FALSE`
#'   wird sie normal gedruckt (z. B. wenn dort ein Hinweis zur Skala steht).
#' @param digits Anzahl der Nachkommastellen für Zahlen.
#' @param striped Sollen die Zeilen abwechselnd eingefärbt werden?
#'
#' @returns Ein `tinytable`-Objekt. Im Bericht (auch über [subchunkify()])
#'   wird es automatisch im passenden Format ausgegeben.
#'
#' @family tabellen
#'
#' @examples
#' lv_table(head(mtcars))
#'
#' lv_table(head(mtcars), striped = FALSE, digits = 1)
#'
#' @export
lv_table <- function(x,
                     col.width = 1,
                     bold = TRUE,
                     bold.corner = TRUE,
                     digits = 2,
                     striped = TRUE) {
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

  # Sonderzeichen in den Zellen maskieren ---------------------------------
  # Antworten und Labels (z. B. mit $, #, _ oder //) erscheinen so als Text.
  # Die Kopfzeile bleibt unverändert, weil sie Typst-Code enthalten kann.

  if (nrow(tab) > 0) {
    tab <- tt_format(tab, i = seq_len(nrow(tab)), escape = TRUE)
  }

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

# Text für Typst-Markup maskieren (für Daten, die in selbst geschriebenen
# Typst-Code eingesetzt werden, z. B. Skalenlabels in Tabellenköpfen).
# Gleiche Zeichen wie tinytable bei format_tt(escape = TRUE).
.escape_typst <- function(x) {
  gsub("([][\\\\#$*_<>@`~=+/\"-])", "\\\\\\1", x)
}

#' Häufigkeitstabelle erstellen
#'
#' @description
#' Erstellt eine Häufigkeitstabelle mit absoluter Häufigkeit, Prozent und der
#' Zeile „Total“. Gibt es fehlende Werte, kommen die Zeile „NAs“ und die
#' Spalte „gültige %“ (Prozent ohne fehlende Werte) hinzu. Wird z. B. von
#' [merge_sc()] und [merge_fachsem()] verwendet.
#'
#' @param x Vektor oder Faktor mit den Antworten.
#' @param cutoff Ist der größte Wert in `x` gleich `cutoff`, wird er als
#'   „`cutoff` oder höher“ beschriftet (für vorher zusammengefasste Werte),
#'   sonst `FALSE`.
#' @param show.all Sollen auch Antwortoptionen gezeigt werden, die niemand
#'   gewählt hat?
#' @param col1.name,col2.name Überschrift der ersten Spalte (Antworten) und
#'   der zweiten Spalte (absolute Häufigkeit).
#' @param col.width Spaltenbreiten (siehe [lv_table()]). Bei `"default"`
#'   werden `col.width3` bzw. `col.width4` aus [setanalysis_defaults]
#'   verwendet.
#' @param order.table Reihenfolge der Zeilen: `FALSE` (wie in den Daten),
#'   `"decreasing"` (nach Häufigkeit absteigend) oder ein anderer Wert, z. B.
#'   `TRUE` (aufsteigend). „NAs“ und „Total“ bleiben am Ende.
#' @param bold Soll die Kopfzeile fett gedruckt werden?
#' @param digits Anzahl der Nachkommastellen der Prozentangaben.
#'
#' @returns Ein `tinytable`-Objekt (siehe [lv_table()]).
#'
#' @family tabellen
#'
#' @examples
#' table_freq(BspDaten$Tabellen$freq, col1.name = "Überschneidung")
#'
#' table_freq(BspDaten$Tabellen$freq, order.table = "decreasing")
#'
#' @export
table_freq <- function(x,
                       cutoff = FALSE,
                       show.all = TRUE,
                       col1.name = "",
                       col2.name = "n",
                       col.width = "default",
                       order.table = FALSE,
                       bold = TRUE,
                       digits = 1) {
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
    if (cutoff != FALSE && max(x, na.rm = TRUE) == cutoff) {
      freq_table[nrow(freq_table) - 2, 1] <- paste0(cutoff, " oder höher")
    }
  }


  if (order.table != FALSE) {
    decreasing <- order.table == "decreasing"
    freq_table <- freq_table[c(
      order(freq_table[1:(nrow(freq_table) - ncol(freq_table) + 2), 2],
        decreasing = decreasing
      ),
      (nrow(freq_table) - ncol(freq_table) + 3):nrow(freq_table)
    ), ]
  }

  if (col.width[1] == "default" && length(freq_table) == 4) {
    col.width <- setanalysis_defaults$col.width4
  }
  if (col.width[1] == "default" && length(freq_table) == 3) {
    col.width <- setanalysis_defaults$col.width3
  }

  lv_table(freq_table, col.width = col.width, bold = bold, digits = digits)
}

#' Kennwerte einer Frage als Tabelle
#'
#' @description
#' Erstellt eine einzeilige Tabelle mit den Kennwerten einer Variable:
#' Anzahl (n), Mittelwert (M), Standardabweichung (SD), optional Median (MD),
#' Minimum und Maximum. Wird z. B. von [merge_sk()] und [merge_num()]
#' verwendet.
#'
#' @param x Numerischer Vektor.
#' @param md Soll der Median gezeigt werden?
#' @param col1.name Überschrift der Spalte mit der Anzahl.
#' @param bold Soll die Kopfzeile fett gedruckt werden?
#' @param digits Anzahl der Nachkommastellen.
#'
#' @returns Ein `tinytable`-Objekt (siehe [lv_table()]).
#'
#' @family tabellen
#' @seealso [table_stat_multi()] für mehrere Items mit Fragetexten.
#'
#' @examples
#' table_stat_single(BspDaten$dataLVE$KF_01, md = TRUE, col1.name = "n")
#'
#' @export
table_stat_single <- function(x,
                              md = FALSE,
                              col1.name = "N_votes",
                              bold = TRUE,
                              digits = 2) {
  if (md == FALSE) {
    stats_table <- data.frame(round(psych::describe(x), 2))[c(2:4, 8:9)]
    colnames(stats_table) <- c(col1.name, "M", "SD", "Min", "Max")
  } else {
    stats_table <- data.frame(round(psych::describe(x), 2))[c(2:5, 8:9)]
    colnames(stats_table) <- c(col1.name, "M", "SD", "MD", "Min", "Max")
  }

  lv_table(stats_table, col.width = 0.5, bold = bold, digits = digits)
}

#' Kennwerte mehrerer Items als Tabelle
#'
#' @description
#' Erstellt eine Tabelle mit einer Zeile pro Item: Fragetext, Anzahl (n),
#' Mittelwert (M), Standardabweichung (SD), Median (MD), Minimum, Maximum und
#' optional die Häufigkeit von bis zu zwei Ausweichoptionen. Wird z. B. von
#' [merge_aggr_sk()] und [merge_grade()] verwendet.
#'
#' @param x Data Frame mit einer numerischen Spalte pro Item.
#' @param col1.name,col2.name Überschrift der Spalte mit den Fragetexten und
#'   der Spalte mit der Anzahl.
#' @param alt1,alt2 Überschrift der Spalte für die erste bzw. zweite
#'   Ausweichoption oder `FALSE`. `alt2` ist nur zusammen mit `alt1` möglich.
#' @param alt1.list,alt2.list Häufigkeiten der Ausweichoptionen, ein Wert pro
#'   Item.
#' @param digits Anzahl der Nachkommastellen.
#' @param bold,bold.corner Siehe [lv_table()].
#' @param labels Fragetexte der Items. Bei `"labels"` werden die Attribute
#'   `label` der Spalten verwendet.
#'
#' @returns Ein `tinytable`-Objekt (siehe [lv_table()]). Die Spaltenbreiten
#'   kommen aus [setanalysis_defaults] (`col.width.sm`, `col.width.sm.alt1`,
#'   `col.width.sm.alt2`).
#'
#' @family tabellen
#'
#' @examples
#' table_stat_multi(BspDaten$Tabellen$multi[, 1:3], col2.name = "n")
#'
#' @export
table_stat_multi <- function(x,
                             col1.name = "Item",
                             col2.name = "N_votes",
                             alt1 = FALSE,
                             alt2 = FALSE,
                             alt1.list = NULL,
                             alt2.list = NULL,
                             digits = 2,
                             bold = TRUE,
                             bold.corner = TRUE,
                             labels = "labels") {
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
