#' Frage mit passender Funktion auswerten (Typ wird erkannt)
#'
#' @description
#' Wählt anhand des Attributs `type` (von [evasys_read_data()] gesetzt) die
#' passende Auswertungsfunktion und ruft sie auf:
#'
#' | Typ in `x`                         | Auswertung        |
#' |------------------------------------|-------------------|
#' | `"sc"` (Single Choice)             | [merge_sc()]      |
#' | `"sk"` (Skalenfrage)               | [merge_sk()]      |
#' | `"open/num"` mit überwiegend Text  | [merge_open()]    |
#' | `"open/num"` mit Zahlen            | [merge_num()]     |
#' | Data Frame mit `"mc"`-Spalten      | [merge_mc()]      |
#' | Data Frame mit `"sk"`-Spalten      | [merge_aggr_sk()] |
#'
#' Bei offenen Fragen entscheidet der Inhalt: Zahlen oder Text, der zu mehr
#' als der Hälfte aus Ziffern besteht, gelten als numerisch.
#'
#' @param x Eine Spalte (Vektor) oder ein Data Frame mit den Spalten einer
#'   MC-Frage bzw. mehreren Skalenfragen.
#' @param nr_auto Soll die Fragenummer aus dem Attribut `nr` übernommen
#'   werden, wenn weder `nr` noch `inkl` angegeben sind? Dann wird auch die
#'   Variable `inkl.<nr>` abgefragt, die dafür existieren muss.
#' @param ... Weitere Argumente für die jeweilige Auswertungsfunktion.
#' @inheritParams merge_sc
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#' @seealso [merge_many()] für mehrere Fragen auf einmal.
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' merge_auto(BspDaten$dataLVE$V3_D, nr_auto = FALSE) # Single Choice
#' merge_auto(BspDaten$dataSHOWUP$zugang_note, nr_auto = FALSE) # numerisch
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_auto <- function(x,
                       nr_auto = TRUE,
                       nr = "",
                       inkl = "nr",
                       ...) {
  if (!is.list(x)) {
    type <- attr(x, "type")
  } else {
    type <- paste0("multi.", attr(x[, 1], "type"))
  }

  if (isTRUE(nr_auto && nr == "" && inkl == "nr")) {
    if (type %in% c("multi.mc", "multi.sk")) {
      nr <- attr(x[, 1], "nr")
    } else {
      nr <- attr(x, "nr")
    }
  }


  # Erkennung, ob es sich um offene oder numerische Fragen handelt
  # Zuerst prüfen, ob es überhaupt Buchstaben gibt

  if (type == "open/num" && typeof(x) != "character") {
    type <- "num"
  }

  # Falls es Buchstaben gibt: Schätzung anhand des Anteils der Ziffern
  if (type == "open/num") {
    # NA-Werte entfernen, Vektor in eine lange Zeichenkette umwandeln
    all_text <- paste(na.omit(x), collapse = "")

    # Anzahl der Ziffern in der Zeichenkette zählen
    n_digits <- length(gregexpr("[0-9]", all_text)[[1]])

    # Gesamtlänge der Zeichenkette speichern
    n_chars <- nchar(all_text)

    # Anteil berechnen
    digit_share <- n_digits / n_chars

    if (digit_share > 0.5) {
      type <- "num"
    } else {
      type <- "open"
    }
  }

  merge_functions <- list(
    sc = merge_sc,
    sk = merge_sk,
    open = merge_open,
    num = merge_num,
    multi.mc = merge_mc,
    multi.sk = merge_aggr_sk
  )

  merge_functions[[type]](x, nr = nr, inkl = inkl, ...)
}
