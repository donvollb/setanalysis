#' Funktion, die den automatisch Itemtyp erkennt und entsprechend auswertet
#'
#' @param x auszuwertende Daten
#' @param nr_auto Soll die Nummer automatisch ermittelt werden?
#' @param nr Manuelle Eingabemöglichkeit der Nummer
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#' @param ... Argumente zum „weitergeben“ in die Funktion
#'
#' @export merge_auto

merge_auto <- function(x,
                       nr_auto = TRUE,
                       nr = "",
                       inkl = "nr",
                       ...) { # Argumente zum „weitergeben“ in die Funktion


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
