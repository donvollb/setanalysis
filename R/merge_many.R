#' Funktion für die automatische Auswertung mehrerer verschiedener Items unterschiedlicher Typen
#'
#' @param x Ausschnitt aus dem Datensatz, der ausgewertet werden soll
#' @param multi.sk Sollen aufeinanderfolgende sk-Items gemeinsam ausgewertet werden?
#' @param nr_auto Soll die Nummer automatisch ermittelt werden?
#' @param nr Manuelle Eingabemöglichkeit der Nummer
#' @param inkl TRUE oder FALSE, ob die Funktion ausgeführt wird; "nr" zieht sich automatisch die entsprechende inkl. Variable
#'
#' @export merge_many

merge_many <- function(x, # Ausschnitt aus dem Datensatz
                       multi.sk = TRUE, # Sollen aufeinanderfolgende sk-Items gemeinsam ausgewertet werden?
                       nr_auto = TRUE, # Soll die Nummer automatisch ermittelt werden?
                       nr = "", #
                       inkl = "nr") {
  # Nur eine Spalte: direkt auswerten -------------------------------------
  if (!is.list(x)) {
    return(merge_auto(x, nr_auto = nr_auto, nr = nr, inkl = inkl))
  }

  evaluate <- function(columns) merge_auto(columns, nr_auto = nr_auto, nr = nr, inkl = inkl)
  type_of <- function(k) attr(x[, k], "type")
  nr_of <- function(k) attr(x[, k], "nr")

  # Spalten Frage für Frage auswerten -------------------------------------
  # `first` und `last` sind die erste und letzte Spalte der aktuellen Frage

  first <- 1
  while (first <= ncol(x)) {
    type <- type_of(first)
    .check_type(type, names(x)[[first]])
    last <- first

    if (type == "mc") {
      # Alle folgenden Spalten mit derselben Fragenummer gehören zur MC-Frage
      while (last < ncol(x) && identical(nr_of(last + 1), nr_of(first))) {
        last <- last + 1
      }
      evaluate(x[, first:last, drop = FALSE])
    } else if (type == "sk" && !isFALSE(multi.sk)) {
      # Aufeinanderfolgende Skalenfragen gemeinsam auswerten
      while (last < ncol(x) && identical(type_of(last + 1), "sk")) {
        last <- last + 1
      }
      if (last > first) evaluate(x[, first:last]) else evaluate(x[, first])
    } else {
      evaluate(x[, first])
    }

    first <- last + 1
  }
}

# Prüfen, ob merge_many() den Typ einer Spalte auswerten kann
.check_type <- function(type, column_name) {
  if (is.null(type)) {
    stop(
      "Die Spalte „", column_name,
      "“ hat keinen Typ (Typen sind z. B. „sc“, „open“, „sk“).",
      call. = FALSE
    )
  }
  if (!type %in% c("sc", "mc", "sk", "open/num")) {
    stop(
      "Die Spalte „", column_name,
      "“ hat den nicht unterstützten Typ „", type, "“.",
      call. = FALSE
    )
  }
}
