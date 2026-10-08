#' Mehrere Fragen auf einmal auswerten
#'
#' @description
#' Geht die Spalten eines Data Frames der Reihe nach durch und wertet jede
#' Frage mit [merge_auto()] aus. Spalten, die zusammengehören, werden dabei
#' gemeinsam ausgewertet:
#'
#' * Spalten einer MC-Frage (gleiche Fragenummer im Attribut `nr`) mit
#'   [merge_mc()],
#' * aufeinanderfolgende Skalenfragen mit [merge_aggr_sk()] (bei
#'   `multi.sk = TRUE`); eine einzelne Skalenfrage mit [merge_sk()].
#'
#' Alle Spalten brauchen das Attribut `type` (`"sc"`, `"mc"`, `"sk"` oder
#' `"open/num"`), wie es [evasys_read_data()] setzt.
#'
#' @param x Data Frame mit den auszuwertenden Fragen (oder eine einzelne
#'   Spalte).
#' @param multi.sk Sollen aufeinanderfolgende Skalenfragen gemeinsam
#'   ausgewertet werden? Bei `FALSE` wird jede einzeln mit [merge_sk()]
#'   ausgewertet.
#' @inheritParams merge_auto
#'
#' @returns Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
#'   [cat()] ausgegeben.
#'
#' @family auswertung
#'
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' # MC-Frage (8 Spalten), zwei Single-Choice-Fragen, eine offene und eine
#' # numerische Frage
#' merge_many(BspDaten$dataSHOWUP[, 1:12], nr_auto = FALSE)
#' \dontshow{
#' setwd(.old_wd)
#' }
#'
#' @export
merge_many <- function(x,
                       multi.sk = TRUE,
                       nr_auto = TRUE,
                       nr = "",
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
