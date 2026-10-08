# Veraltete Funktionsnamen ------------------------------------------------
#
# Die folgenden Namen stammen aus früheren Versionen des Pakets. Sie bleiben
# erhalten, damit bestehende Berichtsvorlagen unverändert weiter funktionieren.
#
# Hinweis: Diese Datei muss nach den Dateien der aktuellen Funktionen geladen
# werden (alphabetische Reihenfolge), weil einige Namen direkt auf diese
# Funktionen verweisen.

#' Veraltete Funktionsnamen
#'
#' @description
#' Diese Funktionen sind ältere Schreibweisen aktueller Funktionen. Sie
#' verhalten sich genau wie die jeweils aktuelle Funktion und werden nur noch
#' bereitgestellt, damit bestehende Berichtsvorlagen weiter funktionieren. In
#' neuem Code sollten die aktuellen Namen verwendet werden.
#'
#' | Veralteter Name        | Aktuelle Funktion          |
#' |------------------------|----------------------------|
#' | `appendix.open()`      | [appendix_open()]          |
#' | `boxplot.ruecklauf()`  | [merge_rueck()]            |
#' | `grade()`              | [merge_grade()]            |
#' | `markdown.in.viewer()` | [markdown_in_viewer()]     |
#' | `merge.evasys.sk()`    | [merge_sk()]               |
#' | `merge.fachsem()`      | [merge_fachsem()]          |
#' | `merge.mc()`           | [merge_mc()]               |
#' | `merge.multi.sk()`     | [merge_aggr_sk()]          |
#' | `merge.num()`          | [merge_num()]              |
#' | `merge.open()`         | [merge_open()]             |
#' | `merge.sc()`           | [merge_sc()]               |
#' | `merge.subj()`         | [merge_subj()]             |
#' | `merge.wl()`           | [merge_wl()]               |
#' | `open.answers()`       | [merge_open()] mit `appendix = TRUE` |
#'
#' @details
#' Namen wie `merge.sc()` oder `boxplot.ruecklauf()` sehen für R wie
#' S3-Methoden der generischen Funktionen [merge()] bzw. [boxplot()] aus. Damit
#' sie nicht als solche behandelt werden, haben sie die Argumente der
#' generischen Funktion und leiten den Aufruf unverändert an die aktuelle
#' Funktion weiter. Argumente werden dabei genau so zugeordnet wie bei einem
#' direkten Aufruf der aktuellen Funktion.
#'
#' @returns Siehe die jeweils aktuelle Funktion.
#'
#' @name setanalysis-deprecated
#' @keywords internal
NULL

# Interne Hilfsfunktion ---------------------------------------------------

# Ruft `fun` mit genau dem Aufruf auf, mit dem die aufrufende (veraltete)
# Funktion aufgerufen wurde, und wertet ihn dort aus, wo er entstanden ist.
# Dadurch werden Argumente so zugeordnet wie bei einem direkten Aufruf.
.weiterleiten <- function(fun) {
  aufruf <- sys.call(-1)
  aufruf[[1]] <- fun
  eval(aufruf, parent.frame(2))
}

# Einfache Aliase ---------------------------------------------------------

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
appendix.open <- appendix_open

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
grade <- merge_grade

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
markdown.in.viewer <- markdown_in_viewer

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export open.answers
open.answers <- function(...) merge_open(..., appendix = TRUE, is_appendix = FALSE)

# Weiterleitungen (Namen, die wie S3-Methoden aussehen) -------------------

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export boxplot.ruecklauf
boxplot.ruecklauf <- function(x, ...) .weiterleiten(merge_rueck)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.evasys.sk
merge.evasys.sk <- function(x, y, ...) .weiterleiten(merge_sk)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.fachsem
merge.fachsem <- function(x, y, ...) .weiterleiten(merge_fachsem)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.mc
merge.mc <- function(x, y, ...) .weiterleiten(merge_mc)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.multi.sk
merge.multi.sk <- function(x, y, ...) .weiterleiten(merge_aggr_sk)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.num
merge.num <- function(x, y, ...) .weiterleiten(merge_num)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.open
merge.open <- function(x, y, ...) .weiterleiten(merge_open)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.sc
merge.sc <- function(x, y, ...) .weiterleiten(merge_sc)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.subj
merge.subj <- function(x, y, ...) .weiterleiten(merge_subj)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.wl
merge.wl <- function(x, y, ...) .weiterleiten(merge_wl)
