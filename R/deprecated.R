# Veraltete Funktionsnamen ------------------------------------------------
#
# Die folgenden Namen stammen aus früheren Versionen des Pakets. Sie bleiben
# erhalten, damit bestehende Berichtsvorlagen unverändert weiter funktionieren.
#
# Alle Namen leiten den Aufruf an die aktuelle Funktion weiter. Die aktuelle
# Funktion wird dabei erst beim Aufruf nachgeschlagen, deshalb spielt die
# Reihenfolge, in der die Dateien geladen werden, keine Rolle.

#' Veraltete Funktionsnamen
#'
#' @description
#' Diese Funktionen sind ältere Schreibweisen aktueller Funktionen. Sie
#' verhalten sich genau wie die jeweils aktuelle Funktion und werden nur noch
#' bereitgestellt, damit bestehende Berichtsvorlagen weiter funktionieren. In
#' neuem Code sollten die aktuellen Namen verwendet werden.
#'
#' | Veralteter Name              | Aktueller Name                       |
#' |------------------------------|--------------------------------------|
#' | `appendix.open()`            | [appendix_open()]                    |
#' | `boxplot.ruecklauf()`        | [merge_rueck()]                      |
#' | `bsp.boxplot()`              | [bsp_boxplot()]                      |
#' | `bsp.evasys.sk6()`           | [bsp_evasys_sk6()]                   |
#' | `bsp.table.stat()`           | [bsp_table_stat()]                   |
#' | `change.analysis.defaults()` | [change_analysis_defaults()]         |
#' | `evasys.read.data()`         | [evasys_read_data()]                 |
#' | `grade()`                    | [merge_grade()]                      |
#' | `input.tabelle()`            | [input_tabelle()]                    |
#' | `label.test()`               | [label_test()]                       |
#' | `list.open.answers`          | [list_open_answers]                  |
#' | `markdown.in.viewer()`       | [markdown_in_viewer()]               |
#' | `merge.evasys.sk()`          | [merge_sk()]                         |
#' | `merge.fachsem()`            | [merge_fachsem()]                    |
#' | `merge.mc()`                 | [merge_mc()]                         |
#' | `merge.multi.sk()`           | [merge_aggr_sk()]                    |
#' | `merge.num()`                | [merge_num()]                        |
#' | `merge.open()`               | [merge_open()]                       |
#' | `merge.sc()`                 | [merge_sc()]                         |
#' | `merge.subj()`               | [merge_subj()]                       |
#' | `merge.wl()`                 | [merge_wl()]                         |
#' | `open.answers()`             | [merge_open()] mit `appendix = TRUE` |
#' | `table.freq()`               | [table_freq()]                       |
#' | `table.stat.multi()`         | [table_stat_multi()]                 |
#' | `table.stat.single()`        | [table_stat_single()]                |
#'
#' @details
#' Die veralteten Namen leiten jeden Aufruf unverändert an die aktuelle
#' Funktion weiter. Argumente werden dabei genau so zugeordnet wie bei einem
#' direkten Aufruf der aktuellen Funktion.
#'
#' Namen wie `merge.sc()` oder `boxplot.ruecklauf()` sehen für R wie
#' S3-Methoden der generischen Funktionen [merge()] bzw. [boxplot()] aus. Sie
#' haben deshalb die Argumente der generischen Funktion, damit sie nicht als
#' fehlerhafte Methoden gelten.
#'
#' `list.open.answers` ist ein zweiter Name für dieselbe Umgebung wie
#' [list_open_answers].
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
.forward_to <- function(fun) {
  original_call <- sys.call(-1)
  original_call[[1]] <- fun
  eval(original_call, parent.frame(2))
}

# Weiterleitungen ---------------------------------------------------------

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
appendix.open <- function(...) .forward_to(appendix_open)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
bsp.boxplot <- function(...) .forward_to(bsp_boxplot)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
bsp.evasys.sk6 <- function(...) .forward_to(bsp_evasys_sk6)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
bsp.table.stat <- function(...) .forward_to(bsp_table_stat)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
change.analysis.defaults <- function(...) .forward_to(change_analysis_defaults)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
evasys.read.data <- function(...) .forward_to(evasys_read_data)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
grade <- function(...) .forward_to(merge_grade)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
input.tabelle <- function(...) .forward_to(input_tabelle)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
label.test <- function(...) .forward_to(label_test)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export
markdown.in.viewer <- function(...) .forward_to(markdown_in_viewer)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export open.answers
open.answers <- function(...) merge_open(..., appendix = TRUE, is_appendix = FALSE)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export table.freq
table.freq <- function(...) .forward_to(table_freq)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export table.stat.multi
table.stat.multi <- function(...) .forward_to(table_stat_multi)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export table.stat.single
table.stat.single <- function(...) .forward_to(table_stat_single)

# Weiterleitungen für Namen, die wie S3-Methoden aussehen -----------------
# (mit den Argumenten der generischen Funktion merge() bzw. boxplot())

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export boxplot.ruecklauf
boxplot.ruecklauf <- function(x, ...) .forward_to(merge_rueck)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.evasys.sk
merge.evasys.sk <- function(x, y, ...) .forward_to(merge_sk)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.fachsem
merge.fachsem <- function(x, y, ...) .forward_to(merge_fachsem)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.mc
merge.mc <- function(x, y, ...) .forward_to(merge_mc)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.multi.sk
merge.multi.sk <- function(x, y, ...) .forward_to(merge_aggr_sk)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.num
merge.num <- function(x, y, ...) .forward_to(merge_num)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.open
merge.open <- function(x, y, ...) .forward_to(merge_open)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.sc
merge.sc <- function(x, y, ...) .forward_to(merge_sc)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.subj
merge.subj <- function(x, y, ...) .forward_to(merge_subj)

#' @rdname setanalysis-deprecated
#' @usage NULL
#' @export merge.wl
merge.wl <- function(x, y, ...) .forward_to(merge_wl)

# Interne Plot-Hilfsfunktion mit Tippfehler im alten Namen ----------------
# (war exportiert und wird ggf. in Berichtsvorlagen verwendet)

#' @noRd
#' @export
.costum_boxplot <- function(...) .forward_to(.custom_boxplot)
