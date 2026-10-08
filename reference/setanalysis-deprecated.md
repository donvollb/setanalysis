# Veraltete Funktionsnamen

Diese Funktionen sind ältere Schreibweisen aktueller Funktionen. Sie
verhalten sich genau wie die jeweils aktuelle Funktion und werden nur
noch bereitgestellt, damit bestehende Berichtsvorlagen weiter
funktionieren. In neuem Code sollten die aktuellen Namen verwendet
werden.

|  |  |
|----|----|
| Veralteter Name | Aktueller Name |
| `appendix.open()` | [`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md) |
| `boxplot.ruecklauf()` | [`merge_rueck()`](https://donvollb.github.io/setanalysis/reference/merge_rueck.md) |
| `bsp.boxplot()` | [`bsp_boxplot()`](https://donvollb.github.io/setanalysis/reference/bsp_boxplot.md) |
| `bsp.evasys.sk6()` | [`bsp_evasys_sk6()`](https://donvollb.github.io/setanalysis/reference/bsp_evasys_sk6.md) |
| `bsp.table.stat()` | [`bsp_table_stat()`](https://donvollb.github.io/setanalysis/reference/bsp_table_stat.md) |
| `change.analysis.defaults()` | [`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md) |
| `evasys.read.data()` | [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md) |
| `grade()` | [`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md) |
| `input.tabelle()` | [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md) |
| `label.test()` | [`label_test()`](https://donvollb.github.io/setanalysis/reference/label_test.md) |
| `list.open.answers` | [list_open_answers](https://donvollb.github.io/setanalysis/reference/list_open_answers.md) |
| `markdown.in.viewer()` | [`markdown_in_viewer()`](https://donvollb.github.io/setanalysis/reference/markdown_in_viewer.md) |
| `merge.evasys.sk()` | [`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md) |
| `merge.fachsem()` | [`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md) |
| `merge.mc()` | [`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md) |
| `merge.multi.sk()` | [`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md) |
| `merge.num()` | [`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md) |
| `merge.open()` | [`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md) |
| `merge.sc()` | [`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md) |
| `merge.subj()` | [`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md) |
| `merge.wl()` | [`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md) |
| `open.answers()` | [`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md) mit `appendix = TRUE` |
| `table.freq()` | [`table_freq()`](https://donvollb.github.io/setanalysis/reference/table_freq.md) |
| `table.stat.multi()` | [`table_stat_multi()`](https://donvollb.github.io/setanalysis/reference/table_stat_multi.md) |
| `table.stat.single()` | [`table_stat_single()`](https://donvollb.github.io/setanalysis/reference/table_stat_single.md) |

## Value

Siehe die jeweils aktuelle Funktion.

## Details

Die veralteten Namen leiten jeden Aufruf unverändert an die aktuelle
Funktion weiter. Argumente werden dabei genau so zugeordnet wie bei
einem direkten Aufruf der aktuellen Funktion.

Namen wie `merge.sc()` oder `boxplot.ruecklauf()` sehen für R wie
S3-Methoden der generischen Funktionen
[`merge()`](https://rdrr.io/r/base/merge.html) bzw.
[`boxplot()`](https://rdrr.io/r/graphics/boxplot.html) aus. Sie haben
deshalb die Argumente der generischen Funktion, damit sie nicht als
fehlerhafte Methoden gelten.

`list.open.answers` ist ein zweiter Name für dieselbe Umgebung wie
[list_open_answers](https://donvollb.github.io/setanalysis/reference/list_open_answers.md).
