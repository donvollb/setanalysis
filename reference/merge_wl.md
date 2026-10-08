# Workload der Lehrveranstaltungen auswerten

Gibt den Fragetext als Überschrift und einen Boxplot des angegebenen
Workloads (Stunden pro Woche) aus
([`boxplot_wl()`](https://donvollb.github.io/setanalysis/reference/boxplot_wl.md)).
Standardmäßig wird zuerst der Median je Lehrveranstaltung gebildet,
sodass jede Lehrveranstaltung gleich stark eingeht.

## Usage

``` r
merge_wl(WL, kennung, already.aggr = FALSE)
```

## Arguments

- WL:

  Vektor mit dem angegebenen Workload in Stunden pro Woche. Erwartet das
  Attribut `label` (Fragetext).

- kennung:

  Vektor mit der Kennung der Lehrveranstaltung für jede Antwort. Nicht
  nötig bei `already.aggr = TRUE`.

- already.aggr:

  Liegt der Workload schon je Lehrveranstaltung vor? Dann wird nicht
  noch einmal aggregiert.

## Value

Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
[`cat()`](https://rdrr.io/r/base/cat.html) ausgegeben.

## See also

Weitere Auswertungsfunktionen:
[`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md),
[`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md),
[`merge_auto()`](https://donvollb.github.io/setanalysis/reference/merge_auto.md),
[`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md),
[`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md),
[`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md),
[`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md),
[`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md),
[`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md),
[`merge_rueck()`](https://donvollb.github.io/setanalysis/reference/merge_rueck.md),
[`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md),
[`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md),
[`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md)

## Examples

``` r
merge_wl(BspDaten$dataLVE$WL, kennung = BspDaten$dataLVE$Kennung)
#> ### Zusätzlich zu Ihren Anwesenheitszeiten in der Veranstaltung: Wie viel Zeit (in Zeitstunden) haben Sie für die vorliegende Veranstaltung im Schnitt pro Woche aufgewendet? (ohne Prüfungsvorbereitung) 
#> 
#> 
#> 
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_44-1.png" alt="plot of chunk sub_chunk_44" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_44</p>
#> </div>
```
