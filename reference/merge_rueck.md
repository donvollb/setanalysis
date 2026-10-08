# Rücklauf der Lehrveranstaltungen auswerten

Berechnet für jede Lehrveranstaltung den Rücklauf (Anzahl der Antworten
geteilt durch die Zahl der angemeldeten Teilnehmenden, in Prozent) und
gibt eine Tabelle mit Kennwerten sowie einen Boxplot
([`boxplot_rueck()`](https://donvollb.github.io/setanalysis/reference/boxplot_rueck.md))
aus.

Da die Teilnehmendenzahl meist bei der Anmeldung zur Evaluation
angegeben wird, kann der Rücklauf über 100 % liegen.

## Usage

``` r
merge_rueck(x, kennung)
```

## Arguments

- x:

  Vektor mit der angegebenen Teilnehmendenzahl der jeweiligen
  Lehrveranstaltung (in jeder Zeile einer Lehrveranstaltung derselbe
  Wert).

- kennung:

  Vektor mit der Kennung der Lehrveranstaltung für jede Zeile.

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
[`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md),
[`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md),
[`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md),
[`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md)

## Examples

``` r
merge_rueck(BspDaten$dataLVE$Teilnehmer, BspDaten$dataLVE$Kennung)
#> 
#> +-------+-------+--------+---------+---------+
#> | **n** | **M** | **SD** | **Min** | **Max** |
#> +=======+=======+========+=========+=========+
#> | 449   | 33.9  | 44.3   | 3.1     | 600     |
#> +-------+-------+--------+---------+---------+  
#>   
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_34-1.png" alt="plot of chunk sub_chunk_34" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_34</p>
#> </div>
```
