# Legende: beschriftete Beispiel-Abbildung einer 6er-Skala

Gibt eine Beispiel-Abbildung im Stil von
[`barplot_sk()`](https://donvollb.github.io/setanalysis/reference/barplot_sk.md)
aus, in der die prozentuale Häufigkeit, der Mittelwert und die
Standardabweichung beschriftet sind. Gedacht für den Abschnitt
„Erläuterung zu Grafiken“ am Anfang eines Berichts.

## Usage

``` r
bsp_evasys_sk6(x = "default")
```

## Arguments

- x:

  Antworten auf einer 6-stufigen Skala. Bei `"default"` werden feste
  Beispielwerte verwendet, zu denen die Beschriftungen passen.

## Value

Nichts (unsichtbar `NULL`). Die Abbildung wird als Sub-Chunk in den
Bericht ausgegeben (siehe
[`subchunkify()`](https://donvollb.github.io/setanalysis/reference/subchunkify.md)).

## See also

Weitere Legenden:
[`bsp_boxplot()`](https://donvollb.github.io/setanalysis/reference/bsp_boxplot.md),
[`bsp_table_stat()`](https://donvollb.github.io/setanalysis/reference/bsp_table_stat.md)

## Examples

``` r
bsp_evasys_sk6()
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_3-1.png" alt="plot of chunk sub_chunk_3" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_3</p>
#> </div>
```
