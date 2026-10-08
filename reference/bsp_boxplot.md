# Legende: beschrifteter Beispiel-Boxplot

Gibt einen Beispiel-Boxplot aus, an dem Ausreißer, Median und Maximum
beschriftet sind. Gedacht für den Abschnitt „Erläuterung zu Grafiken“ am
Anfang eines Berichts.

## Usage

``` r
bsp_boxplot(x = "default")
```

## Arguments

- x:

  Werte für den Boxplot. Bei `"default"` werden feste Beispielwerte
  verwendet, zu denen die Beschriftungen passen.

## Value

Nichts (unsichtbar `NULL`). Die Abbildung wird als Sub-Chunk in den
Bericht ausgegeben (siehe
[`subchunkify()`](https://donvollb.github.io/setanalysis/reference/subchunkify.md)).

## See also

Weitere Legenden:
[`bsp_evasys_sk6()`](https://donvollb.github.io/setanalysis/reference/bsp_evasys_sk6.md),
[`bsp_table_stat()`](https://donvollb.github.io/setanalysis/reference/bsp_table_stat.md)

## Examples

``` r
bsp_boxplot()
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_2-1.png" alt="plot of chunk sub_chunk_2" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_2</p>
#> </div>  
#>   
```
