# Abbildung oder Tabelle als eigenen Chunk ausgeben

In einem Chunk mit `output: asis` haben alle Abbildungen dieselbe Größe.
`subchunkify()` erzeugt deshalb für einen einzelnen Ausdruck (z. B. eine
Abbildung oder eine Tabelle) einen eigenen kleinen Chunk mit eigener
Abbildungsgröße, wertet ihn mit knitr aus und gibt das Ergebnis in den
Bericht aus. Alle Auswertungsfunktionen nutzen `subchunkify()` für ihre
Tabellen und Abbildungen.

## Usage

``` r
subchunkify(g, fig_height = 7, fig_width = 5, hide = FALSE)
```

## Arguments

- g:

  Ausdruck, der eine Abbildung zeichnet oder eine Tabelle erzeugt. Er
  wird erst im Sub-Chunk ausgewertet. Mehrere Befehle können mit
  `c(...)` übergeben werden (dann `hide = TRUE` verwenden).

- fig_height, fig_width:

  Höhe und Breite der Abbildung in Zoll.

- hide:

  Bei `TRUE` wird nur die Abbildung ausgegeben und sonstige Ausgaben des
  Ausdrucks werden unterdrückt (Chunk-Option `results = "hide"`); bei
  `FALSE` wird alles wie in einem `asis`-Chunk ausgegeben.

## Value

Nichts (unsichtbar `NULL`). Das Ergebnis des Sub-Chunks wird mit
[`cat()`](https://rdrr.io/r/base/cat.html) ausgegeben.

## See also

Weitere Werkzeuge:
[`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md),
[`list_open_answers`](https://donvollb.github.io/setanalysis/reference/list_open_answers.md),
[`markdown_in_viewer()`](https://donvollb.github.io/setanalysis/reference/markdown_in_viewer.md),
[`setanalysis_defaults`](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md)

## Examples

``` r
# Tabelle und Abbildung mit eigener Größe
subchunkify(lv_table(head(mtcars, 3)))
#> 
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | **mpg** | **cyl** | **disp** | **hp** | **drat** | **wt** | **qsec** | **vs** | **am** | **gear** | **carb** |
#> +=========+=========+==========+========+==========+========+==========+========+========+==========+==========+
#> | 21.00   | 6       | 160      | 110    | 3.90     | 2.62   | 16.46    | 0      | 1      | 4        | 4        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | 21.00   | 6       | 160      | 110    | 3.90     | 2.88   | 17.02    | 0      | 1      | 4        | 4        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | 22.80   | 4       | 108      | 93     | 3.85     | 2.32   | 18.61    | 1      | 1      | 4        | 1        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
subchunkify(boxplot_grade(BspDaten$Plots$grade), fig_height = 2, fig_width = 9)
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_46-1.png" alt="plot of chunk sub_chunk_46" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_46</p>
#> </div>
```
