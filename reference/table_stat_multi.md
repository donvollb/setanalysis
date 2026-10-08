# Kennwerte mehrerer Items als Tabelle

Erstellt eine Tabelle mit einer Zeile pro Item: Fragetext, Anzahl (n),
Mittelwert (M), Standardabweichung (SD), Median (MD), Minimum, Maximum
und optional die Häufigkeit von bis zu zwei Ausweichoptionen. Wird z. B.
von
[`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md)
und
[`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md)
verwendet.

## Usage

``` r
table_stat_multi(
  x,
  col1.name = "Item",
  col2.name = "N_votes",
  alt1 = FALSE,
  alt2 = FALSE,
  alt1.list = NULL,
  alt2.list = NULL,
  digits = 2,
  bold = TRUE,
  bold.corner = TRUE,
  labels = "labels"
)
```

## Arguments

- x:

  Data Frame mit einer numerischen Spalte pro Item.

- col1.name, col2.name:

  Überschrift der Spalte mit den Fragetexten und der Spalte mit der
  Anzahl.

- alt1, alt2:

  Überschrift der Spalte für die erste bzw. zweite Ausweichoption oder
  `FALSE`. `alt2` ist nur zusammen mit `alt1` möglich.

- alt1.list, alt2.list:

  Häufigkeiten der Ausweichoptionen, ein Wert pro Item.

- digits:

  Anzahl der Nachkommastellen.

- bold, bold.corner:

  Siehe
  [`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md).

- labels:

  Fragetexte der Items. Bei `"labels"` werden die Attribute `label` der
  Spalten verwendet.

## Value

Ein `tinytable`-Objekt (siehe
[`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md)).
Die Spaltenbreiten kommen aus
[setanalysis_defaults](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md)
(`col.width.sm`, `col.width.sm.alt1`, `col.width.sm.alt2`).

## See also

Weitere Tabellenfunktionen:
[`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md),
[`table_freq()`](https://donvollb.github.io/setanalysis/reference/table_freq.md),
[`table_stat_single()`](https://donvollb.github.io/setanalysis/reference/table_stat_single.md)

## Examples

``` r
table_stat_multi(BspDaten$Tabellen$multi[, 1:3], col2.name = "n")
#> 
#> +---------------------------------------------------------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+
#> | **Item**                                                                                                            | **n** | **M** | **SD** | **MD** | **Min** | **Max** |
#> +=====================================================================================================================+=======+=======+========+========+=========+=========+
#> | Didaktische Hilfsmittel (z.B. Folien, Begleitmaterialien) waren für mich hilfreich.                                 | 109   | 5.02  | 0.54   | 5.14   | 3.00    | 6       |
#> +---------------------------------------------------------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+
#> | Die Veranstaltung folgte aus meiner Sicht einer klaren Struktur.                                                    | 109   | 5.12  | 0.52   | 5.25   | 4.00    | 6       |
#> +---------------------------------------------------------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+
#> | Die Veranstaltung war meiner Ansicht nach gut organisiert (z.B. Bereitstellung von Materialien, Informationsfluss). | 109   | 5.04  | 0.56   | 5.08   | 3.33    | 6       |
#> +---------------------------------------------------------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+ 
```
