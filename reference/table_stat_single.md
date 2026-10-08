# Kennwerte einer Frage als Tabelle

Erstellt eine einzeilige Tabelle mit den Kennwerten einer Variable:
Anzahl (n), Mittelwert (M), Standardabweichung (SD), optional Median
(MD), Minimum und Maximum. Wird z. B. von
[`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md)
und
[`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md)
verwendet.

## Usage

``` r
table_stat_single(
  x,
  md = FALSE,
  col1.name = "N_votes",
  bold = TRUE,
  digits = 2
)
```

## Arguments

- x:

  Numerischer Vektor.

- md:

  Soll der Median gezeigt werden?

- col1.name:

  Überschrift der Spalte mit der Anzahl.

- bold:

  Soll die Kopfzeile fett gedruckt werden?

- digits:

  Anzahl der Nachkommastellen.

## Value

Ein `tinytable`-Objekt (siehe
[`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md)).

## See also

[`table_stat_multi()`](https://donvollb.github.io/setanalysis/reference/table_stat_multi.md)
für mehrere Items mit Fragetexten.

Weitere Tabellenfunktionen:
[`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md),
[`table_freq()`](https://donvollb.github.io/setanalysis/reference/table_freq.md),
[`table_stat_multi()`](https://donvollb.github.io/setanalysis/reference/table_stat_multi.md)

## Examples

``` r
table_stat_single(BspDaten$dataLVE$KF_01, md = TRUE, col1.name = "n")
#> 
#> +-------+-------+--------+--------+---------+---------+
#> | **n** | **M** | **SD** | **MD** | **Min** | **Max** |
#> +=======+=======+========+========+=========+=========+
#> | 4420  | 4.85  | 1.10   | 5      | 1       | 6       |
#> +-------+-------+--------+--------+---------+---------+ 
```
