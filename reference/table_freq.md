# Häufigkeitstabelle erstellen

Erstellt eine Häufigkeitstabelle mit absoluter Häufigkeit, Prozent und
der Zeile „Total“. Gibt es fehlende Werte, kommen die Zeile „NAs“ und
die Spalte „gültige %“ (Prozent ohne fehlende Werte) hinzu. Wird z. B.
von
[`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md)
und
[`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md)
verwendet.

## Usage

``` r
table_freq(
  x,
  cutoff = FALSE,
  show.all = TRUE,
  col1.name = "",
  col2.name = "n",
  col.width = "default",
  order.table = FALSE,
  bold = TRUE,
  digits = 1
)
```

## Arguments

- x:

  Vektor oder Faktor mit den Antworten.

- cutoff:

  Ist der größte Wert in `x` gleich `cutoff`, wird er als „`cutoff` oder
  höher“ beschriftet (für vorher zusammengefasste Werte), sonst `FALSE`.

- show.all:

  Sollen auch Antwortoptionen gezeigt werden, die niemand gewählt hat?

- col1.name, col2.name:

  Überschrift der ersten Spalte (Antworten) und der zweiten Spalte
  (absolute Häufigkeit).

- col.width:

  Spaltenbreiten (siehe
  [`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md)).
  Bei `"default"` werden `col.width3` bzw. `col.width4` aus
  [setanalysis_defaults](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md)
  verwendet.

- order.table:

  Reihenfolge der Zeilen: `FALSE` (wie in den Daten), `"decreasing"`
  (nach Häufigkeit absteigend) oder ein anderer Wert, z. B. `TRUE`
  (aufsteigend). „NAs“ und „Total“ bleiben am Ende.

- bold:

  Soll die Kopfzeile fett gedruckt werden?

- digits:

  Anzahl der Nachkommastellen der Prozentangaben.

## Value

Ein `tinytable`-Objekt (siehe
[`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md)).

## See also

Weitere Tabellenfunktionen:
[`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md),
[`table_stat_multi()`](https://donvollb.github.io/setanalysis/reference/table_stat_multi.md),
[`table_stat_single()`](https://donvollb.github.io/setanalysis/reference/table_stat_single.md)

## Examples

``` r
table_freq(BspDaten$Tabellen$freq, col1.name = "Überschneidung")
#> 
#> +--------------------+-------+-------+---------------+
#> | **Überschneidung** | **n** | **%** | **gültige %** |
#> +====================+=======+=======+===============+
#> | ja                 | 230   | 5.2   | 5.2           |
#> +--------------------+-------+-------+---------------+
#> | nein               | 4192  | 94.0  | 94.8          |
#> +--------------------+-------+-------+---------------+
#> | NAs                | 39    | 0.9   | NA            |
#> +--------------------+-------+-------+---------------+
#> | Total              | 4461  | 100.0 | 100.0         |
#> +--------------------+-------+-------+---------------+ 

table_freq(BspDaten$Tabellen$freq, order.table = "decreasing")
#> 
#> +-------+-------+-------+---------------+
#> |       | **n** | **%** | **gültige %** |
#> +=======+=======+=======+===============+
#> | nein  | 4192  | 94.0  | 94.8          |
#> +-------+-------+-------+---------------+
#> | ja    | 230   | 5.2   | 5.2           |
#> +-------+-------+-------+---------------+
#> | NAs   | 39    | 0.9   | NA            |
#> +-------+-------+-------+---------------+
#> | Total | 4461  | 100.0 | 100.0         |
#> +-------+-------+-------+---------------+ 
```
