# Legende: Erklärung der Tabellenspalten

Erstellt eine Tabelle, die die Abkürzungen in den Kopfzeilen der
Statistiktabellen erklärt (n = Häufigkeit, M = Mittelwert usw.). Gedacht
für den Abschnitt „Legende zu Tabellen“ am Anfang eines Berichts.

## Usage

``` r
bsp_table_stat(all = TRUE)
```

## Arguments

- all:

  `TRUE` für die Legende zu Tabellen mit Fragetext und Median (wie bei
  [`table_stat_multi()`](https://donvollb.github.io/setanalysis/reference/table_stat_multi.md), z. B.
  in der LVE), `FALSE` für die kurze Variante ohne beides.

## Value

Ein `tinytable`-Objekt (siehe
[`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md)).

## See also

Weitere Legenden:
[`bsp_boxplot()`](https://donvollb.github.io/setanalysis/reference/bsp_boxplot.md),
[`bsp_evasys_sk6()`](https://donvollb.github.io/setanalysis/reference/bsp_evasys_sk6.md)

## Examples

``` r
bsp_table_stat()
#> 
#> +----------+------------+------------+---------------------+--------+----------------------+--------------------+
#> | **Item** | **n**      | **M**      | **SD**              | **MD** | **Min**              | **Max**            |
#> +==========+============+============+=====================+========+======================+====================+
#> | Frage    | Häufigkeit | Mittelwert | Standard-abweichung | Median | kleinster be⁠ob. Wert | größter be⁠ob. Wert |
#> +----------+------------+------------+---------------------+--------+----------------------+--------------------+ 
bsp_table_stat(all = FALSE)
#> 
#> +------------+------------+---------------------+---------------------+-------------------+
#> | **n**      | **M**      | **SD**              | **Min**             | **Max**           |
#> +============+============+=====================+=====================+===================+
#> | Häufigkeit | Mittelwert | Standard-
#> +------------+------------+---------------------+---------------------+-------------------+
#> abweichung | kleinster
#> +------------+------------+---------------------+---------------------+-------------------+
#> beob. Wert | größter
#> +------------+------------+---------------------+---------------------+-------------------+
#> beob. Wert |
#> +------------+------------+---------------------+---------------------+-------------------+ 
```
