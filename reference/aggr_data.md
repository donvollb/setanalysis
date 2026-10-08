# Antworten je Gruppe mitteln

Berechnet für jede Gruppe (z. B. jede Lehrveranstaltung) den Mittelwert
jeder Variable. So geht später jede Gruppe gleich stark in Tabellen und
Abbildungen ein, unabhängig davon, wie viele Antworten sie hat. Wird von
[`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md)
und
[`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md)
verwendet.

## Usage

``` r
aggr_data(vars, kennung)
```

## Arguments

- vars:

  Data Frame mit den zu mittelnden Variablen oder ein einzelner Vektor.
  Werte werden in Zahlen umgewandelt, fehlende Werte ignoriert.

- kennung:

  Vektor mit der Gruppenzugehörigkeit (z. B. Kennung der
  Lehrveranstaltung) für jede Zeile von `vars`.

## Value

Data Frame mit einer Zeile pro Gruppe (in der Reihenfolge, in der die
Gruppen in `kennung` zuerst vorkommen) und einer Spalte pro Variable.
Die Attribute `label` der Variablen bleiben erhalten.

## See also

Weitere Funktionen zur Datenaufbereitung:
[`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md),
[`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md),
[`label_test()`](https://donvollb.github.io/setanalysis/reference/label_test.md)

## Examples

``` r
lve <- BspDaten$dataLVE
mittelwerte <- aggr_data(lve[, c("KF_01", "KF_02")], lve$Kennung)
head(mittelwerte)
#>      KF_01    KF_02
#> 1 5.833333 5.666667
#> 2 5.100000 5.500000
#> 3 4.727273 5.272727
#> 4 4.571429 5.000000
#> 5 5.222222 5.000000
#> 6 5.687500 5.352941
```
