# Balkendiagramm der Häufigkeiten

Zeichnet ein senkrechtes Balkendiagramm mit der Häufigkeit jeder
Kategorie, z. B. für Fachsemester oder in Klassen eingeteilte Noten.
Wird von
[`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md)
und
[`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md)
verwendet.

## Usage

``` r
barplot_freq(x, xlab = "")
```

## Arguments

- x:

  Faktor (die Stufen bestimmen Reihenfolge und Beschriftung der Balken).

- xlab:

  Beschriftung der x-Achse.

## Value

Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.

## See also

Weitere Grafikfunktionen:
[`barplot_scmc()`](https://donvollb.github.io/setanalysis/reference/barplot_scmc.md),
[`barplot_sk()`](https://donvollb.github.io/setanalysis/reference/barplot_sk.md),
[`boxplot_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/boxplot_aggr_sk.md),
[`boxplot_grade()`](https://donvollb.github.io/setanalysis/reference/boxplot_grade.md),
[`boxplot_rueck()`](https://donvollb.github.io/setanalysis/reference/boxplot_rueck.md),
[`boxplot_wl()`](https://donvollb.github.io/setanalysis/reference/boxplot_wl.md)

## Examples

``` r
barplot_freq(BspDaten$Plots$fsem, xlab = "Fachsemester")

```
