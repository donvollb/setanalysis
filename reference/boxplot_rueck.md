# Boxplot des Rücklaufs

Zeichnet einen waagerechten Boxplot des Rücklaufs in Prozent (Achse von
0 bis 120 %). Wird von
[`merge_rueck()`](https://donvollb.github.io/setanalysis/reference/merge_rueck.md)
verwendet.

## Usage

``` r
boxplot_rueck(x)
```

## Arguments

- x:

  Numerischer Vektor mit dem Rücklauf je Lehrveranstaltung in Prozent.

## Value

Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.

## See also

Weitere Grafikfunktionen:
[`barplot_freq()`](https://donvollb.github.io/setanalysis/reference/barplot_freq.md),
[`barplot_scmc()`](https://donvollb.github.io/setanalysis/reference/barplot_scmc.md),
[`barplot_sk()`](https://donvollb.github.io/setanalysis/reference/barplot_sk.md),
[`boxplot_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/boxplot_aggr_sk.md),
[`boxplot_grade()`](https://donvollb.github.io/setanalysis/reference/boxplot_grade.md),
[`boxplot_wl()`](https://donvollb.github.io/setanalysis/reference/boxplot_wl.md)

## Examples

``` r
boxplot_rueck(BspDaten$Plots$rueck)

```
