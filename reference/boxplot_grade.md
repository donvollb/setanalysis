# Boxplot der Gesamtnote

Zeichnet einen waagerechten Boxplot auf der Notenskala von „sehr gut“
(1) bis „ungenügend“ (6). Wird von
[`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md)
verwendet.

## Usage

``` r
boxplot_grade(x)
```

## Arguments

- x:

  Numerischer Vektor (oder einspaltiger Data Frame) mit den Noten, meist
  schon je Lehrveranstaltung gemittelt.

## Value

Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.

## See also

Weitere Grafikfunktionen:
[`barplot_freq()`](https://donvollb.github.io/setanalysis/reference/barplot_freq.md),
[`barplot_scmc()`](https://donvollb.github.io/setanalysis/reference/barplot_scmc.md),
[`barplot_sk()`](https://donvollb.github.io/setanalysis/reference/barplot_sk.md),
[`boxplot_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/boxplot_aggr_sk.md),
[`boxplot_rueck()`](https://donvollb.github.io/setanalysis/reference/boxplot_rueck.md),
[`boxplot_wl()`](https://donvollb.github.io/setanalysis/reference/boxplot_wl.md)

## Examples

``` r
boxplot_grade(BspDaten$Plots$grade)

```
