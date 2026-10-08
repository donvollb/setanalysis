# Boxplots für mehrere Skalenfragen

Zeichnet für jedes Item einen waagerechten Boxplot auf einer gemeinsamen
Skala, beschriftet mit den Fragetexten. Wird von
[`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md)
verwendet. Bei einer 5-stufigen Skala erscheint unter der Abbildung ein
Hinweis, dass die Skalenlogik von den 6-stufigen Skalen abweicht.

## Usage

``` r
boxplot_aggr_sk(x, item_labels, skala)
```

## Arguments

- x:

  Data Frame mit einer numerischen Spalte pro Item.

- item_labels:

  Beschriftungen der Items (y-Achse), gleiche Reihenfolge wie die
  Spalten von `x`.

- skala:

  Beschriftungen der Skalenstufen (x-Achse), z. B.
  `c("trifft gar nicht zu", "", "", "", "", "trifft voll zu")`. Die
  Länge bestimmt die Anzahl der Stufen.

## Value

Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.

## See also

Weitere Grafikfunktionen:
[`barplot_freq()`](https://donvollb.github.io/setanalysis/reference/barplot_freq.md),
[`barplot_scmc()`](https://donvollb.github.io/setanalysis/reference/barplot_scmc.md),
[`barplot_sk()`](https://donvollb.github.io/setanalysis/reference/barplot_sk.md),
[`boxplot_grade()`](https://donvollb.github.io/setanalysis/reference/boxplot_grade.md),
[`boxplot_rueck()`](https://donvollb.github.io/setanalysis/reference/boxplot_rueck.md),
[`boxplot_wl()`](https://donvollb.github.io/setanalysis/reference/boxplot_wl.md)

## Examples

``` r
boxplot_aggr_sk(
  BspDaten$Plots$aggr.data[, 1:4],
  BspDaten$Plots$aggr.labels[1:4],
  BspDaten$Plots$aggr.skala
)

```
