# Boxplot des Workloads

Zeichnet einen waagerechten Boxplot des angegebenen Workloads in Stunden
pro Woche; unter der Achse steht die Anzahl der Lehrveranstaltungen.
Wird von
[`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md)
verwendet.

## Usage

``` r
boxplot_wl(
  x,
  skala = c("0h", "1h", "2h", "3h", "4h", "5h", "6h", "7h", "8h", "9h", "10h", "11h",
    "12h", "mehr\nals 12h")
)
```

## Arguments

- x:

  Numerischer Vektor mit dem Workload je Lehrveranstaltung (Stunden pro
  Woche).

- skala:

  Beschriftungen der x-Achse, eine pro Stufe ab 0 Stunden.

## Value

Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.

## See also

Weitere Grafikfunktionen:
[`barplot_freq()`](https://donvollb.github.io/setanalysis/reference/barplot_freq.md),
[`barplot_scmc()`](https://donvollb.github.io/setanalysis/reference/barplot_scmc.md),
[`barplot_sk()`](https://donvollb.github.io/setanalysis/reference/barplot_sk.md),
[`boxplot_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/boxplot_aggr_sk.md),
[`boxplot_grade()`](https://donvollb.github.io/setanalysis/reference/boxplot_grade.md),
[`boxplot_rueck()`](https://donvollb.github.io/setanalysis/reference/boxplot_rueck.md)

## Examples

``` r
boxplot_wl(BspDaten$Plots$WL)

```
