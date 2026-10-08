# Waagerechtes Balkendiagramm für Single- und Multiple-Choice-Fragen

Zeichnet für jede Antwortoption einen waagerechten Balken mit der
absoluten Häufigkeit; rechts daneben steht der Prozentwert. Wird von
[`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md)
und
[`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md)
verwendet. Wurde keine Option gewählt, wird statt der Abbildung ein
Hinweis ausgegeben.

## Usage

``` r
barplot_scmc(x, xlab = "")
```

## Arguments

- x:

  Data Frame mit den Spalten `label` (Antwortoption, ggf. mit
  Zeilenumbrüchen), `freq` (absolute Häufigkeit) und `perc` (Prozent),
  eine Zeile pro Antwortoption.

- xlab:

  Beschriftung der x-Achse.

## Value

Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.

## See also

Weitere Grafikfunktionen:
[`barplot_freq()`](https://donvollb.github.io/setanalysis/reference/barplot_freq.md),
[`barplot_sk()`](https://donvollb.github.io/setanalysis/reference/barplot_sk.md),
[`boxplot_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/boxplot_aggr_sk.md),
[`boxplot_grade()`](https://donvollb.github.io/setanalysis/reference/boxplot_grade.md),
[`boxplot_rueck()`](https://donvollb.github.io/setanalysis/reference/boxplot_rueck.md),
[`boxplot_wl()`](https://donvollb.github.io/setanalysis/reference/boxplot_wl.md)

## Examples

``` r
barplot_scmc(BspDaten$Plots$mc, xlab = "Häufigkeit")

```
