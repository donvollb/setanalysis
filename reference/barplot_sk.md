# Verteilung einer Skalenfrage mit Mittelwert und Standardabweichung

Zeichnet die Antworten einer Skalenfrage im Stil der evasys-Berichte:
ein Balken pro Skalenstufe mit dem Prozentwert darüber, die Beschriftung
der beiden Pole links und rechts sowie eine Markierung für Mittelwert
und Standardabweichung. Wird von
[`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md)
verwendet (Abbildungsgröße dort: 9 × 2 Zoll).

## Usage

``` r
barplot_sk(x, tmin, tmax, number = 6)
```

## Arguments

- x:

  Numerischer Vektor mit den Antworten. Werte außerhalb von `1:number`
  (z. B. Ausweichoptionen) werden nicht berücksichtigt.

- tmin, tmax:

  Beschriftung des linken bzw. rechten Pols.

- number:

  Anzahl der Skalenstufen.

## Value

Kein verwertbarer Wert (unsichtbar); die Abbildung wird gezeichnet.

## See also

[`bsp_evasys_sk6()`](https://donvollb.github.io/setanalysis/reference/bsp_evasys_sk6.md)
für eine beschriftete Legende zu dieser Abbildung.

Weitere Grafikfunktionen:
[`barplot_freq()`](https://donvollb.github.io/setanalysis/reference/barplot_freq.md),
[`barplot_scmc()`](https://donvollb.github.io/setanalysis/reference/barplot_scmc.md),
[`boxplot_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/boxplot_aggr_sk.md),
[`boxplot_grade()`](https://donvollb.github.io/setanalysis/reference/boxplot_grade.md),
[`boxplot_rueck()`](https://donvollb.github.io/setanalysis/reference/boxplot_rueck.md),
[`boxplot_wl()`](https://donvollb.github.io/setanalysis/reference/boxplot_wl.md)

## Examples

``` r
barplot_sk(BspDaten$dataSHOWUP$info_ausr_studgang,
  tmin = "stimme gar nicht zu", tmax = "stimme voll zu"
)

```
