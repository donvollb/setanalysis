# Erstes und zweites Fach gemeinsam auswerten

Fasst zwei Single-Choice-Fragen mit denselben Antwortoptionen zusammen,
typischerweise „Was ist Ihr 1. Fach?“ und „Was ist Ihr 2. Fach?“ im
2-Fach-Bachelor, und wertet sie wie
[`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md)
in einer gemeinsamen Tabelle aus. Ein Hinweis unter der Tabelle erklärt,
dass sich dadurch der Stichprobenumfang verdoppelt.

## Usage

``` r
merge_subj(x1, x2, inkl1 = "nr1", inkl2 = "nr2", nr1 = "", nr2 = "")
```

## Arguments

- x1, x2:

  Antworten auf die Frage nach dem 1. bzw. 2. Fach (wie bei
  [`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md)).
  Überschrift und Antwortoptionen werden aus `x1` genommen.

- inkl1, inkl2:

  Wie `inkl` bei
  [`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md),
  jeweils für eine der beiden Fragen; beim Standard `"nr1"` bzw. `"nr2"`
  entscheiden die Variablen `inkl.<nr1>` bzw. `inkl.<nr2>`. Ausgegeben
  wird nur, wenn beide Fragen eingeschlossen sind.

- nr1, nr2:

  Fragenummern der beiden Fragen; beide erscheinen in der Überschrift.

## Value

Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
[`cat()`](https://rdrr.io/r/base/cat.html) ausgegeben.

## See also

Weitere Auswertungsfunktionen:
[`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md),
[`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md),
[`merge_auto()`](https://donvollb.github.io/setanalysis/reference/merge_auto.md),
[`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md),
[`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md),
[`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md),
[`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md),
[`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md),
[`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md),
[`merge_rueck()`](https://donvollb.github.io/setanalysis/reference/merge_rueck.md),
[`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md),
[`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md),
[`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md)

## Examples

``` r
merge_subj(BspDaten$dataSHOWUP$fach1_2FB, BspDaten$dataSHOWUP$fach2_2FB)
#> ###   &  [ZFB] Der 2-Fach-Bachelor kombiniert 2 Basisfächer: Was ist Ihr 1.Fach / 2. Fach?  
#>  
#> 
#> +---------------------------------+-------+-------+---------------+
#> | **Antwortoption**               | **n** | **%** | **gültige %** |
#> +=================================+=======+=======+===============+
#> | Allgemeine Musterwissenschaft   | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Beispielkunde                   | 3     | 0.6   | 12.5          |
#> +---------------------------------+-------+-------+---------------+
#> | Platzhalterlehre/ Vorlagenkunde | 3     | 0.6   | 12.5          |
#> +---------------------------------+-------+-------+---------------+
#> | Angewandte Fiktion              | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Fantasie-Studien                | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Geographie: Musterlandschaften  | 3     | 0.6   | 12.5          |
#> +---------------------------------+-------+-------+---------------+
#> | Testologie                      | 2     | 0.4   | 8.3           |
#> +---------------------------------+-------+-------+---------------+
#> | Theorie der Beispiele           | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Kunst und Platzhalter           | 4     | 0.8   | 16.7          |
#> +---------------------------------+-------+-------+---------------+
#> | Mathematik                      | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Ökologie                        | 4     | 0.8   | 16.7          |
#> +---------------------------------+-------+-------+---------------+
#> | Philosophie                     | 1     | 0.2   | 4.2           |
#> +---------------------------------+-------+-------+---------------+
#> | Physik                          | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Politikwissenschaft             | 1     | 0.2   | 4.2           |
#> +---------------------------------+-------+-------+---------------+
#> | Soziologie                      | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Sportwissenschaft               | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Umweltchemie                    | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Wirtschaftswissenschaft         | 3     | 0.6   | 12.5          |
#> +---------------------------------+-------+-------+---------------+
#> | NAs                             | 460   | 95.0  | NA            |
#> +---------------------------------+-------+-------+---------------+
#> | Total                           | 484   | 100.0 | 100.0         |
#> +---------------------------------+-------+-------+---------------+
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_43-1.png" alt="plot of chunk sub_chunk_43" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_43</p>
#> </div>
#>  
#> *Hinweis: In der Befragung wurden 1. und 2. Fach getrennt abgefragt; in dieser Tabelle werden die Antworten gemeinsam dargestellt. Daraus ergibt sich in dieser Darstellung eine Verdopplung des Stichprobenumfangs (siehe „Total“).*  
#>   
```
