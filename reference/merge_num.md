# Numerische Frage auswerten

Erzeugt den Berichtsabschnitt für eine Frage mit Zahlen als Antwort (z.
B. Alter oder Abiturnote): Überschrift, Tabelle mit Kennwerten (n, M,
SD, Median, Minimum, Maximum) und ein Balkendiagramm der Häufigkeiten
([`barplot_freq()`](https://donvollb.github.io/setanalysis/reference/barplot_freq.md)).
Für das Diagramm können die Werte in Klassen eingeteilt (`cut.breaks`)
oder ab einem Wert zusammengefasst werden (`cutoff`). Die Tabelle beruht
immer auf den ursprünglichen Werten.

Der Abschnitt wird so ausgegeben, dass Überschrift, Tabelle und
Abbildung nicht durch einen Seitenumbruch getrennt werden. Enthält `x`
nur fehlende Werte, wird nichts ausgegeben.

## Usage

``` r
merge_num(
  x,
  inkl = "nr",
  nr = "",
  xlab = "",
  cut.breaks = "",
  cut.labels = "",
  show.table = TRUE,
  fig.height = 6,
  cutoff = FALSE
)
```

## Arguments

- x:

  Vektor mit den Antworten. Zahlen in Textform mit Dezimalkomma (z. B.
  `"1,7"`) werden umgewandelt. Erwartet das Attribut `label`.

- inkl:

  Soll die Frage im Bericht erscheinen? `TRUE` oder `FALSE`. Beim
  Standard `"nr"` entscheidet die Variable `inkl.<nr>` (z. B. `inkl.2.1`
  bei `nr = "2.1"`), die im Berichts-Template gesetzt ist, meist aus der
  Berichtstabelle von
  [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md).
  Ohne Fragenummer wird die Frage immer ausgegeben.

- nr:

  Fragenummer, z. B. `"2.1"`. Sie wird der Überschrift vorangestellt und
  bestimmt bei `inkl = "nr"`, welche Variable `inkl.<nr>` abgefragt
  wird. Standard: `""` (keine Nummer).

- xlab:

  Beschriftung der x-Achse.

- cut.breaks, cut.labels:

  Grenzen und Beschriftungen der Klassen für das Diagramm (siehe
  [`cut()`](https://rdrr.io/r/base/cut.html), Argumente `breaks` und
  `labels`), oder `""` für keine Klassen.

- show.table:

  Soll die Tabelle gezeigt werden?

- fig.height:

  Höhe der Abbildung in Zoll.

- cutoff:

  Alle Werte ab diesem Wert im Diagramm zu einer Kategorie „`cutoff`+“
  zusammenfassen, oder `FALSE`. Nicht zusammen mit `cut.breaks` möglich.

## Value

Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
[`cat()`](https://rdrr.io/r/base/cat.html) ausgegeben.

## See also

[`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md)
für Fachsemester.

Weitere Auswertungsfunktionen:
[`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md),
[`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md),
[`merge_auto()`](https://donvollb.github.io/setanalysis/reference/merge_auto.md),
[`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md),
[`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md),
[`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md),
[`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md),
[`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md),
[`merge_rueck()`](https://donvollb.github.io/setanalysis/reference/merge_rueck.md),
[`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md),
[`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md),
[`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md),
[`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md)

## Examples

``` r
# Abiturnoten, im Diagramm in Notenbereiche eingeteilt
merge_num(BspDaten$dataSHOWUP$zugang_note,
  xlab = "Durchschnittsnote der Hochschulzugangsberechtigung",
  cut.breaks = c(0, 1.4, 1.9, 2.4, 2.9, 3.4, 4),
  cut.labels = c(
    "1,0 bis 1,4", "1,5 bis 1,9", "2,0 bis 2,4", "2,5 bis 2,9",
    "3,0 bis 3,4", "3,5 bis 4,0"
  )
)
#> ::: {.block breakable=false}
#> 
#> ###     [BACHELOR] Welche Durchschnittsnote hatten Sie in dem Zeugnis, mit dem Sie Ihre Hochschulzugangsberechtigung erworben haben? 
#>  
#> 
#> +-------+-------+--------+--------+---------+---------+
#> | **n** | **M** | **SD** | **MD** | **Min** | **Max** |
#> +=======+=======+========+========+=========+=========+
#> | 147   | 2.19  | 0.56   | 2.10   | 1       | 3.80    |
#> +-------+-------+--------+--------+---------+---------+  
#>   
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_29-1.png" alt="plot of chunk sub_chunk_29" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_29</p>
#> </div>
#> 
#> :::
#> 
```
