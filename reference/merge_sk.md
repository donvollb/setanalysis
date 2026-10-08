# Skalenfrage auswerten (Einzelantworten)

Erzeugt den Berichtsabschnitt für eine einzelne Skalenfrage (z. B. eine
6-stufige Likert-Skala): Überschrift, Tabelle mit Kennwerten (n, M, SD,
Median, Minimum, Maximum), optional die Häufigkeit von Ausweichoptionen
sowie eine Abbildung mit der Verteilung der Antworten, Mittelwert und
Standardabweichung
([`barplot_sk()`](https://donvollb.github.io/setanalysis/reference/barplot_sk.md)).

Der Abschnitt wird so ausgegeben, dass Überschrift, Tabelle und
Abbildung nicht durch einen Seitenumbruch getrennt werden.

## Usage

``` r
merge_sk(
  x,
  inkl = "nr",
  nr = "",
  show.alt = TRUE,
  number = 6,
  alt1 = FALSE,
  alt2 = FALSE,
  alt1.num = 0,
  alt2.num = 7,
  lime = FALSE,
  lime.brackets = FALSE,
  show.plot = setanalysis_defaults$show.plot.sk
)
```

## Arguments

- x:

  Vektor mit den Antworten (Zahlen). Erwartet die Attribute `label`
  (Fragetext) und `labels` (benannte Antwortcodes; das erste und das
  `number`-te Label beschriften die Pole der Abbildung).

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

- show.alt:

  Sollen Ausweichoptionen und die Abbildung gezeigt werden? Bei `FALSE`
  werden nur Überschrift und Tabelle ausgegeben.

- number:

  Anzahl der Skalenstufen **ohne** Ausweichoptionen. Werte außerhalb von
  `1:number` gehen nicht in Tabelle und Abbildung ein.

- alt1, alt2:

  Text der ersten bzw. zweiten Ausweichoption (z. B.
  `"kann ich nicht beurteilen"`) oder `FALSE`, wenn es sie nicht gibt.
  Ausgegeben wird, wie oft sie gewählt wurde.

- alt1.num, alt2.num:

  Code der ersten bzw. zweiten Ausweichoption in den Daten.

- lime:

  Liegen die Daten im Format eines LimeSurvey-Exports vor (Faktor mit
  den Antworttexten als Stufen)?

- lime.brackets:

  Nur mit `lime = TRUE`: Steht der eigentliche Fragetext in eckigen
  Klammern am Anfang des Labels? Dann wird nur dieser Teil verwendet.

- show.plot:

  Soll die Abbildung gezeigt werden? Voreinstellung aus
  [setanalysis_defaults](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md)
  (`show.plot.sk`).

## Value

Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
[`cat()`](https://rdrr.io/r/base/cat.html) ausgegeben.

## See also

[`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md),
um mehrere Skalenfragen gemeinsam oder auf Ebene von Lehrveranstaltungen
aggregiert darzustellen.

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
[`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md),
[`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md)

## Examples

``` r
merge_sk(BspDaten$dataSHOWUP$info_ausr_studgang)
#> ::: {.block breakable=false}
#> 
#> ###  Vor Beginn meines Studiums war ich ausreichend über den Studiengang informiert. 
#>  
#>   
#>   
#> 
#> +-------+-------+--------+--------+---------+---------+
#> | **n** | **M** | **SD** | **MD** | **Min** | **Max** |
#> +=======+=======+========+========+=========+=========+
#> | 239   | 4.37  | 1.32   | 5      | 1       | 6       |
#> +-------+-------+--------+--------+---------+---------+  
#>   
#>   
#>  
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_39-1.png" alt="plot of chunk sub_chunk_39" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_39</p>
#> </div>
#> 
#> :::
#> 

# Mit Ausweichoption (Code 0 in den Daten)
merge_sk(BspDaten$dataSHOWUP$info_ausr_studgang, alt1 = "kann ich nicht beurteilen")
#> ::: {.block breakable=false}
#> 
#> ###  Vor Beginn meines Studiums war ich ausreichend über den Studiengang informiert. 
#>  
#>   
#>   
#> 
#> +-------+-------+--------+--------+---------+---------+
#> | **n** | **M** | **SD** | **MD** | **Min** | **Max** |
#> +=======+=======+========+========+=========+=========+
#> | 239   | 4.37  | 1.32   | 5      | 1       | 6       |
#> +-------+-------+--------+--------+---------+---------+  
#>   
#> Die Ausweichoption *kann ich nicht beurteilen* wurde 2 mal gewählt. 
#> 
#>   
#>  
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_41-1.png" alt="plot of chunk sub_chunk_41" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_41</p>
#> </div>
#> 
#> :::
#> 

if (interactive()) markdown_in_viewer(merge_sk(BspDaten$dataSHOWUP$info_ausr_studgang))
```
