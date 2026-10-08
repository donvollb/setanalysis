# Frage mit passender Funktion auswerten (Typ wird erkannt)

Wählt anhand des Attributs `type` (von
[`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
gesetzt) die passende Auswertungsfunktion und ruft sie auf:

|  |  |
|----|----|
| Typ in `x` | Auswertung |
| `"sc"` (Single Choice) | [`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md) |
| `"sk"` (Skalenfrage) | [`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md) |
| `"open/num"` mit überwiegend Text | [`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md) |
| `"open/num"` mit Zahlen | [`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md) |
| Data Frame mit `"mc"`-Spalten | [`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md) |
| Data Frame mit `"sk"`-Spalten | [`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md) |

Bei offenen Fragen entscheidet der Inhalt: Zahlen oder Text, der zu mehr
als der Hälfte aus Ziffern besteht, gelten als numerisch.

## Usage

``` r
merge_auto(x, nr_auto = TRUE, nr = "", inkl = "nr", ...)
```

## Arguments

- x:

  Eine Spalte (Vektor) oder ein Data Frame mit den Spalten einer
  MC-Frage bzw. mehreren Skalenfragen.

- nr_auto:

  Soll die Fragenummer aus dem Attribut `nr` übernommen werden, wenn
  weder `nr` noch `inkl` angegeben sind? Dann wird auch die Variable
  `inkl.<nr>` abgefragt, die dafür existieren muss.

- nr:

  Fragenummer, z. B. `"2.1"`. Sie wird der Überschrift vorangestellt und
  bestimmt bei `inkl = "nr"`, welche Variable `inkl.<nr>` abgefragt
  wird. Standard: `""` (keine Nummer).

- inkl:

  Soll die Frage im Bericht erscheinen? `TRUE` oder `FALSE`. Beim
  Standard `"nr"` entscheidet die Variable `inkl.<nr>` (z. B. `inkl.2.1`
  bei `nr = "2.1"`), die im Berichts-Template gesetzt ist, meist aus der
  Berichtstabelle von
  [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md).
  Ohne Fragenummer wird die Frage immer ausgegeben.

- ...:

  Weitere Argumente für die jeweilige Auswertungsfunktion.

## Value

Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
[`cat()`](https://rdrr.io/r/base/cat.html) ausgegeben.

## See also

[`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md)
für mehrere Fragen auf einmal.

Weitere Auswertungsfunktionen:
[`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md),
[`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md),
[`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md),
[`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md),
[`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md),
[`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md),
[`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md),
[`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md),
[`merge_rueck()`](https://donvollb.github.io/setanalysis/reference/merge_rueck.md),
[`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md),
[`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md),
[`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md),
[`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md)

## Examples

``` r
merge_auto(BspDaten$dataLVE$V3_D, nr_auto = FALSE) # Single Choice
#> ###  Überschneidet sich der Termin dieser Lehrveranstaltung mit anderen laut Studienverlaufsplan (in diesem Semester) vorgesehenen Pflichtveranstaltungen? 
#>  
#> 
#> +-------------------+-------+-------+---------------+
#> | **Antwortoption** | **n** | **%** | **gültige %** |
#> +===================+=======+=======+===============+
#> | ja                | 230   | 5.2   | 5.2           |
#> +-------------------+-------+-------+---------------+
#> | nein              | 4192  | 94.0  | 94.8          |
#> +-------------------+-------+-------+---------------+
#> | NAs               | 39    | 0.9   | NA            |
#> +-------------------+-------+-------+---------------+
#> | Total             | 4461  | 100.0 | 100.0         |
#> +-------------------+-------+-------+---------------+
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_8-1.png" alt="plot of chunk sub_chunk_8" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_8</p>
#> </div>
#>  
merge_auto(BspDaten$dataSHOWUP$zugang_note, nr_auto = FALSE) # numerisch
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
#> <img src="figure/sub_chunk_10-1.png" alt="plot of chunk sub_chunk_10" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_10</p>
#> </div>
#> 
#> ```
#> ## Error in `seq.default()`:
#> ## ! wrong sign in 'by' argument
#> ```
#> 
#> :::
#> 
```
