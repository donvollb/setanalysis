# Fachsemester auswerten

Erzeugt den Berichtsabschnitt zur Frage nach dem Fachsemester (LVE):
Überschrift, Häufigkeitstabelle und Balkendiagramm. Alle Semester ab
`cutoff` werden zu einer Kategorie zusammengefasst (z. B. „12+“).

## Usage

``` r
merge_fachsem(
  x,
  fig.height = 5,
  cutoff = 12,
  group = "a",
  inkl = "nr",
  nr = ""
)
```

## Arguments

- x:

  Vektor mit den Fachsemestern (Zahlen).

- fig.height:

  Höhe der Abbildung in Zoll. Beim Standard 5 passen Tabelle und
  Abbildung bei `cutoff = 12` auf eine Seite.

- cutoff:

  Ab diesem Fachsemester werden alle Werte zusammengefasst.

- group:

  Für welche Gruppe gilt die Auswertung? `"a"` (alle), `"b"` (nur
  Bachelor) oder `"m"` (nur Master). Bestimmt Überschrift und
  Tabellenkopf; die Daten müssen vorher entsprechend gefiltert sein.

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

## Value

Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
[`cat()`](https://rdrr.io/r/base/cat.html) ausgegeben.

## See also

[`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md)
für andere numerische Fragen.

Weitere Auswertungsfunktionen:
[`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md),
[`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md),
[`merge_auto()`](https://donvollb.github.io/setanalysis/reference/merge_auto.md),
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
merge_fachsem(BspDaten$dataLVE$FachSemN)
#> ## Fachsemester (alle)  
#>   
#> ### Bezogen auf das Fach, dem die vorliegende Veranstaltung zugehört: in welchem Fachsemester sind Sie eingeschrieben?  
#>   
#> 
#> +-----------------------+-------+-------+---------------+
#> | **Fachsemester alle** | **n** | **%** | **gültige %** |
#> +=======================+=======+=======+===============+
#> | 1                     | 399   | 8.9   | 9.1           |
#> +-----------------------+-------+-------+---------------+
#> | 2                     | 1599  | 35.8  | 36.6          |
#> +-----------------------+-------+-------+---------------+
#> | 3                     | 262   | 5.9   | 6.0           |
#> +-----------------------+-------+-------+---------------+
#> | 4                     | 1130  | 25.3  | 25.9          |
#> +-----------------------+-------+-------+---------------+
#> | 5                     | 133   | 3.0   | 3.0           |
#> +-----------------------+-------+-------+---------------+
#> | 6                     | 395   | 8.9   | 9.0           |
#> +-----------------------+-------+-------+---------------+
#> | 7                     | 81    | 1.8   | 1.9           |
#> +-----------------------+-------+-------+---------------+
#> | 8                     | 162   | 3.6   | 3.7           |
#> +-----------------------+-------+-------+---------------+
#> | 9                     | 39    | 0.9   | 0.9           |
#> +-----------------------+-------+-------+---------------+
#> | 10                    | 93    | 2.1   | 2.1           |
#> +-----------------------+-------+-------+---------------+
#> | 11                    | 32    | 0.7   | 0.7           |
#> +-----------------------+-------+-------+---------------+
#> | 12+                   | 44    | 1.0   | 1.0           |
#> +-----------------------+-------+-------+---------------+
#> | NAs                   | 92    | 2.1   | NA            |
#> +-----------------------+-------+-------+---------------+
#> | Total                 | 4461  | 100.0 | 100.0         |
#> +-----------------------+-------+-------+---------------+  
#>   
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_12-1.png" alt="plot of chunk sub_chunk_12" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_12</p>
#> </div>  
#>   

# Bis zum 8. Semester einzeln, danach zusammengefasst
merge_fachsem(BspDaten$dataLVE$FachSemN, cutoff = 8)
#> ## Fachsemester (alle)  
#>   
#> ### Bezogen auf das Fach, dem die vorliegende Veranstaltung zugehört: in welchem Fachsemester sind Sie eingeschrieben?  
#>   
#> 
#> +-----------------------+-------+-------+---------------+
#> | **Fachsemester alle** | **n** | **%** | **gültige %** |
#> +=======================+=======+=======+===============+
#> | 1                     | 399   | 8.9   | 9.1           |
#> +-----------------------+-------+-------+---------------+
#> | 2                     | 1599  | 35.8  | 36.6          |
#> +-----------------------+-------+-------+---------------+
#> | 3                     | 262   | 5.9   | 6.0           |
#> +-----------------------+-------+-------+---------------+
#> | 4                     | 1130  | 25.3  | 25.9          |
#> +-----------------------+-------+-------+---------------+
#> | 5                     | 133   | 3.0   | 3.0           |
#> +-----------------------+-------+-------+---------------+
#> | 6                     | 395   | 8.9   | 9.0           |
#> +-----------------------+-------+-------+---------------+
#> | 7                     | 81    | 1.8   | 1.9           |
#> +-----------------------+-------+-------+---------------+
#> | 8+                    | 370   | 8.3   | 8.5           |
#> +-----------------------+-------+-------+---------------+
#> | NAs                   | 92    | 2.1   | NA            |
#> +-----------------------+-------+-------+---------------+
#> | Total                 | 4461  | 100.0 | 100.0         |
#> +-----------------------+-------+-------+---------------+  
#>   
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_14-1.png" alt="plot of chunk sub_chunk_14" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_14</p>
#> </div>  
#>   
```
