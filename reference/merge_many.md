# Mehrere Fragen auf einmal auswerten

Geht die Spalten eines Data Frames der Reihe nach durch und wertet jede
Frage mit
[`merge_auto()`](https://donvollb.github.io/setanalysis/reference/merge_auto.md)
aus. Spalten, die zusammengehören, werden dabei gemeinsam ausgewertet:

- Spalten einer MC-Frage (gleiche Fragenummer im Attribut `nr`) mit
  [`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md),

- aufeinanderfolgende Skalenfragen mit
  [`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md)
  (bei `multi.sk = TRUE`); eine einzelne Skalenfrage mit
  [`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md).

Alle Spalten brauchen das Attribut `type` (`"sc"`, `"mc"`, `"sk"` oder
`"open/num"`), wie es
[`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
setzt.

## Usage

``` r
merge_many(x, multi.sk = TRUE, nr_auto = TRUE, nr = "", inkl = "nr")
```

## Arguments

- x:

  Data Frame mit den auszuwertenden Fragen (oder eine einzelne Spalte).

- multi.sk:

  Sollen aufeinanderfolgende Skalenfragen gemeinsam ausgewertet werden?
  Bei `FALSE` wird jede einzeln mit
  [`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md)
  ausgewertet.

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
# MC-Frage (8 Spalten), zwei Single-Choice-Fragen, eine offene und eine
# numerische Frage
merge_many(BspDaten$dataSHOWUP[, 1:12], nr_auto = FALSE)
#> ###   Welchen Studienabschluss streben Sie an? (Mehrfachnennung möglich)  
#>  
#> 
#> +------------------------------------------+-------+-------+---------------+
#> | **Antwortoption**                        | **n** | **%** | **gültige %** |
#> +==========================================+=======+=======+===============+
#> | Bachelor of Arts (B.A.)                  | 21    | 8.7   | 8.7           |
#> +------------------------------------------+-------+-------+---------------+
#> | Bachelor of Education (B.Ed.)            | 71    | 29.3  | 29.3          |
#> +------------------------------------------+-------+-------+---------------+
#> | Bachelor of Science (B.Sc.)              | 52    | 21.5  | 21.5          |
#> +------------------------------------------+-------+-------+---------------+
#> | 2-Fach-Bachelor (B.A., B.Sc.)            | 12    | 5.0   | 5.0           |
#> +------------------------------------------+-------+-------+---------------+
#> | Master of Arts (M.A.)                    | 18    | 7.4   | 7.4           |
#> +------------------------------------------+-------+-------+---------------+
#> | Master of Education (M.Ed.)              | 26    | 10.7  | 10.7          |
#> +------------------------------------------+-------+-------+---------------+
#> | Master of Science (M.Sc.)                | 42    | 17.4  | 17.4          |
#> +------------------------------------------+-------+-------+---------------+
#> | lehramtsbezogener Zertifikatsstudiengang | 0     | 0.0   | 0.0           |
#> +------------------------------------------+-------+-------+---------------+
#> | NAs                                      | 0     | 0.0   | NA            |
#> +------------------------------------------+-------+-------+---------------+
#> | Total                                    | 242   | NA    | NA            |
#> +------------------------------------------+-------+-------+---------------+
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_18-1.png" alt="plot of chunk sub_chunk_18" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_18</p>
#> </div>  
#>   
#> ###  [ZFB] Der 2-Fach-Bachelor kombiniert 2 Basisfächer: Was ist Ihr 1.Fach? 
#>  
#> 
#> +---------------------------------+-------+-------+---------------+
#> | **Antwortoption**               | **n** | **%** | **gültige %** |
#> +=================================+=======+=======+===============+
#> | Allgemeine Musterwissenschaft   | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Beispielkunde                   | 3     | 1.2   | 25.0          |
#> +---------------------------------+-------+-------+---------------+
#> | Platzhalterlehre/ Vorlagenkunde | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Angewandte Fiktion              | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Fantasie-Studien                | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Geographie: Musterlandschaften  | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Testologie                      | 2     | 0.8   | 16.7          |
#> +---------------------------------+-------+-------+---------------+
#> | Theorie der Beispiele           | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Kunst und Platzhalter           | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Mathematik                      | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Ökologie                        | 4     | 1.7   | 33.3          |
#> +---------------------------------+-------+-------+---------------+
#> | Philosophie                     | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Physik                          | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Politikwissenschaft             | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Soziologie                      | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Sportwissenschaft               | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Umweltchemie                    | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Wirtschaftswissenschaft         | 3     | 1.2   | 25.0          |
#> +---------------------------------+-------+-------+---------------+
#> | NAs                             | 230   | 95.0  | NA            |
#> +---------------------------------+-------+-------+---------------+
#> | Total                           | 242   | 100.0 | 100.0         |
#> +---------------------------------+-------+-------+---------------+
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_20-1.png" alt="plot of chunk sub_chunk_20" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_20</p>
#> </div>
#>  
#> ###  [ZFB] Der 2-Fach-Bachelor kombiniert 2 Basisfächer: Was ist Ihr 2.Fach? 
#>  
#> 
#> +---------------------------------+-------+-------+---------------+
#> | **Antwortoption**               | **n** | **%** | **gültige %** |
#> +=================================+=======+=======+===============+
#> | Allgemeine Musterwissenschaft   | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Beispielkunde                   | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Platzhalterlehre/ Vorlagenkunde | 3     | 1.2   | 25.0          |
#> +---------------------------------+-------+-------+---------------+
#> | Angewandte Fiktion              | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Fantasie-Studien                | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Geographie: Musterlandschaften  | 3     | 1.2   | 25.0          |
#> +---------------------------------+-------+-------+---------------+
#> | Testologie                      | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Theorie der Beispiele           | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Kunst und Platzhalter           | 4     | 1.7   | 33.3          |
#> +---------------------------------+-------+-------+---------------+
#> | Mathematik                      | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Ökologie                        | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Philosophie                     | 1     | 0.4   | 8.3           |
#> +---------------------------------+-------+-------+---------------+
#> | Physik                          | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Politikwissenschaft             | 1     | 0.4   | 8.3           |
#> +---------------------------------+-------+-------+---------------+
#> | Soziologie                      | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Sportwissenschaft               | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Umweltchemie                    | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | Wirtschaftswissenschaft         | 0     | 0.0   | 0.0           |
#> +---------------------------------+-------+-------+---------------+
#> | NAs                             | 230   | 95.0  | NA            |
#> +---------------------------------+-------+-------+---------------+
#> | Total                           | 242   | 100.0 | 100.0         |
#> +---------------------------------+-------+-------+---------------+
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_22-1.png" alt="plot of chunk sub_chunk_22" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_22</p>
#> </div>
#>  
#> ###  [FILTER_123] Welche weiteren Informationen hätten Sie gerne gehabt, damit Sie sich vor Beginn Ihres Studiums ausreichend über den Studiengang informiert gefühlt hätten? {#sec-1.top} 
#> 
#> *Die offenen Antworten zu dieser Frage finden sich* [im Anhang](#sec-1.bottom).  
#> 
#> \
#> 
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
#> <img src="figure/sub_chunk_24-1.png" alt="plot of chunk sub_chunk_24" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_24</p>
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
