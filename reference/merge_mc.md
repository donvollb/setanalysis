# Multiple-Choice-Frage auswerten

Erzeugt den Berichtsabschnitt für eine Frage, bei der mehrere Antworten
gewählt werden können (Multiple Choice): Überschrift, Tabelle mit der
Häufigkeit jeder Antwortoption und optional ein Balkendiagramm.

## Usage

``` r
merge_mc(
  x,
  head = "default",
  col1.name = "Antwortoption",
  col2.name = "n",
  show.table = TRUE,
  fig.height = "default",
  inkl = "nr",
  nr = "",
  lime = FALSE,
  filter = FALSE,
  valid.perc = TRUE,
  order.table = FALSE,
  digits = 1,
  show.plot = setanalysis_defaults$show.plot.mc
)
```

## Arguments

- x:

  Data Frame mit einer Spalte pro Antwortoption. Ein Wert ungleich 0
  bedeutet „gewählt“, 0 „nicht gewählt“, `NA` „keine Angabe“. Jede
  Spalte braucht das Attribut `label` in der Form
  `"Fragetext : Antwortoption"` (wie von
  [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
  erzeugt).

- head:

  Fragetext für die Überschrift. Bei `"default"` wird er aus dem Label
  der ersten Spalte genommen (Text vor dem `:`).

- col1.name:

  Überschrift der Spalte mit den Antwortoptionen.

- col2.name:

  Überschrift der Spalte mit den absoluten Häufigkeiten.

- show.table:

  Soll die Tabelle gezeigt werden?

- fig.height:

  Höhe der Abbildung in Zoll. Bei `"default"` wird sie aus der Anzahl
  der Antwortoptionen berechnet.

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

- lime:

  Liegen die Daten im Format eines LimeSurvey-Exports vor (1 = gewählt,
  2 = nicht gewählt; Label `"[Antwortoption] Fragetext"`)?

- filter:

  Nur mit `lime = TRUE`: Text, der dem Fragetext in eckigen Klammern
  vorangestellt wird (z. B. ein Filterhinweis), oder `FALSE`.

- valid.perc:

  Sollen zusätzlich gültige Prozent (ohne fehlende Angaben) sowie die
  Zeilen „NAs“ und „Total“ gezeigt werden?

- order.table:

  Reihenfolge der Antwortoptionen in der Tabelle: `FALSE` (Reihenfolge
  der Antwortoptionen), `"decreasing"` (nach Häufigkeit absteigend) oder
  ein anderer Wert, z. B. `TRUE` (aufsteigend).

- digits:

  Anzahl der Nachkommastellen der Prozentangaben.

- show.plot:

  Soll ein Balkendiagramm gezeigt werden? Voreinstellung aus
  [setanalysis_defaults](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md)
  (`show.plot.mc`).

## Value

Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
[`cat()`](https://rdrr.io/r/base/cat.html) ausgegeben.

## See also

[`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md)
für Fragen mit genau einer Antwortmöglichkeit.

Weitere Auswertungsfunktionen:
[`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md),
[`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md),
[`merge_auto()`](https://donvollb.github.io/setanalysis/reference/merge_auto.md),
[`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md),
[`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md),
[`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md),
[`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md),
[`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md),
[`merge_rueck()`](https://donvollb.github.io/setanalysis/reference/merge_rueck.md),
[`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md),
[`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md),
[`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md),
[`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md)

## Examples

``` r
abschluesse <- BspDaten$dataSHOWUP[, paste0("abschluss_", 1:8)]

merge_mc(abschluesse)
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
#> <img src="figure/sub_chunk_26-1.png" alt="plot of chunk sub_chunk_26" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_26</p>
#> </div>  
#>   

# Nach Häufigkeit sortiert, ohne gültige Prozent und ohne Abbildung
merge_mc(abschluesse, order.table = "decreasing", valid.perc = FALSE, show.plot = FALSE)
#> ###   Welchen Studienabschluss streben Sie an? (Mehrfachnennung möglich)  
#>  
#> 
#> +------------------------------------------+-------------+--------+
#> | **Antwortoption**                        | **N_votes** | **\%** |
#> +==========================================+=============+========+
#> | Bachelor of Arts (B.A.)                  | 21          | 8.7    |
#> +------------------------------------------+-------------+--------+
#> | Bachelor of Education (B.Ed.)            | 71          | 29.3   |
#> +------------------------------------------+-------------+--------+
#> | Bachelor of Science (B.Sc.)              | 52          | 21.5   |
#> +------------------------------------------+-------------+--------+
#> | 2-Fach-Bachelor (B.A., B.Sc.)            | 12          | 5.0    |
#> +------------------------------------------+-------------+--------+
#> | Master of Arts (M.A.)                    | 18          | 7.4    |
#> +------------------------------------------+-------------+--------+
#> | Master of Education (M.Ed.)              | 26          | 10.7   |
#> +------------------------------------------+-------------+--------+
#> | Master of Science (M.Sc.)                | 42          | 17.4   |
#> +------------------------------------------+-------------+--------+
#> | lehramtsbezogener Zertifikatsstudiengang | 0           | 0.0    |
#> +------------------------------------------+-------------+--------+  
#>   

if (interactive()) markdown_in_viewer(merge_mc(abschluesse))
```
