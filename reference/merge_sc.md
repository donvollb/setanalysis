# Single-Choice-Frage auswerten

Erzeugt den Berichtsabschnitt für eine Frage mit genau einer
Antwortmöglichkeit (Single Choice): Überschrift mit dem Fragetext, eine
Häufigkeitstabelle und optional ein Balkendiagramm.

Wie alle Auswertungsfunktionen schreibt `merge_sc()` den Code für den
Bericht direkt in die Ausgabe. Sie wird deshalb in einem Quarto-Chunk
mit `output: asis` aufgerufen.

## Usage

``` r
merge_sc(
  x,
  inkl = "nr",
  nr = "",
  fig.height = "default",
  already.labels = FALSE,
  col2.name = "n",
  order.table = FALSE,
  show.plot = setanalysis_defaults$show.plot.sc,
  pagebreak = FALSE,
  digits = 1
)
```

## Arguments

- x:

  Vektor mit den Antworten (Antwortcodes). Erwartet die Attribute
  `label` (Fragetext) und `labels` (benannte Antwortcodes), wie sie
  [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
  erzeugt.

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

- fig.height:

  Höhe der Abbildung in Zoll. Bei `"default"` wird sie aus der Anzahl
  der Antwortoptionen berechnet.

- already.labels:

  Liegen die Antworten schon als Text bzw. Faktor vor (`TRUE`)?
  Standardmäßig (`FALSE`) werden die Antwortcodes mithilfe des Attributs
  `labels` in Antworttexte übersetzt.

- col2.name:

  Überschrift der Spalte mit den absoluten Häufigkeiten.

- order.table:

  Reihenfolge der Antwortoptionen in der Tabelle: `FALSE` (Reihenfolge
  der Antwortoptionen), `"decreasing"` (nach Häufigkeit absteigend) oder
  ein anderer Wert, z. B. `TRUE` (aufsteigend).

- show.plot:

  Soll ein Balkendiagramm gezeigt werden? Voreinstellung aus
  [setanalysis_defaults](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md)
  (`show.plot.sc`).

- pagebreak:

  Soll nach der Frage ein Seitenumbruch eingefügt werden?

- digits:

  Anzahl der Nachkommastellen der Prozentangaben.

## Value

Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
[`cat()`](https://rdrr.io/r/base/cat.html) ausgegeben.

## See also

[`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md)
für Fragen mit mehreren Antwortmöglichkeiten,
[`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md)
für die automatische Auswertung mehrerer Fragen.

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
[`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md),
[`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md),
[`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md)

## Examples

``` r
# Code für den Bericht (im Quarto-Dokument in einem Chunk mit `output: asis`)
merge_sc(BspDaten$dataLVE$V3_D)
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
#> <img src="figure/sub_chunk_36-1.png" alt="plot of chunk sub_chunk_36" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_36</p>
#> </div>
#>  

# Nach Häufigkeit sortiert, ohne Abbildung
merge_sc(BspDaten$dataLVE$FachSemN, order.table = "decreasing", show.plot = FALSE)
#> ###  Bezogen auf das Fach, dem die vorliegende Veranstaltung zugehört: In welchem Fachsemester sind Sie eingeschrieben? 
#>  
#> 
#> +-------------------+-------+-------+---------------+
#> | **Antwortoption** | **n** | **%** | **gültige %** |
#> +===================+=======+=======+===============+
#> | 02                | 1599  | 35.8  | 36.6          |
#> +-------------------+-------+-------+---------------+
#> | 04                | 1130  | 25.3  | 25.9          |
#> +-------------------+-------+-------+---------------+
#> | 01                | 399   | 8.9   | 9.1           |
#> +-------------------+-------+-------+---------------+
#> | 06                | 395   | 8.9   | 9.0           |
#> +-------------------+-------+-------+---------------+
#> | 03                | 262   | 5.9   | 6.0           |
#> +-------------------+-------+-------+---------------+
#> | 08                | 162   | 3.6   | 3.7           |
#> +-------------------+-------+-------+---------------+
#> | 05                | 133   | 3.0   | 3.0           |
#> +-------------------+-------+-------+---------------+
#> | 10                | 93    | 2.1   | 2.1           |
#> +-------------------+-------+-------+---------------+
#> | 07                | 81    | 1.8   | 1.9           |
#> +-------------------+-------+-------+---------------+
#> | 09                | 39    | 0.9   | 0.9           |
#> +-------------------+-------+-------+---------------+
#> | 11                | 32    | 0.7   | 0.7           |
#> +-------------------+-------+-------+---------------+
#> | 14                | 13    | 0.3   | 0.3           |
#> +-------------------+-------+-------+---------------+
#> | 12                | 11    | 0.2   | 0.3           |
#> +-------------------+-------+-------+---------------+
#> | 13                | 9     | 0.2   | 0.2           |
#> +-------------------+-------+-------+---------------+
#> | 20 und höher      | 5     | 0.1   | 0.1           |
#> +-------------------+-------+-------+---------------+
#> | 15                | 2     | 0.0   | 0.0           |
#> +-------------------+-------+-------+---------------+
#> | 17                | 2     | 0.0   | 0.0           |
#> +-------------------+-------+-------+---------------+
#> | 16                | 1     | 0.0   | 0.0           |
#> +-------------------+-------+-------+---------------+
#> | 18                | 1     | 0.0   | 0.0           |
#> +-------------------+-------+-------+---------------+
#> | 19                | 0     | 0.0   | 0.0           |
#> +-------------------+-------+-------+---------------+
#> | NAs               | 92    | 2.1   | NA            |
#> +-------------------+-------+-------+---------------+
#> | Total             | 4461  | 100.0 | 100.0         |
#> +-------------------+-------+-------+---------------+
#>  

# Vorschau, wie der Abschnitt im Bericht aussieht
if (interactive()) markdown_in_viewer(merge_sc(BspDaten$dataLVE$V3_D))
```
