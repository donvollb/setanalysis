# Gesamtnote auswerten

Erzeugt den Berichtsabschnitt für eine Gesamtnote im Schulnotenformat (1
= sehr gut bis 6 = ungenügend): eine Tabelle mit Kennwerten und einen
Boxplot
([`boxplot_grade()`](https://donvollb.github.io/setanalysis/reference/boxplot_grade.md)).
Standardmäßig werden die Noten zuerst je Lehrveranstaltung gemittelt,
sodass jede Lehrveranstaltung gleich stark eingeht.

## Usage

``` r
merge_grade(
  x,
  kennung,
  show.table = TRUE,
  already.aggr = FALSE,
  inkl = "nr",
  nr = ""
)
```

## Arguments

- x:

  Vektor mit den Noten. Das Attribut `label` wird als Fragetext in der
  Tabelle verwendet.

- kennung:

  Vektor mit der Kennung der Lehrveranstaltung für jede Note. Nicht
  nötig bei `already.aggr = TRUE`.

- show.table:

  Soll die Tabelle gezeigt werden?

- already.aggr:

  Sind die Noten schon je Lehrveranstaltung gemittelt? Dann wird nicht
  noch einmal aggregiert.

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

[`aggr_data()`](https://donvollb.github.io/setanalysis/reference/aggr_data.md)
für die Aggregierung.

Weitere Auswertungsfunktionen:
[`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md),
[`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md),
[`merge_auto()`](https://donvollb.github.io/setanalysis/reference/merge_auto.md),
[`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md),
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
merge_grade(BspDaten$dataLVE$Note, kennung = BspDaten$dataLVE$Kennung)
#> 
#> +----------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+
#> | #text(weight: "bold")[Item] _[Skala: Schulnoten]_                    | **n** | **M** | **SD** | **MD** | **Min** | **Max** |
#> +======================================================================+=======+=======+========+========+=========+=========+
#> | Welche Gesamtnote (Schulnote) geben Sie der Veranstaltung insgesamt? | 449   | 2.09  | 0.50   | 2      | 1       | 3.78    |
#> +----------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_16-1.png" alt="plot of chunk sub_chunk_16" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_16</p>
#> </div>
```
