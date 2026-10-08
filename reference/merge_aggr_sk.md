# Skalenfragen gemeinsam auswerten, optional pro Lehrveranstaltung aggregiert

Erzeugt den Berichtsabschnitt für eine oder mehrere Skalenfragen mit
derselben Skala: eine gemeinsame Tabelle mit Kennwerten je Item (n, M,
SD, Median, Minimum, Maximum, optional Ausweichoptionen) und Boxplots je
Item
([`boxplot_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/boxplot_aggr_sk.md)).

Mit `aggr = TRUE` werden die Antworten zuerst je Lehrveranstaltung
(`kennung`) gemittelt. Jede Lehrveranstaltung geht dann mit ihrem
Mittelwert ein, und `n` zählt Lehrveranstaltungen statt Antworten.

## Usage

``` r
merge_aggr_sk(
  x,
  kennung,
  number = "default",
  alt1 = FALSE,
  alt2 = FALSE,
  alt1.num = 0,
  alt2.num = 7,
  nr = "",
  inkl = "nr",
  tmin = "default",
  tmid = "default",
  tmax = "default",
  show.table = TRUE,
  show.plot = TRUE,
  fig.height = "default",
  col2.name = "n",
  message = "",
  aggr = FALSE
)
```

## Arguments

- x:

  Data Frame mit einer Spalte pro Item (oder ein einzelnes Item als
  Vektor). Jede Spalte braucht die Attribute `label` (Fragetext) und
  `labels` (benannte Antwortcodes).

- kennung:

  Vektor mit der Kennung der Lehrveranstaltung (oder einer anderen
  Gruppe) für jede Zeile von `x`. Nur bei `aggr = TRUE` nötig.

- number:

  Anzahl der Skalenstufen **ohne** Ausweichoptionen. Bei `"default"` die
  Anzahl der Antwortlabels der Items (alle Items müssen gleich viele
  haben). Haben Ausweichoptionen ein eigenes Label, sollte `number`
  angegeben werden.

- alt1, alt2:

  Text der ersten bzw. zweiten Ausweichoption oder `FALSE`. Ist ein Text
  angegeben, zeigt die Tabelle eine zusätzliche Spalte mit der
  Häufigkeit dieser Antwort je Item. `alt2` ist nur zusammen mit `alt1`
  möglich.

- alt1.num, alt2.num:

  Code der ersten bzw. zweiten Ausweichoption in den Daten.

- nr:

  Fragenummer des **ersten** Items, z. B. `"2.1"`. Die folgenden Items
  werden fortlaufend nummeriert (`"2.2"`, `"2.3"`, …), die Nummern
  werden den Fragetexten vorangestellt. Bei `inkl = "nr"` wird jedes
  Item einzeln über seine Variable `inkl.<nr>` ein- oder ausgeschlossen.

- inkl:

  Soll die Frage im Bericht erscheinen? `TRUE` oder `FALSE`. Beim
  Standard `"nr"` entscheidet die Variable `inkl.<nr>` (z. B. `inkl.2.1`
  bei `nr = "2.1"`), die im Berichts-Template gesetzt ist, meist aus der
  Berichtstabelle von
  [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md).
  Ohne Fragenummer wird die Frage immer ausgegeben.

- tmin, tmid, tmax:

  Beschriftung des linken Pols, der Mitte (nur bei ungerader Stufenzahl)
  und des rechten Pols. Bei `"default"` werden die Antwortlabels der
  Items verwendet; unterscheiden sie sich zwischen den Items, gibt es
  eine Warnung.

- show.table:

  Soll die Tabelle gezeigt werden?

- show.plot:

  Sollen die Boxplots gezeigt werden?

- fig.height:

  Höhe der Abbildung in Zoll. Bei `"default"` wird sie aus der Anzahl
  der Items berechnet.

- col2.name:

  Überschrift der Spalte mit der Anzahl (z. B. Anzahl der
  Lehrveranstaltungen bei `aggr = TRUE`).

- message:

  Text, der vor der Tabelle ausgegeben wird (Markdown), oder `""` für
  keinen.

- aggr:

  Sollen die Antworten zuerst je `kennung` gemittelt werden (siehe
  [`aggr_data()`](https://donvollb.github.io/setanalysis/reference/aggr_data.md))?

## Value

Nichts (unsichtbar `NULL`). Der Code für den Bericht wird mit
[`cat()`](https://rdrr.io/r/base/cat.html) ausgegeben.

## See also

[`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md)
für die Auswertung einer einzelnen Skalenfrage mit
Häufigkeitsverteilung.

Weitere Auswertungsfunktionen:
[`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md),
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
[`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md),
[`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md)

## Examples

``` r
kernfragen <- BspDaten$dataLVE[, c("KF_01", "KF_02", "KF_03")]

# Mittelwerte je Lehrveranstaltung
merge_aggr_sk(kernfragen, kennung = BspDaten$dataLVE$Kennung, aggr = TRUE)
#> 
#> +---------------------------------------------------------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+
#> | #text(weight: "bold")[Item] _[Skala: (1)~trifft gar nicht zu - (6)~trifft voll zu]_                                 | **n** | **M** | **SD** | **MD** | **Min** | **Max** |
#> +=====================================================================================================================+=======+=======+========+========+=========+=========+
#> | Didaktische Hilfsmittel (z.B. Folien, Begleitmaterialien) waren für mich hilfreich.                                 | 449   | 4.82  | 0.58   | 4.88   | 2.80    | 6       |
#> +---------------------------------------------------------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+
#> | Die Veranstaltung folgte aus meiner Sicht einer klaren Struktur.                                                    | 449   | 5.01  | 0.52   | 5.00   | 3.19    | 6       |
#> +---------------------------------------------------------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+
#> | Die Veranstaltung war meiner Ansicht nach gut organisiert (z.B. Bereitstellung von Materialien, Informationsfluss). | 449   | 4.94  | 0.54   | 5.00   | 3.00    | 6       |
#> +---------------------------------------------------------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+
#> <div class="figure" style="text-align: center">
#> <img src="figure/sub_chunk_5-1.png" alt="plot of chunk sub_chunk_5" width="100%" />
#> <p class="caption">plot of chunk sub_chunk_5</p>
#> </div>  
#>   

# Alle Antworten, nur Tabelle
merge_aggr_sk(kernfragen, show.plot = FALSE)
#> 
#> +---------------------------------------------------------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+
#> | #text(weight: "bold")[Item] _[Skala: (1)~trifft gar nicht zu - (6)~trifft voll zu]_                                 | **n** | **M** | **SD** | **MD** | **Min** | **Max** |
#> +=====================================================================================================================+=======+=======+========+========+=========+=========+
#> | Didaktische Hilfsmittel (z.B. Folien, Begleitmaterialien) waren für mich hilfreich.                                 | 4420  | 4.85  | 1.10   | 5      | 1       | 6       |
#> +---------------------------------------------------------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+
#> | Die Veranstaltung folgte aus meiner Sicht einer klaren Struktur.                                                    | 4420  | 5.01  | 1.05   | 5      | 1       | 6       |
#> +---------------------------------------------------------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+
#> | Die Veranstaltung war meiner Ansicht nach gut organisiert (z.B. Bereitstellung von Materialien, Informationsfluss). | 4412  | 4.93  | 1.07   | 5      | 1       | 6       |
#> +---------------------------------------------------------------------------------------------------------------------+-------+-------+--------+--------+---------+---------+  
#>   

if (interactive()) {
  markdown_in_viewer(
    merge_aggr_sk(kernfragen, kennung = BspDaten$dataLVE$Kennung, aggr = TRUE)
  )
}
```
