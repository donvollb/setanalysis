# Speicher für die offenen Antworten des Anhangs

Umgebung, in der
[`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md)
bei `appendix = TRUE` die offenen Fragen eines Berichts sammelt, bis
[`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md)
sie im Anhang ausgibt und den Speicher wieder leert. Für die normale
Nutzung muss man sie nicht direkt ansprechen.

## Usage

``` r
list_open_answers
```

## Format

Eine Umgebung mit dem Zähler `anchor.nr` (Anzahl der gesammelten Fragen)
sowie den Einträgen `var.1`, `nr.1`, `var.2`, `nr.2`, … mit den
Antworten und Fragenummern.

## See also

Weitere Werkzeuge:
[`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md),
[`markdown_in_viewer()`](https://donvollb.github.io/setanalysis/reference/markdown_in_viewer.md),
[`setanalysis_defaults`](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md),
[`subchunkify()`](https://donvollb.github.io/setanalysis/reference/subchunkify.md)
