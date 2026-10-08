# Vorschau eines Berichtsabschnitts im Viewer

Zeigt das Ergebnis einer Auswertungsfunktion so an, wie es ungefähr im
Bericht aussehen wird: Die Ausgabe wird in HTML umgewandelt und im
RStudio-Viewer (bzw. im Browser) geöffnet. Praktisch zum Ausprobieren
und Testen; im Bericht selbst wird die Funktion nicht verwendet.

Benötigt die zusätzlichen Pakete htmltools, markdown und svglite.

## Usage

``` r
markdown_in_viewer(markdown_function)
```

## Arguments

- markdown_function:

  Aufruf einer Funktion, die Berichtscode ausgibt, z. B.
  `merge_sc(daten$frage)`.

## Value

Unsichtbar der Pfad zur erzeugten HTML-Datei; die Vorschau wird im
Viewer geöffnet.

## See also

Weitere Werkzeuge:
[`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md),
[`list_open_answers`](https://donvollb.github.io/setanalysis/reference/list_open_answers.md),
[`setanalysis_defaults`](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md),
[`subchunkify()`](https://donvollb.github.io/setanalysis/reference/subchunkify.md)

## Examples

``` r
if (interactive()) {
  markdown_in_viewer(merge_fachsem(BspDaten$dataLVE$FachSemN))
}
```
