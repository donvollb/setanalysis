# Einstellungen des Pakets ändern

Ändert einen oder mehrere Einträge in
[setanalysis_defaults](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md),
z. B. die Akzentfarbe oder ob Abbildungen gezeigt werden. Die Änderung
gilt für alle folgenden Aufrufe in der R-Sitzung, typischerweise also
für den ganzen Bericht. Nur bestehende Einstellungen können geändert
werden.

## Usage

``` r
change_analysis_defaults(...)
```

## Arguments

- ...:

  Einstellungen in der Form `name = wert`, z. B.
  `color.bars = "#507289"` oder `show.plot.sc = FALSE`. Die möglichen
  Namen stehen in
  [setanalysis_defaults](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md).

## Value

Nichts (unsichtbar `NULL`).

## See also

Weitere Werkzeuge:
[`list_open_answers`](https://donvollb.github.io/setanalysis/reference/list_open_answers.md),
[`markdown_in_viewer()`](https://donvollb.github.io/setanalysis/reference/markdown_in_viewer.md),
[`setanalysis_defaults`](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md),
[`subchunkify()`](https://donvollb.github.io/setanalysis/reference/subchunkify.md)

## Examples

``` r
alte_werte <- mget(c("color.bars", "show.plot.sc"), envir = setanalysis_defaults)

# Balken blaugrau, keine Abbildungen bei Single-Choice-Fragen
change_analysis_defaults(color.bars = "#507289", show.plot.sc = FALSE)
setanalysis_defaults$color.bars
#> [1] "#507289"

# Unbekannte Einstellungen führen zu einem Fehler
try(change_analysis_defaults(farbe = "red"))
#> Error in change_analysis_defaults(farbe = "red") : 
#>   Die Einstellungsvariable „farbe“ existiert nicht.

# Vorherige Werte wiederherstellen
do.call(change_analysis_defaults, alte_werte)
```
