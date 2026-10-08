# Rohdaten und Codebuch aus evasys einlesen

Liest den CSV-Export der Rohdaten und das zugehörige Codebuch aus evasys
ein und bereitet die Daten für die Auswertung auf. Jede Variable erhält
die Attribute

- `label`: Fragetext (ohne Fragenummer),

- `nr`: Fragenummer im Fragebogen, z. B. `"2.1"`,

- `type`: Fragetyp, `"sc"` (Single Choice), `"mc"` (Multiple Choice),
  `"sk"` (Skalenfrage) oder `"open/num"` (offene oder numerische Frage),

- `labels`: bei Single-Choice- und Skalenfragen die Antwortcodes mit
  ihren Bezeichnungen.

Außerdem werden das Präfix `[FILTER]` aus Spaltennamen entfernt, die
Spalten von MC-Fragen passend zum Codebuch nummeriert (`frage_1`,
`frage_2`, …) und Platzhalter in offenen Antworten (`""`, `"-"`, `"."`,
`"/"`, `"[Freitextfeld]"`) durch `NA` ersetzt.

## Usage

``` r
evasys_read_data(raw.data.path = NULL, codebook.path = NULL)
```

## Arguments

- raw.data.path:

  Pfad zur CSV-Datei mit den Rohdaten. Ohne Angabe öffnet sich ein
  Dialog zur Dateiauswahl (nur in RStudio).

- codebook.path:

  Pfad zur CSV-Datei mit dem Codebuch. Ohne Angabe öffnet sich ein
  Dialog zur Dateiauswahl (nur in RStudio).

## Value

Data Frame mit den aufbereiteten Daten.

## See also

Weitere Funktionen zur Datenaufbereitung:
[`aggr_data()`](https://donvollb.github.io/setanalysis/reference/aggr_data.md),
[`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md),
[`label_test()`](https://donvollb.github.io/setanalysis/reference/label_test.md)

## Examples

``` r
# Kleiner, fiktiver Export im Format von evasys
rohdaten <- system.file("extdata", "beispiel_evasys_rohdaten.csv", package = "setanalysis")
codebuch <- system.file("extdata", "beispiel_evasys_codebuch.csv", package = "setanalysis")

daten <- evasys_read_data(rohdaten, codebuch)
str(daten$zufrieden)
#>  int [1:5] 5 6 2 4 7
#>  - attr(*, "label")= chr "Ich bin mit dem Studium zufrieden."
#>  - attr(*, "nr")= chr "1.2"
#>  - attr(*, "type")= chr "sk"
#>  - attr(*, "labels")= Named num [1:7] 1 2 3 4 5 6 7
#>   ..- attr(*, "names")= chr [1:7] "trifft gar nicht zu" "" "" "" ...
```
