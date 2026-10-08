# Festlegen, welche Fragen in welchen Bericht kommen

Liest zwei Excel-Tabellen ein und berechnet für jeden Bericht, welche
Fragen er enthält:

- die **Berichtstabelle** („blank“): eine Zeile pro Bericht mit
  Merkmalen wie Studiengang oder Abschluss. Die **erste Zeile muss der
  Master-Bericht** sein, der alle Fragen enthält.

- die **Regeltabelle**: eine Zeile pro Frage bzw. Abschnitt. Die erste
  Spalte enthält den Namen (`inkl.<Abschnitt>.<Frage>`, z. B.
  `inkl.2.1`, oder `header<Abschnitt>`), die zweite die Bedingung.

Bedingungen sind R-Code mit den Spaltennamen der Berichtstabelle, z. B.
`Art != "speziell"` oder `Abschluss == "Bachelor of Science (B.Sc.)"`.
Zusätzlich gibt es `immer TRUE` und `immer FALSE`. Für `header`-Zeilen
wird die Bedingung automatisch erzeugt: Der Abschnitt erscheint, sobald
eine seiner Fragen erscheint (der Text in der Tabelle wird ignoriert).
Die `header`-Zeilen müssen deshalb nach den zugehörigen `inkl`-Zeilen
stehen.

Im Berichts-Template werden die Werte einer Zeile als Variablen
(`inkl.2.1` usw.) gesetzt; die Auswertungsfunktionen fragen sie über ihr
Argument `nr` ab (siehe
[`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md)).

## Usage

``` r
input_tabelle(blank.path = NULL, rules.path = NULL)
```

## Arguments

- blank.path:

  Pfad zur Excel-Datei mit der Berichtstabelle. Ohne Angabe öffnet sich
  ein Dialog zur Dateiauswahl (nur in RStudio).

- rules.path:

  Pfad zur Excel-Datei mit der Regeltabelle. Ohne Angabe öffnet sich ein
  Dialog zur Dateiauswahl (nur in RStudio).

## Value

Die Berichtstabelle (Tibble) mit einer zusätzlichen logischen Spalte pro
Regel. In der ersten Zeile (Master-Bericht) sind alle diese Spalten
`TRUE`.

## See also

Weitere Funktionen zur Datenaufbereitung:
[`aggr_data()`](https://donvollb.github.io/setanalysis/reference/aggr_data.md),
[`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md),
[`label_test()`](https://donvollb.github.io/setanalysis/reference/label_test.md)

## Examples

``` r
# Fiktive Beispieltabellen mit sechs Berichten
berichte <- system.file("extdata", "beispiel_berichte.xlsx", package = "setanalysis")
regeln <- system.file("extdata", "beispiel_regeln.xlsx", package = "setanalysis")

tabelle <- input_tabelle(berichte, regeln)
tabelle[, c("Code", "inkl.1.2", "inkl.2.1", "header2", "inkl.3.1")]
#> # A tibble: 6 × 5
#>   Code                     inkl.1.2 inkl.2.1 header2 inkl.3.1
#>   <chr>                    <lgl>    <lgl>    <lgl>   <lgl>   
#> 1 MASTER                   TRUE     TRUE     TRUE    TRUE    
#> 2 Gesamtbericht            TRUE     FALSE    FALSE   FALSE   
#> 3 B.Sc._Musterwissenschaft TRUE     TRUE     TRUE    FALSE   
#> 4 M.Sc._Musterwissenschaft TRUE     TRUE     TRUE    FALSE   
#> 5 B.A._Beispielkunde       TRUE     FALSE    FALSE   FALSE   
#> 6 Sonderauswertung         FALSE    FALSE    FALSE   TRUE    
```
