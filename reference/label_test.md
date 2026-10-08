# Schreibweisen in Berichtstabelle und Daten abgleichen

Prüft, ob alle Einträge einer Spalte der Berichtstabelle (z. B. die
Namen der Fachbereiche, nach denen die Daten je Bericht gefiltert
werden) in genau dieser Schreibweise auch in den Daten vorkommen. So
fallen Tippfehler auf, bevor ein Bericht versehentlich leer bleibt.

## Usage

``` r
label_test(col, var, exception = "alle")
```

## Arguments

- col:

  Spalte der Berichtstabelle, z. B. `info$FB.txt`.

- var:

  Passende Variable im Datensatz. Bei einem Faktor werden seine Stufen
  verwendet, sonst die vorkommenden Werte.

- exception:

  Einträge von `col`, die nicht geprüft werden (Standard:
  `"alle"`, z. B. für einen Gesamtbericht).

## Value

Die Meldung als Text (unsichtbar); sie wird außerdem ausgegeben. Bei
Abweichungen gibt es eine Meldung pro nicht gefundenem Eintrag.

## See also

Weitere Funktionen zur Datenaufbereitung:
[`aggr_data()`](https://donvollb.github.io/setanalysis/reference/aggr_data.md),
[`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md),
[`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md)

## Examples

``` r
# Alles stimmt
label_test(BspDaten$pInfo$FB.txt, BspDaten$dataLVE$Teilbereich)
#> [1] "Alle Einträge der Spalte aus der Berichtstabelle kommen in gleicher Schreibweise auch in der Variable vor."

# Ein Eintrag mit Tippfehler
label_test(BspDaten$pInfo$FB.txt.falsch, BspDaten$dataLVE$Teilbereich)
#> [1] "Der Eintrag \"Angewandte Fktion - SoSe24\" aus der Berichtstabelle kommt nicht in gleicher Schreibweise in der Variable vor."
```
