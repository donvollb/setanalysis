# Tabelle im Stil der Berichte formatieren

Wandelt einen Data Frame in eine mit
[`tinytable::tt()`](https://vincentarelbundock.github.io/tinytable/man/tt.html)
formatierte Tabelle im einheitlichen Stil der Berichte um: fette
Kopfzeile, abwechselnd eingefärbte Zeilen (Akzentfarbe `color.bars` aus
[setanalysis_defaults](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md)
mit 10 % Deckkraft), erste Spalte linksbündig, übrige Spalten
rechtsbündig (bei nur einer Zeile zentriert). Spalten mit ganzen Zahlen
werden ohne Nachkommastellen gezeigt.

Sonderzeichen in den Zellen (z. B. `$`, `#`, `_` oder `//` in offenen
Antworten) werden maskiert und erscheinen im Bericht als normaler Text.
Die Kopfzeile wird nicht maskiert, sie darf also Typst-Code enthalten
(z. B. `'#text(weight: "bold")[Item]'`).

Alle anderen Tabellenfunktionen des Pakets nutzen `lv_table()`.

## Usage

``` r
lv_table(
  x,
  col.width = 1,
  bold = TRUE,
  bold.corner = TRUE,
  digits = 2,
  striped = TRUE
)
```

## Arguments

- x:

  Data Frame mit dem Tabelleninhalt.

- col.width:

  Spaltenbreiten, wie bei `width` in
  [`tinytable::tt()`](https://vincentarelbundock.github.io/tinytable/man/tt.html):
  eine Zahl (Breite der ganzen Tabelle als Anteil der Seitenbreite) oder
  ein Vektor mit relativen Breiten je Spalte.

- bold:

  Soll die Kopfzeile fett gedruckt werden?

- bold.corner:

  Soll auch die Zelle oben links fett sein? Bei `FALSE` wird sie normal
  gedruckt (z. B. wenn dort ein Hinweis zur Skala steht).

- digits:

  Anzahl der Nachkommastellen für Zahlen.

- striped:

  Sollen die Zeilen abwechselnd eingefärbt werden?

## Value

Ein `tinytable`-Objekt. Im Bericht (auch über
[`subchunkify()`](https://donvollb.github.io/setanalysis/reference/subchunkify.md))
wird es automatisch im passenden Format ausgegeben.

## See also

Weitere Tabellenfunktionen:
[`table_freq()`](https://donvollb.github.io/setanalysis/reference/table_freq.md),
[`table_stat_multi()`](https://donvollb.github.io/setanalysis/reference/table_stat_multi.md),
[`table_stat_single()`](https://donvollb.github.io/setanalysis/reference/table_stat_single.md)

## Examples

``` r
lv_table(head(mtcars))
#> 
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | **mpg** | **cyl** | **disp** | **hp** | **drat** | **wt** | **qsec** | **vs** | **am** | **gear** | **carb** |
#> +=========+=========+==========+========+==========+========+==========+========+========+==========+==========+
#> | 21.00   | 6       | 160      | 110    | 3.90     | 2.62   | 16.46    | 0      | 1      | 4        | 4        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | 21.00   | 6       | 160      | 110    | 3.90     | 2.88   | 17.02    | 0      | 1      | 4        | 4        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | 22.80   | 4       | 108      | 93     | 3.85     | 2.32   | 18.61    | 1      | 1      | 4        | 1        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | 21.40   | 6       | 258      | 110    | 3.08     | 3.21   | 19.44    | 1      | 0      | 3        | 1        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | 18.70   | 8       | 360      | 175    | 3.15     | 3.44   | 17.02    | 0      | 0      | 3        | 2        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | 18.10   | 6       | 225      | 105    | 2.76     | 3.46   | 20.22    | 1      | 0      | 3        | 1        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+ 

lv_table(head(mtcars), striped = FALSE, digits = 1)
#> 
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | **mpg** | **cyl** | **disp** | **hp** | **drat** | **wt** | **qsec** | **vs** | **am** | **gear** | **carb** |
#> +=========+=========+==========+========+==========+========+==========+========+========+==========+==========+
#> | 21.0    | 6       | 160      | 110    | 3.9      | 2.6    | 16.5     | 0      | 1      | 4        | 4        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | 21.0    | 6       | 160      | 110    | 3.9      | 2.9    | 17.0     | 0      | 1      | 4        | 4        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | 22.8    | 4       | 108      | 93     | 3.9      | 2.3    | 18.6     | 1      | 1      | 4        | 1        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | 21.4    | 6       | 258      | 110    | 3.1      | 3.2    | 19.4     | 1      | 0      | 3        | 1        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | 18.7    | 8       | 360      | 175    | 3.1      | 3.4    | 17.0     | 0      | 0      | 3        | 2        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+
#> | 18.1    | 6       | 225      | 105    | 2.8      | 3.5    | 20.2     | 1      | 0      | 3        | 1        |
#> +---------+---------+----------+--------+----------+--------+----------+--------+--------+----------+----------+ 
```
