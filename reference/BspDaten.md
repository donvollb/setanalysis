# Beispieldaten

Fiktive Daten einer Lehrveranstaltungsevaluation (LVE) und einer
Studieneingangsbefragung (SHOWUP), aufbereitet wie mit
[`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md):
Jede Frage trägt die Attribute `label` (Fragetext), `nr` (Fragenummer),
`type` (Fragetyp) und ggf. `labels` (Antwortcodes). Die Daten werden in
den Beispielen und Tests des Pakets verwendet.

Alle Antworten sind zufällig erzeugt, Fachbereiche und Fächer sind
erfunden und offene Antworten bestehen aus Lorem ipsum. Das Skript dazu
liegt im Quell-Repository unter `data-raw/BspDaten.R`.

## Usage

``` r
BspDaten
```

## Format

Eine Liste mit fünf Elementen:

- `dataLVE`:

  Data Frame mit 4461 Antworten aus 449 Lehrveranstaltungen in vier
  Fachbereichen, u. a. `Teilbereich` (Fachbereich), `Kennung` (Kennung
  der Lehrveranstaltung), `Teilnehmer` (angemeldete Teilnehmende),
  `FachSemN` (Fachsemester), `KF_01` bis `KF_03` (Kernfragen, 6-stufige
  Skala), `Note` (Gesamtnote 1–6), `V3_D` (Single Choice ja/nein) und
  `WL` (Workload in Stunden pro Woche).

- `dataSHOWUP`:

  Data Frame mit 242 Antworten, u. a. `abschluss_1` bis `abschluss_8`
  (Multiple Choice), `fach1_2FB` und `fach2_2FB` (1. und 2. Fach im
  2-Fach-Bachelor), `offen` (offene Frage), `zugang_note`
  (Durchschnittsnote) und `info_ausr_studgang` (6-stufige Skala, Code 0
  = „kann ich nicht beurteilen“).

- `pInfo`:

  Beispiel einer Berichtstabelle mit vier Berichten (je Fachbereich).
  Die Spalte `FB.txt.falsch` enthält absichtlich einen Tippfehler zum
  Ausprobieren von
  [`label_test()`](https://donvollb.github.io/setanalysis/reference/label_test.md).

- `Plots`:

  Vorbereitete Eingaben für die Grafikfunktionen, aus `dataLVE` und
  `dataSHOWUP` berechnet wie in den Auswertungsfunktionen: `aggr.data`
  (Mittelwerte von 15 Items je Lehrveranstaltung) mit `aggr.labels` und
  `aggr.skala`, `grade` (Gesamtnote), `rueck` (Rücklauf in Prozent) und
  `WL` (Workload) je Lehrveranstaltung, `num` (Durchschnittsnote in
  Klassen), `fsem` (Fachsemester), `sc` und `mc` (Häufigkeitstabellen).

- `Tabellen`:

  Vorbereitete Eingaben für die Tabellenfunktionen: `multi` (Mittelwerte
  von 15 Items je Lehrveranstaltung) und `freq` (Faktor ja/nein).

## Examples

``` r
str(BspDaten, max.level = 1)
#> List of 5
#>  $ pInfo     :'data.frame':  4 obs. of  6 variables:
#>  $ dataLVE   :'data.frame':  4461 obs. of  10 variables:
#>  $ dataSHOWUP:'data.frame':  242 obs. of  13 variables:
#>  $ Tabellen  :List of 2
#>  $ Plots     :List of 10

# Fragetext und Antwortcodes einer Frage
attr(BspDaten$dataLVE$KF_01, "label")
#> [1] "Didaktische Hilfsmittel (z.B. Folien, Begleitmaterialien) waren für mich hilfreich."
attr(BspDaten$dataLVE$KF_01, "labels")
#> trifft gar nicht zu                                                             
#>                   1                   2                   3                   4 
#>                          trifft voll zu 
#>                   5                   6 
```
