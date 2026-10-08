# Einstellungen des Pakets

Umgebung mit den Einstellungen, die die Auswertungsfunktionen verwenden
(Farben, Spaltenbreiten und Voreinstellungen). Die Werte lassen sich mit
[`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md)
für einen Bericht ändern, z. B. eine eigene Akzentfarbe je Befragung.

## Usage

``` r
setanalysis_defaults
```

## Format

Eine Umgebung mit folgenden Einträgen:

- `font.family`:

  Schriftart der Abbildungen (`"Red Hat Text"`; wird beim Laden des
  Pakets registriert).

- `color.bars`:

  Akzentfarbe für Balken, Boxen und die eingefärbten Tabellenzeilen.

- `col.width3`, `col.width4`:

  Relative Spaltenbreiten der Häufigkeitstabellen mit drei bzw. vier
  Spalten
  ([`table_freq()`](https://donvollb.github.io/setanalysis/reference/table_freq.md),
  [`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md)).

- `col.width.sm`, `col.width.sm.alt1`, `col.width.sm.alt2`:

  Relative Spaltenbreiten der Statistiktabellen für mehrere Items ohne
  bzw. mit einer oder zwei Ausweichoptionen
  ([`table_stat_multi()`](https://donvollb.github.io/setanalysis/reference/table_stat_multi.md)).

- `col1.width.tss`:

  Wird derzeit nicht verwendet (aus früheren Versionen).

- `show.plot.sc`, `show.plot.mc`, `show.plot.sk`:

  Voreinstellung für `show.plot` in
  [`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md),
  [`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md)
  und
  [`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md).

- `open.appendix`:

  Voreinstellung für `appendix` in
  [`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md):
  offene Antworten im Anhang sammeln?

- `inkl.open`:

  Voreinstellung für `inkl_global` in
  [`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md):
  offene Fragen überhaupt ausgeben?

## See also

Weitere Werkzeuge:
[`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md),
[`list_open_answers`](https://donvollb.github.io/setanalysis/reference/list_open_answers.md),
[`markdown_in_viewer()`](https://donvollb.github.io/setanalysis/reference/markdown_in_viewer.md),
[`subchunkify()`](https://donvollb.github.io/setanalysis/reference/subchunkify.md)

## Examples

``` r
setanalysis_defaults$color.bars
#> [1] "#6DACDC"
ls(setanalysis_defaults)
#>  [1] "col.width.sm"      "col.width.sm.alt1" "col.width.sm.alt2"
#>  [4] "col.width3"        "col.width4"        "col1.width.tss"   
#>  [7] "color.bars"        "font.family"       "inkl.open"        
#> [10] "open.appendix"     "show.plot.mc"      "show.plot.sc"     
#> [13] "show.plot.sk"     
```
