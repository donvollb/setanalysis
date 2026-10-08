# setanalysis

**Deutsch** ·
[English](https://donvollb.github.io/setanalysis/README.en.md) ·
[Website](https://donvollb.github.io/setanalysis/)

[![R-CMD-check](https://github.com/donvollb/setanalysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/donvollb/setanalysis/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/license-MIT-yellow.svg)](https://donvollb.github.io/setanalysis/LICENSE.md)

R-Paket für personalisierte Ergebnisberichte von Befragungen,
insbesondere Lehrveranstaltungsevaluationen sowie Befragungen von
Studierenden und Absolvent\*innen. Eine Zeile Code pro Frage erzeugt
Überschrift, Tabelle und Abbildung für einen
[Quarto](https://quarto.org)-Bericht (PDF über Typst). Über eine
Berichts- und eine Regeltabelle entstehen aus einer einzigen Vorlage
viele Berichte mit unterschiedlichem Inhalt, z. B. für einzelne
Fachbereiche oder Studiengänge.

![Skalenfragen: Tabelle mit Kennwerten und Boxplots je Item, darunter
die Gesamtnote](reference/figures/vorschau-1.png)![Multiple-Choice-Frage
mit Tabelle und Balkendiagramm, darunter eine Skalenfrage mit
Verteilung](reference/figures/vorschau-2.png)

## Funktionen

- **Daten aus evasys einlesen:**
  [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
  liest Rohdaten und Codebuch und hängt jeder Variable Fragetext,
  Fragenummer, Fragetyp und Antwortcodes als Attribute an.
- **Eine Funktion pro Fragetyp:** Single Choice, Multiple Choice,
  Skalen, mehrere Skalenfragen gemeinsam, Zahlen, Fachsemester, Noten,
  Workload, Rücklauf und offene Fragen – jeweils mit einheitlich
  gestalteten Tabellen und Grafiken.
  [`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md)
  wertet ganze Fragenblöcke auf einmal aus.
- **Viele Berichte aus einer Vorlage:**
  [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md)
  berechnet aus einer Berichts- und einer Regeltabelle (Excel), welche
  Fragen in welchem Bericht erscheinen.
- **Offene Antworten im Anhang:** gleiche Antworten werden
  zusammengefasst, Links führen von der Frage in den Anhang und zurück.
- **Einheitliches Erscheinungsbild:** Akzentfarbe, Spaltenbreiten und
  Voreinstellungen zentral über
  [`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md)
  einstellbar; die Schrift „Red Hat Text“ wird für die Grafiken
  mitgeliefert.

## Installation

``` r

# install.packages("pak")
pak::pak("donvollb/setanalysis")
```

Für die Berichte werden außerdem [Quarto](https://quarto.org) (≥ 1.7,
enthält Typst) und die Schrift [Red Hat
Text](https://fonts.google.com/specimen/Red+Hat+Text) benötigt.

## Schnellstart

Die Auswertungsfunktionen schreiben Markdown bzw. Typst in die Ausgabe.
Sie stehen deshalb in R-Chunks mit `output: asis`:

```` markdown
---
title: "Lehrveranstaltungsevaluation"
format: typst
---

```{r}
#| include: false
library(setanalysis)
daten <- BspDaten$dataLVE # eigene Daten: evasys_read_data("rohdaten.csv", "codebuch.csv")
```

```{r}
#| output: asis
merge_aggr_sk(daten[, c("KF_01", "KF_02", "KF_03")], kennung = daten$Kennung, aggr = TRUE)
merge_grade(daten$Note, daten$Kennung)
merge_sc(daten$V3_D)
```
````

In RStudio lässt sich eine einzelne Auswertung ohne Rendern als Vorschau
anzeigen: `markdown_in_viewer(merge_sc(BspDaten$dataLVE$V3_D))`.

## Typischer Ablauf

1.  **Daten einlesen** mit
    [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
    (oder eigene Daten mit den Attributen `label`, `nr`, `type` und
    `labels` versehen).
2.  **Berichte festlegen** (bei mehreren Berichten mit unterschiedlichem
    Inhalt): Berichtstabelle mit einer Zeile pro Bericht, Regeltabelle
    mit einer Bedingung pro Frage;
    [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md)
    berechnet daraus die Schalter `inkl.<Abschnitt>.<Frage>` und
    `header<Abschnitt>`.
3.  **Bericht schreiben** in Quarto: pro Frage eine
    Auswertungsfunktion, z. B. `merge_sc(x, nr = "1.3")`. Mit `nr` fragt
    die Funktion den Schalter `inkl.1.3` ab und erscheint nur in den
    passenden Berichten.
    [`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md)
    am Ende gibt die gesammelten offenen Antworten aus.
4.  **Alle Berichte erstellen**, z. B. mit `quarto::quarto_render()` in
    einer Schleife über die Zeilen der Berichtstabelle.

Eine ausführliche Anleitung mit allen Schritten steht in der Vignette
[Einen Evaluationsbericht
erstellen](https://donvollb.github.io/setanalysis/articles/bericht-erstellen.html).
Alle Hilfeseiten gibt es in der
[Funktionsreferenz](https://donvollb.github.io/setanalysis/reference/),
einen Überblick in R mit
[`?setanalysis`](https://donvollb.github.io/setanalysis/reference/setanalysis-package.md).

Eine fertige Berichtsvorlage mit Layout (Kopf- und Fußzeile,
Inhaltsverzeichnis, Akzentfarbe) und zwei lauffähigen Beispielen ist
[set-template](https://github.com/donvollb/set-template).

## Funktionsüberblick

| Bereich | Funktionen |
|----|----|
| Auswertung je Frage | [`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md), [`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md), [`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md), [`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md), [`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md), [`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md), [`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md), [`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md), [`merge_rueck()`](https://donvollb.github.io/setanalysis/reference/merge_rueck.md), [`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md), [`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md), [`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md) |
| Automatisch nach Fragetyp | [`merge_auto()`](https://donvollb.github.io/setanalysis/reference/merge_auto.md), [`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md) |
| Daten aufbereiten | [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md), [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md), [`aggr_data()`](https://donvollb.github.io/setanalysis/reference/aggr_data.md), [`label_test()`](https://donvollb.github.io/setanalysis/reference/label_test.md) |
| Tabellen | [`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md), [`table_freq()`](https://donvollb.github.io/setanalysis/reference/table_freq.md), [`table_stat_single()`](https://donvollb.github.io/setanalysis/reference/table_stat_single.md), [`table_stat_multi()`](https://donvollb.github.io/setanalysis/reference/table_stat_multi.md) |
| Grafiken | [`barplot_freq()`](https://donvollb.github.io/setanalysis/reference/barplot_freq.md), [`barplot_scmc()`](https://donvollb.github.io/setanalysis/reference/barplot_scmc.md), [`barplot_sk()`](https://donvollb.github.io/setanalysis/reference/barplot_sk.md), [`boxplot_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/boxplot_aggr_sk.md), [`boxplot_grade()`](https://donvollb.github.io/setanalysis/reference/boxplot_grade.md), [`boxplot_rueck()`](https://donvollb.github.io/setanalysis/reference/boxplot_rueck.md), [`boxplot_wl()`](https://donvollb.github.io/setanalysis/reference/boxplot_wl.md) |
| Legenden | [`bsp_table_stat()`](https://donvollb.github.io/setanalysis/reference/bsp_table_stat.md), [`bsp_boxplot()`](https://donvollb.github.io/setanalysis/reference/bsp_boxplot.md), [`bsp_evasys_sk6()`](https://donvollb.github.io/setanalysis/reference/bsp_evasys_sk6.md) |
| Werkzeuge | [`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md), [`subchunkify()`](https://donvollb.github.io/setanalysis/reference/subchunkify.md), [`markdown_in_viewer()`](https://donvollb.github.io/setanalysis/reference/markdown_in_viewer.md) |

Die Namen aus Version 1.0.0 (z. B.
[`merge.sc()`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md),
[`table.freq()`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md))
funktionieren weiterhin, siehe `?setanalysis-deprecated`.

## Beispieldaten

`BspDaten` enthält fiktive, zufällig erzeugte Daten einer
Lehrveranstaltungsevaluation und einer Studieneingangsbefragung im
Format von
[`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md).
Beispiel-Dateien für
[`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
und
[`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md)
liegen in `system.file("extdata", package = "setanalysis")`.

## Entwicklung

Das Verhalten der Funktionen ist mit Snapshot-Tests (testthat)
abgesichert, die die erzeugten Berichtsbausteine einschließlich der
Grafiken (SVG) vergleichen. Die Skripte, mit denen Beispieldaten und
Vorschaubilder erzeugt werden, liegen in `data-raw/`. Änderungen stehen
im [Changelog](https://donvollb.github.io/setanalysis/news/). Wie das
Paket weiterentwickelt wird (Branches, Tests, GitHub Actions, Releases),
beschreibt der Artikel [Das Paket
pflegen](https://donvollb.github.io/setanalysis/articles/paket-pflegen.html).

## Autoren und Lizenz

Dominik Vollbracht und Simon Männle · [MIT
License](https://donvollb.github.io/setanalysis/LICENSE.md)
