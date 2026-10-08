# setanalysis

[Deutsch](https://github.com/donvollb/setanalysis#readme) · **English**
· [Website](https://donvollb.github.io/setanalysis/) (German)

[![R-CMD-check](https://github.com/donvollb/setanalysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/donvollb/setanalysis/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/license-MIT-yellow.svg)](https://donvollb.github.io/setanalysis/LICENSE.md)

R package for personalised survey reports, in particular course
evaluations and surveys of students and graduates. One line of code per
question produces the heading, table and chart for a
[Quarto](https://quarto.org) report (PDF via Typst). A report table and
a rules table turn a single template into many reports with different
content, e.g. one per department or degree programme.

The package, its documentation and the generated reports are in German.

![Rating scale items: table of statistics and box plots per item,
followed by the overall
grade](reference/figures/vorschau-1.png)![Multiple-choice question with
table and bar chart, followed by a rating scale item with its
distribution](reference/figures/vorschau-2.png)

## Features

- **Import from evasys:**
  [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
  reads raw data and codebook and attaches question text, question
  number, question type and value labels to every variable.
- **One function per question type:** single choice, multiple choice,
  rating scales, groups of rating scale items, numbers, semester of
  study, grades, workload, response rate and open questions – each with
  consistently styled tables and charts.
  [`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md)
  analyses whole blocks of questions at once.
- **Many reports from one template:**
  [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md)
  uses a report table and a rules table (Excel) to decide which
  questions appear in which report.
- **Open answers in an appendix:** identical answers are combined, links
  lead from the question to the appendix and back.
- **Consistent look:** accent colour, column widths and defaults are set
  in one place with
  [`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md);
  the font “Red Hat Text” is bundled for the charts.

## Installation

``` r

# install.packages("pak")
pak::pak("donvollb/setanalysis")
```

The reports also require [Quarto](https://quarto.org) (≥ 1.7, includes
Typst) and the font [Red Hat
Text](https://fonts.google.com/specimen/Red+Hat+Text).

## Quick start

The analysis functions write Markdown or Typst to the output, so they
are called in R chunks with `output: asis`:

```` markdown
---
title: "Lehrveranstaltungsevaluation"
format: typst
---

```{r}
#| include: false
library(setanalysis)
daten <- BspDaten$dataLVE # own data: evasys_read_data("rohdaten.csv", "codebuch.csv")
```

```{r}
#| output: asis
merge_aggr_sk(daten[, c("KF_01", "KF_02", "KF_03")], kennung = daten$Kennung, aggr = TRUE)
merge_grade(daten$Note, daten$Kennung)
merge_sc(daten$V3_D)
```
````

In RStudio, a single analysis can be previewed without rendering:
`markdown_in_viewer(merge_sc(BspDaten$dataLVE$V3_D))`.

## Typical workflow

1.  **Read the data** with
    [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
    (or add the attributes `label`, `nr`, `type` and `labels` to your
    own data).
2.  **Define the reports** (for several reports with different content):
    a report table with one row per report and a rules table with one
    condition per question;
    [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md)
    computes the switches `inkl.<section>.<question>` and
    `header<section>`.
3.  **Write the report** in Quarto: one analysis function per question,
    e.g. `merge_sc(x, nr = "1.3")`. With `nr`, the function checks the
    switch `inkl.1.3` and only appears in the matching reports.
    [`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md)
    at the end prints the collected open answers.
4.  **Render all reports**, e.g. with `quarto::quarto_render()` in a
    loop over the rows of the report table.

The vignette [Einen Evaluationsbericht
erstellen](https://donvollb.github.io/setanalysis/articles/bericht-erstellen.html)
walks through all steps (in German). All help pages are in the [function
reference](https://donvollb.github.io/setanalysis/reference/); in R,
[`?setanalysis`](https://donvollb.github.io/setanalysis/reference/setanalysis-package.md)
gives an overview.

A complete report template with layout (header and footer, table of
contents, accent colour) and two working examples is
[set-template](https://github.com/donvollb/set-template).

## Function overview

| Area | Functions |
|----|----|
| Analysis per question | [`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md), [`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md), [`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md), [`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md), [`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md), [`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md), [`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md), [`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md), [`merge_rueck()`](https://donvollb.github.io/setanalysis/reference/merge_rueck.md), [`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md), [`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md), [`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md) |
| Automatic by question type | [`merge_auto()`](https://donvollb.github.io/setanalysis/reference/merge_auto.md), [`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md) |
| Data preparation | [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md), [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md), [`aggr_data()`](https://donvollb.github.io/setanalysis/reference/aggr_data.md), [`label_test()`](https://donvollb.github.io/setanalysis/reference/label_test.md) |
| Tables | [`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md), [`table_freq()`](https://donvollb.github.io/setanalysis/reference/table_freq.md), [`table_stat_single()`](https://donvollb.github.io/setanalysis/reference/table_stat_single.md), [`table_stat_multi()`](https://donvollb.github.io/setanalysis/reference/table_stat_multi.md) |
| Charts | [`barplot_freq()`](https://donvollb.github.io/setanalysis/reference/barplot_freq.md), [`barplot_scmc()`](https://donvollb.github.io/setanalysis/reference/barplot_scmc.md), [`barplot_sk()`](https://donvollb.github.io/setanalysis/reference/barplot_sk.md), [`boxplot_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/boxplot_aggr_sk.md), [`boxplot_grade()`](https://donvollb.github.io/setanalysis/reference/boxplot_grade.md), [`boxplot_rueck()`](https://donvollb.github.io/setanalysis/reference/boxplot_rueck.md), [`boxplot_wl()`](https://donvollb.github.io/setanalysis/reference/boxplot_wl.md) |
| Legends | [`bsp_table_stat()`](https://donvollb.github.io/setanalysis/reference/bsp_table_stat.md), [`bsp_boxplot()`](https://donvollb.github.io/setanalysis/reference/bsp_boxplot.md), [`bsp_evasys_sk6()`](https://donvollb.github.io/setanalysis/reference/bsp_evasys_sk6.md) |
| Tools | [`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md), [`subchunkify()`](https://donvollb.github.io/setanalysis/reference/subchunkify.md), [`markdown_in_viewer()`](https://donvollb.github.io/setanalysis/reference/markdown_in_viewer.md) |

The names from version 1.0.0
(e.g. [`merge.sc()`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md),
[`table.freq()`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md))
still work, see `?setanalysis-deprecated`.

## Example data

`BspDaten` contains fictional, randomly generated data from a course
evaluation and a first-year student survey in the format of
[`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md).
Example files for
[`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
and
[`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md)
are in `system.file("extdata", package = "setanalysis")`.

## Development

The behaviour of all functions is covered by snapshot tests (testthat)
that compare the generated report building blocks, including the charts
(SVG). The scripts that generate the example data and preview images are
in `data-raw/`. Changes are listed in the
[changelog](https://donvollb.github.io/setanalysis/news/) (German).

## Authors and licence

Dominik Vollbracht and Simon Männle · [MIT
License](https://donvollb.github.io/setanalysis/LICENSE.md)
