# Package index

## Überblick

- [`setanalysis`](https://donvollb.github.io/setanalysis/reference/setanalysis-package.md)
  [`setanalysis-package`](https://donvollb.github.io/setanalysis/reference/setanalysis-package.md)
  : setanalysis: Personalisierte Berichte für Lehrevaluationen

## Auswertung je Frage

Erzeugen Überschrift, Tabelle und Abbildung für eine Frage. Aufruf in
Quarto-Chunks mit `output: asis`.

- [`merge_sc()`](https://donvollb.github.io/setanalysis/reference/merge_sc.md)
  : Single-Choice-Frage auswerten
- [`merge_mc()`](https://donvollb.github.io/setanalysis/reference/merge_mc.md)
  : Multiple-Choice-Frage auswerten
- [`merge_sk()`](https://donvollb.github.io/setanalysis/reference/merge_sk.md)
  : Skalenfrage auswerten (Einzelantworten)
- [`merge_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/merge_aggr_sk.md)
  : Skalenfragen gemeinsam auswerten, optional pro Lehrveranstaltung
  aggregiert
- [`merge_num()`](https://donvollb.github.io/setanalysis/reference/merge_num.md)
  : Numerische Frage auswerten
- [`merge_fachsem()`](https://donvollb.github.io/setanalysis/reference/merge_fachsem.md)
  : Fachsemester auswerten
- [`merge_subj()`](https://donvollb.github.io/setanalysis/reference/merge_subj.md)
  : Erstes und zweites Fach gemeinsam auswerten
- [`merge_grade()`](https://donvollb.github.io/setanalysis/reference/merge_grade.md)
  : Gesamtnote auswerten
- [`merge_wl()`](https://donvollb.github.io/setanalysis/reference/merge_wl.md)
  : Workload der Lehrveranstaltungen auswerten
- [`merge_rueck()`](https://donvollb.github.io/setanalysis/reference/merge_rueck.md)
  : Rücklauf der Lehrveranstaltungen auswerten
- [`merge_open()`](https://donvollb.github.io/setanalysis/reference/merge_open.md)
  : Offene Frage auswerten
- [`appendix_open()`](https://donvollb.github.io/setanalysis/reference/appendix_open.md)
  : Anhang mit den offenen Antworten ausgeben
- [`merge_auto()`](https://donvollb.github.io/setanalysis/reference/merge_auto.md)
  : Frage mit passender Funktion auswerten (Typ wird erkannt)
- [`merge_many()`](https://donvollb.github.io/setanalysis/reference/merge_many.md)
  : Mehrere Fragen auf einmal auswerten

## Daten aufbereiten

- [`evasys_read_data()`](https://donvollb.github.io/setanalysis/reference/evasys_read_data.md)
  : Rohdaten und Codebuch aus evasys einlesen
- [`input_tabelle()`](https://donvollb.github.io/setanalysis/reference/input_tabelle.md)
  : Festlegen, welche Fragen in welchen Bericht kommen
- [`aggr_data()`](https://donvollb.github.io/setanalysis/reference/aggr_data.md)
  : Antworten je Gruppe mitteln
- [`label_test()`](https://donvollb.github.io/setanalysis/reference/label_test.md)
  : Schreibweisen in Berichtstabelle und Daten abgleichen

## Tabellen

- [`lv_table()`](https://donvollb.github.io/setanalysis/reference/lv_table.md)
  : Tabelle im Stil der Berichte formatieren
- [`table_freq()`](https://donvollb.github.io/setanalysis/reference/table_freq.md)
  : Häufigkeitstabelle erstellen
- [`table_stat_single()`](https://donvollb.github.io/setanalysis/reference/table_stat_single.md)
  : Kennwerte einer Frage als Tabelle
- [`table_stat_multi()`](https://donvollb.github.io/setanalysis/reference/table_stat_multi.md)
  : Kennwerte mehrerer Items als Tabelle

## Grafiken

- [`barplot_freq()`](https://donvollb.github.io/setanalysis/reference/barplot_freq.md)
  : Balkendiagramm der Häufigkeiten
- [`barplot_scmc()`](https://donvollb.github.io/setanalysis/reference/barplot_scmc.md)
  : Waagerechtes Balkendiagramm für Single- und Multiple-Choice-Fragen
- [`barplot_sk()`](https://donvollb.github.io/setanalysis/reference/barplot_sk.md)
  : Verteilung einer Skalenfrage mit Mittelwert und Standardabweichung
- [`boxplot_aggr_sk()`](https://donvollb.github.io/setanalysis/reference/boxplot_aggr_sk.md)
  : Boxplots für mehrere Skalenfragen
- [`boxplot_grade()`](https://donvollb.github.io/setanalysis/reference/boxplot_grade.md)
  : Boxplot der Gesamtnote
- [`boxplot_rueck()`](https://donvollb.github.io/setanalysis/reference/boxplot_rueck.md)
  : Boxplot des Rücklaufs
- [`boxplot_wl()`](https://donvollb.github.io/setanalysis/reference/boxplot_wl.md)
  : Boxplot des Workloads

## Legenden

Erläuterungen zu Kennwerten und Grafiken für den Anfang oder das Ende
eines Berichts.

- [`bsp_table_stat()`](https://donvollb.github.io/setanalysis/reference/bsp_table_stat.md)
  : Legende: Erklärung der Tabellenspalten
- [`bsp_boxplot()`](https://donvollb.github.io/setanalysis/reference/bsp_boxplot.md)
  : Legende: beschrifteter Beispiel-Boxplot
- [`bsp_evasys_sk6()`](https://donvollb.github.io/setanalysis/reference/bsp_evasys_sk6.md)
  : Legende: beschriftete Beispiel-Abbildung einer 6er-Skala

## Einstellungen und Werkzeuge

- [`change_analysis_defaults()`](https://donvollb.github.io/setanalysis/reference/change_analysis_defaults.md)
  : Einstellungen des Pakets ändern
- [`setanalysis_defaults`](https://donvollb.github.io/setanalysis/reference/setanalysis_defaults.md)
  : Einstellungen des Pakets
- [`list_open_answers`](https://donvollb.github.io/setanalysis/reference/list_open_answers.md)
  : Speicher für die offenen Antworten des Anhangs
- [`subchunkify()`](https://donvollb.github.io/setanalysis/reference/subchunkify.md)
  : Abbildung oder Tabelle als eigenen Chunk ausgeben
- [`markdown_in_viewer()`](https://donvollb.github.io/setanalysis/reference/markdown_in_viewer.md)
  : Vorschau eines Berichtsabschnitts im Viewer

## Beispieldaten

- [`BspDaten`](https://donvollb.github.io/setanalysis/reference/BspDaten.md)
  : Beispieldaten

## Frühere Funktionsnamen

- [`setanalysis-deprecated`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`appendix.open`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`bsp.boxplot`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`bsp.evasys.sk6`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`bsp.table.stat`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`change.analysis.defaults`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`evasys.read.data`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`grade`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`input.tabelle`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`label.test`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`markdown.in.viewer`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`open.answers`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`table.freq`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`table.stat.multi`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`table.stat.single`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`boxplot.ruecklauf`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`merge.evasys.sk`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`merge.fachsem`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`merge.mc`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`merge.multi.sk`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`merge.num`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`merge.open`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`merge.sc`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`merge.subj`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`merge.wl`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  [`list.open.answers`](https://donvollb.github.io/setanalysis/reference/setanalysis-deprecated.md)
  : Veraltete Funktionsnamen
