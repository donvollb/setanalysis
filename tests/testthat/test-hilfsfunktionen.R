test_that("aggr_data() aggregiert unverändert", {
  # Ausschnitt mit einigen wenigen LV-Kennungen; beim Zeilen-Subsetting gehen
  # die Labels verloren, daher werden sie wieder gesetzt
  lve <- BspDaten$dataLVE
  zeilen <- 1:40
  items <- lve[zeilen, c("KF_01", "KF_02")]
  for (n in names(items)) attr(items[[n]], "label") <- attr(lve[[n]], "label")
  note <- structure(lve$Note[zeilen], label = attr(lve$Note, "label"))

  expect_snapshot_value(aggr_data(items, lve$Kennung[zeilen]), style = "deparse")
  expect_snapshot_value(aggr_data(note, lve$Kennung[zeilen]), style = "deparse")
})

test_that("label_test() meldet Übereinstimmungen und Abweichungen", {
  expect_snapshot({
    label_test(BspDaten$pInfo$FB.txt, BspDaten$dataLVE$Teilbereich)
    label_test(BspDaten$pInfo$FB.txt.falsch, BspDaten$dataLVE$Teilbereich)
    label_test(BspDaten$pInfo$FB.txt.falsch, BspDaten$dataLVE$Teilbereich,
      exception = "Psüchologie - SoSe24"
    )
    label_test(c("ja", "nein"), BspDaten$Tabellen$freq)
  })
})

test_that("change_analysis_defaults() ändert Einstellungen", {
  reset_setanalysis_state()
  withr::defer(reset_setanalysis_state())

  change_analysis_defaults(color.bars = "red", show.plot.sc = FALSE)
  expect_identical(setanalysis_defaults$color.bars, "red")
  expect_false(setanalysis_defaults$show.plot.sc)

  expect_error(
    change_analysis_defaults(color.width2 = "turquoise"),
    "existiert nicht"
  )
})

test_that("Standardeinstellungen haben die erwarteten Werte", {
  reset_setanalysis_state()
  expect_snapshot(str(as.list(setanalysis_defaults)[sort(ls(setanalysis_defaults))]))
})

test_that("Einstellungen wirken sich auf die Ausgabe aus", {
  local({
    reset_setanalysis_state()
    withr::defer(reset_setanalysis_state())
    change_analysis_defaults(show.plot.sc = FALSE)
    ausgabe <- capture.output(merge_sc(BspDaten$dataLVE$V3_D))
    expect_false(any(grepl("<img", ausgabe)))
  })
})

test_that("markdown_in_viewer() übergibt eine HTML-Datei an den Viewer", {
  angezeigt <- NULL
  withr::local_options(viewer = function(url, ...) angezeigt <<- url)
  withr::local_dir(withr::local_tempdir())
  reset_setanalysis_state()

  markdown_in_viewer(merge_sc(BspDaten$dataLVE$V3_D))

  expect_true(file.exists(angezeigt))
  html <- paste(readLines(angezeigt, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "Red Hat Text", fixed = TRUE)
  expect_match(html, "Pflichtveranstaltungen", fixed = TRUE)
})

test_that("Veraltete Namen von Tabellen- und Hilfsfunktionen liefern dasselbe Ergebnis", {
  lve <- BspDaten$dataLVE
  typst <- function(tabelle) tinytable::save_tt(tabelle, output = "typst")
  beispiel <- function(datei) system.file("extdata", datei, package = "setanalysis")

  expect_identical(
    typst(table.freq(BspDaten$Tabellen$freq, col1.name = "x")),
    typst(table_freq(BspDaten$Tabellen$freq, col1.name = "x"))
  )
  expect_identical(
    typst(table.stat.single(lve$KF_01, TRUE)),
    typst(table_stat_single(lve$KF_01, TRUE))
  )
  expect_identical(
    typst(table.stat.multi(BspDaten$Tabellen$multi)),
    typst(table_stat_multi(BspDaten$Tabellen$multi))
  )
  expect_identical(typst(bsp.table.stat(FALSE)), typst(bsp_table_stat(FALSE)))
  expect_identical(
    capture.output(label.test(BspDaten$pInfo$FB.txt.falsch, lve$Teilbereich)),
    capture.output(label_test(BspDaten$pInfo$FB.txt.falsch, lve$Teilbereich))
  )
  expect_identical(
    input.tabelle(beispiel("beispiel_berichte.xlsx"), beispiel("beispiel_regeln.xlsx")),
    input_tabelle(beispiel("beispiel_berichte.xlsx"), beispiel("beispiel_regeln.xlsx"))
  )
  expect_identical(list.open.answers, list_open_answers)

  reset_setanalysis_state()
  withr::defer(reset_setanalysis_state())
  change.analysis.defaults(color.bars = "green")
  expect_identical(setanalysis_defaults$color.bars, "green")
})

test_that("Veraltete Namen erzeugen dieselbe Ausgabe wie die aktuellen Funktionen", {
  lve <- BspDaten$dataLVE
  showup <- BspDaten$dataSHOWUP

  # Ausgabe in einem frischen Zustand und Temp-Ordner erzeugen
  ausgabe <- function(code) {
    reset_setanalysis_state()
    withr::local_dir(withr::local_tempdir())
    capture.output(code)
  }
  expect_gleiche_ausgabe <- function(alt, neu) {
    expect_identical(ausgabe(alt), ausgabe(neu))
  }

  expect_gleiche_ausgabe(merge.sc(lve$V3_D), merge_sc(lve$V3_D))
  # Argumente ohne Namen werden wie beim direkten Aufruf zugeordnet
  expect_gleiche_ausgabe(
    merge.sc(lve$V3_D, TRUE, "", 3),
    merge_sc(lve$V3_D, TRUE, "", 3)
  )
  expect_gleiche_ausgabe(merge.sc(lve$V3_D, FALSE), merge_sc(lve$V3_D, FALSE))
  expect_gleiche_ausgabe(
    merge.mc(showup[, paste0("abschluss_", 1:8)], "Abschluss"),
    merge_mc(showup[, paste0("abschluss_", 1:8)], "Abschluss")
  )
  expect_gleiche_ausgabe(
    merge.evasys.sk(lve$KF_01, show.plot = FALSE),
    merge_sk(lve$KF_01, show.plot = FALSE)
  )
  expect_gleiche_ausgabe(
    merge.multi.sk(lve[, c("KF_01", "KF_02")], lve$Kennung, 6),
    merge_aggr_sk(lve[, c("KF_01", "KF_02")], lve$Kennung, 6)
  )
  expect_gleiche_ausgabe(merge.num(showup$zugang_note), merge_num(showup$zugang_note))
  expect_gleiche_ausgabe(
    merge.fachsem(lve$FachSemN, 5, 10),
    merge_fachsem(lve$FachSemN, 5, 10)
  )
  expect_gleiche_ausgabe(
    merge.open(showup$offen, appendix = FALSE),
    merge_open(showup$offen, appendix = FALSE)
  )
  expect_gleiche_ausgabe(
    merge.subj(showup$fach1_2FB, showup$fach2_2FB),
    merge_subj(showup$fach1_2FB, showup$fach2_2FB)
  )
  expect_gleiche_ausgabe(merge.wl(lve$WL, lve$Kennung), merge_wl(lve$WL, lve$Kennung))
  expect_gleiche_ausgabe(
    boxplot.ruecklauf(lve$Teilnehmer, lve$Kennung),
    merge_rueck(lve$Teilnehmer, lve$Kennung)
  )
  expect_gleiche_ausgabe(grade(lve$Note, lve$Kennung), merge_grade(lve$Note, lve$Kennung))
  expect_gleiche_ausgabe(bsp.boxplot(), bsp_boxplot())
  expect_gleiche_ausgabe(bsp.evasys.sk6(), bsp_evasys_sk6())
  expect_gleiche_ausgabe(
    {
      merge_open(showup$offen, appendix = TRUE, inkl = TRUE)
      appendix.open()
    },
    {
      merge_open(showup$offen, appendix = TRUE, inkl = TRUE)
      appendix_open()
    }
  )

  # Aufruf aus einer anderen Funktion heraus über `...`
  umschlag <- function(...) merge.sc(...)
  expect_gleiche_ausgabe(
    umschlag(lve$V3_D, show.plot = FALSE),
    merge_sc(lve$V3_D, show.plot = FALSE)
  )

  # Aufruf über lapply()
  expect_gleiche_ausgabe(
    lapply(list(lve$V3_D), merge.sc, show.plot = FALSE),
    lapply(list(lve$V3_D), merge_sc, show.plot = FALSE)
  )
})

test_that("Alle veralteten Namen werden exportiert", {
  namespace <- readLines(system.file("NAMESPACE", package = "setanalysis"))
  alt <- c(
    "appendix.open", "boxplot.ruecklauf", "bsp.boxplot", "bsp.evasys.sk6",
    "bsp.table.stat", "change.analysis.defaults", "evasys.read.data",
    "grade", "input.tabelle", "label.test", "list.open.answers",
    "markdown.in.viewer", "merge.evasys.sk", "merge.fachsem", "merge.mc",
    "merge.multi.sk", "merge.num", "merge.open", "merge.sc", "merge.subj",
    "merge.wl", "open.answers", "table.freq", "table.stat.multi",
    "table.stat.single", ".costum_boxplot"
  )
  expect_true(all(paste0("export(", alt, ")") %in% namespace))
  expect_false(any(grepl("^S3method", namespace)))
})

test_that("open.answers() erzeugt den Verweis auf den Anhang", {
  local_inkl(`1.34` = TRUE)
  expect_report_snapshot(
    open.answers(BspDaten$dataSHOWUP$offen, nr = "1.34"),
    "open.answers"
  )
})
