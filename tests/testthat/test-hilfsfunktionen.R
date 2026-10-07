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

test_that("label.test() meldet Übereinstimmungen und Abweichungen", {
  expect_snapshot({
    label.test(BspDaten$pInfo$FB.txt, BspDaten$dataLVE$Teilbereich)
    label.test(BspDaten$pInfo$FB.txt.falsch, BspDaten$dataLVE$Teilbereich)
    label.test(BspDaten$pInfo$FB.txt.falsch, BspDaten$dataLVE$Teilbereich,
               exception = "Psüchologie - SoSe24")
    label.test(c("ja", "nein"), BspDaten$Tabellen$freq)
  })
})

test_that("get.label() zieht das Label der Antwortoption", {
  expect_identical(get.label(BspDaten$dataSHOWUP$abschluss_1),
                   "Bachelor of Arts (B.A.)")
  expect_identical(get.label(BspDaten$dataSHOWUP$abschluss_1, match = "\\("),
                   "B.A.)")
})

test_that("change.analysis.defaults() ändert Einstellungen", {
  reset_setanalysis_state()
  withr::defer(reset_setanalysis_state())

  change.analysis.defaults(color.bars = "red", show.plot.sc = FALSE)
  expect_identical(setanalysis_defaults$color.bars, "red")
  expect_false(setanalysis_defaults$show.plot.sc)

  expect_error(change.analysis.defaults(color.width2 = "turquoise"),
               "existiert nicht")
})

test_that("Standardeinstellungen haben die erwarteten Werte", {
  reset_setanalysis_state()
  expect_snapshot(str(as.list(setanalysis_defaults)[sort(ls(setanalysis_defaults))]))
})

test_that("Einstellungen wirken sich auf die Ausgabe aus", {
  local({
    reset_setanalysis_state()
    withr::defer(reset_setanalysis_state())
    change.analysis.defaults(show.plot.sc = FALSE)
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

test_that("Veraltete Funktionsnamen verweisen auf die aktuellen Funktionen", {
  expect_identical(appendix.open, appendix_open)
  expect_identical(boxplot.ruecklauf, merge_rueck)
  expect_identical(grade, merge_grade)
  expect_identical(markdown.in.viewer, markdown_in_viewer)
  expect_identical(merge.evasys.sk, merge_sk)
  expect_identical(merge.fachsem, merge_fachsem)
  expect_identical(merge.mc, merge_mc)
  expect_identical(merge.multi.sk, merge_aggr_sk)
  expect_identical(merge.num, merge_num)
  expect_identical(merge.open, merge_open)
  expect_identical(merge.sc, merge_sc)
  expect_identical(merge.subj, merge_subj)
  expect_identical(merge.wl, merge_wl)
})

test_that("open.answers() erzeugt den Verweis auf den Anhang", {
  local_inkl(`1.34` = TRUE)
  expect_report_snapshot(open.answers(BspDaten$dataSHOWUP$offen, nr = "1.34"),
                         "open.answers")
})
